library(data.table)
library(stringi)
library(ranger)
library(pROC)
library(ggplot2)
library(ggdist)
library(iml)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)

survey <- readRDS(file.survey.proc)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)


# fwrite(variables, file.variables.proc.csv)

## SELECT OBSERVATIONS AND VARIABLES ##########################################

# For global model: Identify questions that ask for additional information given
# specific responses to other questions

var.omit <-
  c(
    "A39", "A43", "A51",
    # … see conditioning on respondents below.
    variables[main == "infoSources", code],
    # … asked depending on who takes part in decision making.
    variables[main == "plantSelecReasons", code],
    # … only asked if selection criteria changed.
    variables[main == "pastChangeChemicals", code],
    # … specific for respondants not from Québec.
    variables[main == "pastChangeReasons", code],
    # … only asked if changed management in the past.
    variables[main == "futureOpportunWhy", code],
    # … only asked if CC is seen as an opportunity.
    variables[main == "mgmtChangeACCWhyNo", code],
    # … only asked if no intention to implement any measures.
    variables[main == "ACCIntended", code],
    # … only asked if any intention to implement measures.
    variables[main == "ACCEfficiency", code],
    # … only asked if any intention to implement measures.
    variables[main == "CCExperience", code],
    # … only asked if events linked to CC have been observed.
    variables[main == "CCWhen", code],
    # … only asked if future impacts are expected.
    variables[main == "CCSurface", code],
    # … only asked if future impacts are expected.
    variables[main == "professionalForester", code]
    # … asked depending on education level.
  ) 

variables.full <- variables[!(code %in% var.omit)]

# Of these variables, select only variables that have a category assigned

var.sel <- 
  variables.full[!(is.na(category.personal_stakes) |
                   is.na(category.threat_appraisal) |
                   is.na(category.coping_appraisal) |
                   is.na(category.control) |
                   is.na(category.adaptation))]


# Subset responses to survey
survey.sub <- survey[
                     A39 == "No" &
                     # … only respondants with managed forests.
                     A43 == "No" &
                     # … only respondants that take part in decision making.
                     A51 == "Yes"
                     # … only respondants that take part in decision making in the future.
                     ]

# Exclude variables that do not vary between respondents

var.include <-
  names(survey.sub)[survey.sub[, which(survey.sub[, lapply(.SD, \(x) length(unique(x)))] > 1)]]

var.sel <- var.sel[code %in% var.include]

survey.sel <-
  survey.sub[, var.sel$code, with = FALSE]
  # survey[survey.sub, var.sel$code, with = FALSE][, lapply(.SD, \(x) sum(is.na(x)))]


## RESPONSE VARIABLES ##########################################################

# Select response variables
sel.adaptation <- var.sel[main == "mgmtChangeACC", code]


y.acc <- matrix(0, nrow = nrow(survey.sel), ncol = length(sel.adaptation))
colnames(y.acc) <- paste0("y.acc.", 1:ncol(y.acc))
for(i in seq_along(sel.adaptation)) {
  y.acc[,i] <-
    # as.logical(survey.sel[[sel.adaptation[i]]] %in%
    #            c("Yes, within the next 5 years",
    #              "Yes, in 6 to 10 years"))
    as.numeric(survey.sel[[sel.adaptation[i]]] %in%
               c("Yes, within the next 5 years",
                 "Yes, in 6 to 10 years"))
}

y.acc.comb <- rowSums(y.acc)

vars.y.acc <- c(paste0("y.acc.", 1:length(sel.adaptation)), "y.acc.comb")

## PREPARE DATA FOR RANDOM FOREST #####################################

# Combine survey data and calculated response variables
survey.fit <- 
  cbind(y.acc.comb,
        y.acc,
        # survey.sel[, ..mod.preds])
        survey.sel)

# Remove ordering from factor variables
var.ord <- names(which(unlist(lapply(survey.fit, is.ordered))))
survey.fit[, (var.ord) := lapply(.SD,
                                 \(x) factor(x,
                                             ordered = FALSE,
                                             levels = levels(x))),
           .SDcols = var.ord]



saveRDS(survey.fit, file.survey.rf)


## RANDOM FOREST #######################################################


mod.preds <- var.sel[! code %in% sel.adaptation, code]

n.obs <- nrow(survey.fit)
n.var <- length(mod.preds)

num.trees.seq <- c(100, 1000, 10000)
mtry.seq <-
  floor(seq(sqrt(n.var)/2, n.var, length.out = 5))
min.node.size.seq <-
  floor(seq(2, sqrt(n.obs), length.out = 5))

tune.grid.bin <-
  expand.grid(resp = colnames(y.acc),
              type = "probability",
              num.trees = num.trees.seq,
              mtry = mtry.seq,
              min.node.size = min.node.size.seq,
              splitrule = c("gini", "hellinger")) |>
  as.data.table()
tune.grid.bin[, `:=`(score = NA, auc = NA)]

tune.grid.count <-
  expand.grid(resp = "y.acc.comb",
              type = "regression",
              num.trees = num.trees.seq,
              mtry = mtry.seq,
              min.node.size = min.node.size.seq,
              splitrule = "variance") |>
  as.data.table()
tune.grid.count[, `:=`(score = NA, r2 = NA)]

tune.grid <- rbind(tune.grid.bin, tune.grid.count, fill = TRUE)
set.seed(1234)
tune.grid[, seed := sample(1:1e9, .N)]


## The tuning will take roughly an hour (on 4 cores)

set.seed(1234)
pg <- txtProgressBar(min = 0,
                     max = nrow(tune.grid),
                     style = 3)
for(i in 1:nrow(tune.grid)) {
# for(i in sample(1:nrow(tune.grid), 10)) {
    mod.resp <- tune.grid$resp[i]
    form <- 
      as.formula(paste(mod.resp, "~",
                       paste(mod.preds, collapse = "+")))
  if(tune.grid$type[i] == "probability") {
    rf <-
      ranger(form,
             data = as.data.frame(survey.fit),
             num.trees = tune.grid$num.trees[i],
             mtry = tune.grid$mtry[i],
             min.node.size = tune.grid$min.node.size[i],
             splitrule = tune.grid$splitrule[i],
             respect.unordered.factors = "order",
             probability = TRUE,
             scale.permutation.importance = TRUE,
             importance = "permutation",
             seed = tune.grid$seed[i],
             num.threads = 4)
    tune.grid$score[i] <- rf$prediction.error
    tune.grid$auc[i] <-
      roc(survey.fit[[as.character(mod.resp)]], rf$predictions[,1])$auc |>
      as.numeric() |>
      suppressMessages()
  }
  if(tune.grid$type[i] == "regression") {
    rf <-
      ranger(form,
             data = as.data.frame(survey.fit),
             num.trees = tune.grid$num.trees[i],
             mtry = tune.grid$mtry[i],
             min.node.size = tune.grid$min.node.size[i],
             splitrule = tune.grid$splitrule[i],
             respect.unordered.factors = "order",
             scale.permutation.importance = TRUE,
             importance = "permutation",
             seed = tune.grid$seed[i],
             num.threads = 4)
    tune.grid$score[i] <- rf$prediction.error
    tune.grid$r2[i] <- rf$r.squared
  }
  setTxtProgressBar(pg, i)
}
close(pg)

fwrite(tune.grid, file.rf.parameters)
# tune.grid <- fread(file.rf.parameters)

tune.grid[, resp := factor(resp, levels = vars.y.acc)]


rf.par <- tune.grid[, .SD[which.min(score)], by = resp]

rf.mod <- list()
for(i in 1:nrow(rf.par)) {
    mod.resp <- rf.par$resp[i]
    form <- 
      as.formula(paste(mod.resp, "~",
                       paste(mod.preds, collapse = "+")))
  if(rf.par$type[i] == "probability") {
    rf.mod[[i]] <-
      ranger(form,
             data = as.data.frame(survey.fit),
             num.trees = rf.par$num.trees[i],
             mtry = rf.par$mtry[i],
             min.node.size = rf.par$min.node.size[i],
             splitrule = rf.par$splitrule[i],
             respect.unordered.factors = "order",
             probability = TRUE,
             scale.permutation.importance = TRUE,
             importance = "permutation",
             seed = rf.par$seed[i],
             num.threads = 4)
  }
  if(rf.par$type[i] == "regression") {
    rf.mod[[i]] <-
      ranger(form,
             data = as.data.frame(survey.fit),
             num.trees = rf.par$num.trees[i],
             mtry = rf.par$mtry[i],
             min.node.size = rf.par$min.node.size[i],
             splitrule = rf.par$splitrule[i],
             respect.unordered.factors = "order",
             scale.permutation.importance = TRUE,
             importance = "permutation",
             seed = rf.par$seed[i],
             num.threads = 4)
  }
}

names(rf.mod) <- rf.par$resp

saveRDS(rf.mod, file.rf.mod)
# rf.mod <- readRDS(file.rf.mod)


## VARIABLE IMPORTANCE #################################################

resp.code <- c(sel.adaptation, "Count")
resp.code <- factor(resp.code, levels = resp.code)
names(resp.code) <- levels(rf.par$resp)

n.perm <- 100
imp.l <- list()
pg <- txtProgressBar(min = 0,
                     max = length(rf.mod),
                     style = 3)
for(i in seq_along(rf.mod)) {
  mod.resp <- rf.par$resp[i]
  form <- 
    as.formula(paste(mod.resp, "~",
                     paste(mod.preds, collapse = "+")))
  if(rf.par$type[i] == "probability") {
  imp.l[[i]] <-
    importance_pvalues(rf.mod[[i]],
                       num.permutations = n.perm,
                       method = "altmann",
                       formula = form,
                       data = as.data.frame(survey.fit),
                       splitrule = rf.par$splitrule[i],
                       respect.unordered.factors = "order",
                       probability = TRUE,
                       scale.permutation.importance = TRUE,
                       seed = rf.par$seed[i],
                       num.threads = 4) |>
    as.data.table(keep.rownames = "code")
  }
  if(rf.par$type[i] == "regression") {
  imp.l[[i]] <-
      importance_pvalues(rf.mod[[i]],
                         num.permutations = n.perm,
                         method = "altmann",
                         formula = form,
                         data = as.data.frame(survey.fit),
                         splitrule = rf.par$splitrule[i],
                         respect.unordered.factors = "order",
                         scale.permutation.importance = TRUE,
                         seed = rf.par$seed[i],
                         num.threads = 4) |>
      as.data.table(keep.rownames = "code")
  }
  setTxtProgressBar(pg, i)
}
close(pg)

names(imp.l) <- names(rf.mod)
rf.imp <- rbindlist(imp.l, idcol = "resp")
rf.imp[, resp := factor(resp, levels = vars.y.acc)]

var.sel.l <-
  melt(var.sel,
       measure.vars =
         measure(category, pattern = "category.(.*)"),
      value.name = "cat") |>
  DT(cat != 0,
     # .(code, main, sub,
     .(code,
       category = factor(category,
                         levels = unique(category))))

rf.imp.var <-
  merge(rf.imp, var.sel.l,
  # merge(rf.imp[pvalue < 0.05], var.sel.l,
        all.x = TRUE, all.y = FALSE, by = "code")
        # all.x = TRUE, all.y = FALSE, by = "code")

rf.imp.var[, resp := resp.code[as.character(resp)]]
setnames(rf.imp.var, "code", "expl")
setcolorder(rf.imp.var, c("resp", "expl", "category", "importance"))
setorder(rf.imp.var, resp, -importance)

fwrite(rf.imp.var, file.rf.varimp)
# rf.imp.var <- fread(file.rf.varimp)

rf.imp.cat <-
  rf.imp.var[pvalue < 0.05] |>
  DT(order(resp, category)
     , .(resp = resp.code[as.character(resp)],
         importance.min = min(importance),
         importance.median = median(importance),
         importance.mean = mean(importance),
         importance.max = max(importance),
         cat.n = length(importance)),
     by = c("resp", "category")) |>
  DT(, .(resp,
         category,
         importance.min,
         importance.median,
         importance.mean,
         importance.max,
         cat.n))

fwrite(rf.imp.cat, file.rf.catimp)
# rf.imp.cat <- fread(file.rf.catimp)


items <- vars.y.acc[1:10]
vars.pred <-
  rf.imp.var[pvalue < 0.05
             ][order(resp, -importance),
               .SD[1:3],
               by = resp
               ][, unique(expl)]


vars.irt <- c(items, vars.pred)

survey.irt <- survey.fit[, ..vars.irt]

var.cat <- var.sel[code %in% vars.pred & type == "categorical", .(code, cat.ref)]
for(i in 1:nrow(var.cat)) {
  survey.irt[[var.cat$code[i]]] <-
    relevel(survey.irt[[var.cat$code[i]]], var.cat$cat.ref[i])
}

var.cont <- var.sel[code %in% vars.pred & type == "continuous", .(code, cat.ref)]
# var.cont <- var.sel[type == "continuous", .(code, cont.mean, cont.sd)]
if(nrow(var.cont) > 0) {
  for(i in 1:nrow(var.cont)) {
    survey.irt[[var.cont$code[i]]] <-
      (survey.irt[[var.cont$code[i]]] - var.cont$cont.mean[i]) /
      var.cont$cont.sd[i]
  }
}

survey.irt[, id := 1:.N]


survey.irt <-
  melt(copy(survey.irt),
       measure.vars = items,
       variable.name = "item",
       value.name = "resp")

saveRDS(survey.irt, file.survey.irt)






