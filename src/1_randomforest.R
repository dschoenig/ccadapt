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

# Exclude variables that do not vary between respondents

var.include <-
  names(survey.sel)[survey.sel[, which(survey.sel[, lapply(.SD, \(x) length(unique(x)))] > 1)]]

var.sel <- var.sel[code %in% var.include]

# Subset responses to survey
survey.sub <-
  with(survey,
       which(
             A39 == "No" &
             # … only respondants with managed forests.
             A43 == "No" &
             # … only respondants that take part in decision making.
             A51 == "Yes"
             # … only respondants that take part in decision making in the future.
            ))

survey.sel <-
  survey[survey.sub, var.sel$code, with = FALSE]
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



# survey.fit[,
#            colnames(y.acc) :=
#              lapply(.SD,
#                     \(x) factor(ifelse(x == 0, "no", "yes"), levels = c("no", "yes"))),
#            .SDcols = colnames(y.acc)]



# mod.resp <- "y.acc.1"

# library(caret)
# library(pROC)


# p.split <- 0.8
# sam.train <- createDataPartition(survey.fit[[mod.resp]], p = p.split, list = FALSE)
# survey.train <- survey.fit[sam.train]
# survey.valid <- survey.fit[!sam.train]

# fitControl <- trainControl(## 10-fold CV
#                            method = "oob")

# # survey.rf <- train(form,
# #                    data = survey.train, 
# #                    method = "ranger", 
# #                    trControl = fitControl,
# #                    importance = "permutation",
# #                    num.trees = 1000)
# # survey.rf

# # survey.gbm <- train(form,
# #                    data = survey.train, 
# #                    method = "xgbTree", 
# #                    trControl = fitControl,
# #                    ## This last option is actually one
# #                    ## for gbm() that passes through
# #                    verbose = FALSE)
# # survey.gbm

# # rf.grid <- 
# #   expand.grid(splitrule = c("variance", "extratrees", "maxstat", "beta"),
# #               min.node.size = seq(2, 10, 2),
# #               mtry = 2^(seq(1, ceiling(log2(100)))))

# mtry.max <- dim(model.matrix(form, data = survey.train))[2] - 1

# rf.grid <- 
#   expand.grid(splitrule = c("gini", "extratrees", "hellinger"),
#               min.node.size = seq(1, 10, 1),
#               mtry = floor(2^seq(1, log2(mtry.max), length.out = 10)))

# rf.grid <- 
#   expand.grid(splitrule = c("variance", "extratrees", "maxstat", "beta"),
#               min.node.size = seq(1, 10, 1),
#               mtry = floor(2^seq(1, log2(mtry.max), length.out = 10)))

# # rf.grid <- 
# #   expand.grid(splitrule = c("gini"),
# #               min.node.size = c(4, 6),
# #               mtry = floor(2^seq(1, log2(mtry.max), length.out = 10)))


# survey.rf <- train(form,
#                    data = survey.train, 
#                    method = "ranger", 
#                    trControl = trainControl(method = "oob"),
#                    importance = "permutation",
#                    num.trees = 1000,
#                    metric = "Kappa",
#                    tuneGrid = rf.grid,
#                    num.threads = 4)
# survey.rf

# sort(round(importance(rf), 5), decreasing = TRUE)

# var.sel[code == "B38"]


# survey.gbm <- train(form,
#                     data = survey.train, 
#                     # n.trees = 1000,
#                     method = "gbm")

# survey.gbm

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

# fwrite(tune.grid, file.rf.parameters)

tune.grid <- fread(file.rf.parameters)
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

# saveRDS(rf.mod, file.rf.mod)
rf.mod <- readRDS(file.rf.mod)


## VARIABLE IMPORTANCE #################################################

resp.code <- c(sel.adaptation, "Count")
resp.code <- factor(resp.code, levels = resp.code)
names(resp.code) <- levels(rf.imp.var$resp)

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

# fwrite(rf.imp, file.rf.varimp)
rf.imp <- fread(file.rf.varimp)
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
  merge(rf.imp[pvalue < 0.05], var.sel.l,
        all.x = TRUE, all.y = FALSE, by = "code")

rf.imp.var[, resp := resp.code[as.character(resp)]]
setnames(rf.imp.var, "code", "expl")
setcolorder(rf.imp.var, c("resp", "expl", "category", "importance"))
setorder(rf.imp.var, resp, -importance)

fwrite(rf.imp.var, file.rf.varimp.sel)

rf.imp.var$category

rf.imp.var[, .N, by = c("resp", "code")][N>1]
rf.imp.var[code == "E10" & resp == "y.acc.6"]

rf.imp.var$resp

rf.imp.var[resp == "y.acc.2" & pvalue < 0.05,
           .(code, category, importance, main, sub)
           ][order(-importance)]

unique(rf.imp.var$code)




rf.imp.cat <-
  rf.imp.var |>
  DT(order(resp, category)
     , .(resp = resp.code[as.character(resp)],
         importance.max = max(importance),
         importance.mean = mean(importance),
         cat.n = length(importance)),
     by = c("resp", "category")) |>
  DT(, .(resp,
         category,
         importance.max,
         importance.mean,
         cat.n))

fwrite(rf.imp.cat, file.rf.catimp.sel)

library(ggdist)

svg(file.rf.catimp.plot, width = 6, height = 9)

ggplot(rf.imp.cat,
       aes(x = category)) +
  geom_bar(aes(x = importance.mean,
               y = category,
               fill = category),
           stat = "identity") +
  geom_bar(aes(x = importance.max,
               y = category,
               fill = category),
           stat = "identity",
           alpha = 0.5) +
  # geom_text(aes(x = 0.5, y = category, label = as.character(cat.n)),
  #           hjust = 1, colour = "white") +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  facet_wrap(vars(resp), ncol = 2) +
  guides(fill = "none") +
  labs(y = "Variable category", x = "Variable importance",
       caption = "Average and maximum variable importance by category. Dark shades indicate the\naverage importance across all variables of a category, while light shades refer to the\nimportance value of the most important variable in the given category.") +
  theme_ggdist() +
  theme(plot.caption = element_text(hjust = 0))

dev.off() 

rf.imp.var.agg <-
  rf.imp.var[,
             .(importance.mean = mean(importance) / length(vars.y.acc),
               importance.max = max(importance),
               count = .N),
             by = code][order(-importance.mean)] |>
  merge(var.sel.l[, .(code, category, main)], sort = FALSE)

svg(file.rf.catimp.plot, width = 6, height = 9)

ggplot(rf.imp.cat,
       aes(x = category)) +
  geom_bar(aes(x = importance.mean,
               y = category,
               fill = category),
           stat = "identity") +
  geom_bar(aes(x = importance.max,
               y = category,
               fill = category),
           stat = "identity",
           alpha = 0.5) +
  scale_y_discrete(limits = rev) +
  scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
  scale_fill_brewer(type = "qual", palette = "Set1") +
  facet_wrap(vars(resp), ncol = 2) +
  guides(fill = "none") +
  labs(y = "Variable category", x = "Variable importance",
       caption = "Average and maximum variable importance by category. Dark shades indicate the\naverage importance across all variables of a category, while light shades refer to the\nimportance value of the most important variable in the given category.") +
  theme_ggdist() +
  theme(plot.caption = element_text(hjust = 0))

dev.off() 



rf.imp.var.agg[1:10]

var.sel[code == "B37"]


rf.imp.var[, .(count = .N), by = c("resp", "main")][order(resp, -count)]
rf.imp.var[, .(count = .N), by = c("main")][order(-count)]




items <- vars.y.acc[1:10]
vars.pred <-
  rf.imp.var[order(-importance),
             .SD[1:3],
             by = resp
             ][, unique(code)]

vars.irt <- c(items, vars.pred)

survey.irt <- survey.fit[, ..vars.irt]

saveRDS(survey.irt, file.survey.irt)










## OLD CODE




items <- vars.y.acc[1:10]
pers.cov <- "B2"
# vars.demo <- c("F11", "F12", "F14", "F16", "F19")
# names(vars.demo) <- var.sel[code %in% vars.demo, main]
# vars.provinces <- paste0("A", 5:17)
# vars.prov.sel <-
#   lapply(survey.fit[, ..vars.provinces],
#          \(x) length(unique(x))) |>
#   unlist()
# vars.provinces <- vars.provinces[vars.prov.sel != 1]
# names(vars.provinces) <- var.sel[code %in% vars.provinces, sub]
# vars.control <- c(vars.demo, vars.provinces)

vars.control <- c("F11", "F12", "F14", "F16", "F19")
names(vars.control) <- var.sel[code %in% vars.control, main]

vars.irt <- c(items, pers.cov, vars.control)

# cat.lev <-
#   levels(survey.fit[[pers.cov[1]]]) |>
#   stri_replace_all_fixed(" ", "_") |>
#   tolower()
# cat.lev.c <- factor(cat.lev, levels = cat.lev)

survey.irt <-
  survey.fit[, ..vars.irt] |>
  setnames(vars.control, names(vars.control))

survey.irt[, id := 1:.N]

survey.irt <-
  melt(survey.irt,
       measure.vars = items,
       variable.name = "item",
       value.name = "resp")


# form.B2.1pl <-
#   bf(resp ~
#        B2 + 
#        (1 + B2 | item) + (1 | id))

# prior.B2.1pl <-
#   prior("student_t(3, 0, 2.5)",class = "b") +
#   prior("normal(0, 3)",class = "sd", group = "id") +
#   prior("normal(0, 3)", class = "sd", group = "item")

# mod.B2.1pl <-
#   brm(formula = form.B2.1pl,
#       data = survey.irt,
#       family = brmsfamily("bernoulli", "logit"),
#       prior = prior.B2.1pl)

# mod.B2.1pl <- add_criterion(mod.B2.1pl, c("loo", "waic"))

# plot(mod.B2.1pl)

# summary(mod.B2.1pl)

# hypothesis(mod.B2.1pl, "B2Yes - B2No > 0")

form.B2.con.1pl <-
  paste0("resp ~ B2 + ",
         paste(names(vars.control), collapse = " + "),
         " + (1 + B2 | item) + (1 | id)") |>
  as.formula() |>
  bf()


form.B2.con.1pl <-
  bf(resp ~
       B2 +
       age + gender + autochtonous + education + job +
        + (1 + B2 | item) + (1 | id))

prior.B2.con.1pl <-
  prior("student_t(3, 0, 2.5)",class = "b") +
  prior("normal(0, 3)",class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

mod.B2.con.1pl <-
  brm(formula = form.B2.con.1pl,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      prior = prior.B2.con.1pl)


mod.B2.con.1pl <- add_criterion(mod.B2.con.1pl, "loo")

saveRDS(mod.B2.con.1pl, paste0(path.results.brm, "mod.B2.con.1pl.rds"))
# mod.B2.con.1pl <- readRDS(paste0(path.results.brm, "mod.B2.con.1pl.rds"))


plot(mod.B2.con.1pl)

summary(mod.B2.con.1pl)

hypothesis(mod.B2.con.1pl, "B2Yes > 0")

variables(mod.B2.con.1pl)


ranef(mod.B2.con.1pl, summary = FALSE)$item[,,2] |>
apply(2, \(x) x + fixef(mod.B2.con.1pl, summary = FALSE, pars = "B2Yes")) |>
apply(2, \(x) quantile(x, c(0.025, 0.975)))

B2.ran <- ranef(mod.B2.1pl)

B2.item <- as.data.table(B2.ran$item[,,1], keep.rownames = "Item")
B2.item[, Code := resp.code[as.character(Item)]]

ggplot(B2.item) +
  geom_pointrange(aes(x = Estimate, y = Code,
                      xmin = Q2.5, xmax = Q97.5)) +
  scale_y_discrete(limits = rev)

B2.id <- as.data.table(B2.ran$id[,,1], keep.rownames = "ID")
B2.id[, rank := frank(Estimate)]

ggplot(B2.id) +
  geom_linerange(aes(y = rank, xmin = Q2.5, xmax = Q97.5),
                 linewidth = 0.5, alpha = 0.35) +
  geom_point(aes(x = Estimate, y = rank),
             size = 0.2)


conditional_effects(mod.B2.con.1pl, c("B2:autochtonous"))



form.B2.conint.1pl <-
  bf(resp ~
       B2 +
       age + gender + autochtonous + education + job +
       B2:age + B2:gender + B2:autochtonous + B2:education + B2:job +
       (1 + B2 | item) + (1 | id))

prior.B2.conint.1pl <-
  prior("student_t(3, 0, 2.5)",class = "b") +
  prior("normal(0, 3)",class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

mod.B2.conint.1pl <-
  brm(formula = form.B2.conint.1pl,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      prior = prior.B2.conint.1pl)

mod.B2.conint.1pl <- add_criterion(mod.B2.conint.1pl, "loo")

mod.B2.conint.1pl

loo_compare(mod.B2.con.1pl, mod.B2.conint.1pl)




survey.fit$F12

nrow(survey.irt)


unique(effects.item$draw)

str(b.imp)

b_sum <-
as_draws_matrix(mod.imp.1pl) |>
summary(mean,
        median,
        sd,
        mad,
        \(x) quantile2(x, c(0.025, 0.05, 0.1, 0.25,
                            0.75, 0.9, 0.95, 0.975))) |>
as.data.table()




b_sum[sign(q2.5) == sign(q97.5) & variable %like% "b_"]
b_sum[sign(q5) == sign(q95) & variable %like% "b_"]
b_sum[sign(q10) == sign(q90) & variable %like% "b_"]
b_sum[sign(q25) == sign(q75) & variable %like% "b_"]

ranef(mod.imp.1pl, summary = FALSE)$item[,,2] |>
apply(2, \(x) x + fixef(mod.imp.1pl, summary = FALSE, pars = "B2Yes")) |>
apply(2, \(x) quantile(x, c(0.025, 0.975)))




items <- vars.y.acc[1:10]
vars.pred <- var.sel[main == "future", code]
vars.demo <- c("F11", "F12", "F14", "F16", "F19")
names(vars.demo) <- var.sel[code %in% vars.demo, main]
vars.provinces <- paste0("A", 5:17)
vars.prov.sel <-
  lapply(survey.fit[, ..vars.provinces],
         \(x) length(unique(x))) |>
  unlist()
vars.provinces <- vars.provinces[vars.prov.sel != 1]
names(vars.provinces) <- var.sel[code %in% vars.provinces, sub]
vars.control <- c(vars.demo, vars.provinces)

pred.lev.simple <-
  factor(c(rep("low", 4), rep("high", 3)),
         levels = c("low", "high"))
names(pred.lev.simple) <- 
  c("Absolutely disagree",
    "Disagree",
    "Somewhat disagree",
    "Neutral",
    "Somewhat agree",
    "Agree",
    "Absolutely agree")


vars.irt <- c(items, vars.pred, vars.control)

survey.irt <-
  survey.fit[, ..vars.irt] |>
  setnames(vars.control, names(vars.control))
survey.irt[, id := 1:.N]

survey.irt[,
           (vars.pred) :=
              lapply(.SD,
                     \(x) pred.lev.simple[as.character(x)]),
           .SDcols = vars.pred]

survey.irt <-
  melt(survey.irt,
       measure.vars = items,
       variable.name = "item",
       value.name = "resp") |>
  melt(measure.vars = vars.pred,
       variable.name = "code",
       value.name = "level") |>
  merge(var.sel.l[, .(code, category)])

# form.future.1pl <-
#   bf(resp ~
#        category * level +
#        age + gender + autochtonous + education + job +
#        (1 + category*level | item) + (1 | id))

form.future.1pl <-
  paste0("resp ~ category*level + ",
         paste(names(vars.control), collapse = " + "),
         " + (1 + category*level | item) + (1 | id)") |>
  as.formula() |>
  bf()
      
prior.future.1pl <-
  prior("student_t(3, 0, 2.5)",class = "b") +
  prior("normal(0, 3)",class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

mod.future.1pl <- brm(
  formula = form.future.1pl,
  data = survey.irt,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior.future.1pl)

mod.future.1pl <- add_criterion(mod.future.1pl, "loo")





var.sel[main == "ACCAssistance"]


rf.imp.cat$adaptation

rf.imp[order(-importance),
       .SD[1:5],
       by = resp
       ][order(resp, -importance)]

var.sel[,
                .(code,
                  main,
                  sub,
                  category.personal_stakes,
                  category.threat_appraisal,
                  category.coping_appraisal,
                  category.control)]


       c("category.personal_stakes",
                      "category.threat_appraisal",
                      "category.coping_appraisal",
                      "category.control")) |>



DT(


rf.imp[order(-importance),
       .SD[1:5],
       by = resp
       ]

rf.imp[,
       apply(.SD, 2, \(x) sum(importance * x) / sum(x)),
         .SDcols = c("category.personal_stakes",
                    "category.threat_appraisal",
                    "category.coping_appraisal"),
      by = resp]


var.sel[,apply(.SD, 2, sum)/.N,
        .SDcols = c("category.personal_stakes",
                    "category.threat_appraisal",
                    "category.coping_appraisal")]

|>
ggplot(aes(x = code, y= importance, fill=importance)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  guides(fill= "none") +
  scale_fill_viridis_c() +
  facet_wrap(vars(resp))


rf.imp[resp == "y.acc.comb"] |>
ggplot(aes(x = code, y= importance, fill=importance)) + 
  geom_bar(stat="identity", position="dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  guides(fill= "none") +
  scale_fill_viridis_c()

rf.imp[,
       importance = 

                  c("category.personal_stakes",
                    "category.threat_appraisal",
                    "category.coping_appraisal")


var.sel[main == "future"]


rf.imp


# Extract variable importance






  mod.resp <- "y.acc.comb"
  form <- 
    as.formula(paste(mod.resp, "~",
                     paste(mod.preds, collapse = "+")))

  rf <-
    ranger(form,
           data = as.data.frame(survey.fit),
           mtry = 10,
           # probability = TRUE,
           respect.unordered.factors = "order",
           min.node.size = 20,
           num.trees = 10000,
           scale.permutation.importance = TRUE,
           importance = "permutation",
           # classification = TRUE,
           num.threads = 4)

rf

rf$confusion.matrix

auc[i] <-

    roc(survey.fit[[mod.resp]], rf$predictions[,1])$auc |>
    as.numeric() |>
    suppressMessages()

rf$predictions

tune.grid$score <- score

tune.grid[order(score)][1:150]


importance_pvalues(rf)

importance(rf)

rf

|>
as.data.frame() |>
DT(order(-importance)) |>
DT(1:20)

importance_pvalues(rf, method = "altmann", formula = form, data = survey.fit)

rf$confusion.matrix
?
str(rf, max.level = 1)

rf
pred.data <- predict(rf, data = survey.valid)
roc(survey.valid[[mod.resp]], pred.data$predictions[,1])

|>
plot()

data(iris)


iris <- iris[iris$Species == "virginica" | iris$Species == "versicolor", ]
iris$Species <- factor(iris$Species)  # setosa should be removed from factor



samples <- sample(NROW(iris), NROW(iris) * .5)
data.train <- iris[samples, ]
data.test <- iris[-samples, ]
forest.model <- train(Species ~., data.train)

result.predicted.prob <- predict(forest.model, data.test, type="prob") # Prediction

result.roc <- roc(data.test$Species, result.predicted.prob$versicolor) # Draw ROC curve.
plot(result.roc, print.thres="best", print.thres.best.method="closest.topleft")

result.coords <- coords(result.roc, "best", best.method="closest.topleft", ret=c("threshold", "accuracy"))
print(result.coords)#to get threshold and accuracy


ocRFall=
aucRF=pROC::auc(rocRFall)
print(aucRF)


var.imp <- importance(rf)

var.imp.dt <-
  data.table(code = names(var.imp), importance = var.imp) |>
  merge(var.sel)

var.imp.dt <- var.imp.dt[order(-importance)]
var.imp.dt[, code := factor(code, levels = rev(code))]
var.imp.dt[, name := factor(name, levels = rev(name))]

ggplot(var.imp.dt[1:50],
       aes(x = name, y= importance,fill=importance)) + 
 geom_bar(stat="identity", position="dodge") +
 coord_flip() +
 ylab("Variable Importance") +
 xlab("") +
 guides(fill=F) +
 scale_fill_viridis_c()

library(iml)

predictor <- Predictor$new(rf, data = survey.sel, y = y.acc.comb)


# Plot age effect
ale <- FeatureEffect$new(predictor, feature = "F11")
ale <- ale$results
ale$F11 <- factor(ale$F11, levels = levels(survey.sel$F11))
ggplot(ale, aes(x = F11, y = .value)) +
  geom_bar(stat = "identity")


# Plot logging effect
ale <- FeatureEffect$new(predictor, feature = "B38")
ale <- ale$results
ale$B38 <- factor(ale$B38, levels = levels(survey.sel$B38))
ggplot(ale, aes(x = B38, y = .value)) +
  geom_bar(stat = "identity") +
  labs(title = var.sel[code == "B38", question.main],
       subtitle = var.sel[code == "B38", question.sub])


ggplot(survey.fit, aes(x = B38 , y = y.acc.comb)) +
  geom_violin()


ale <- FeatureEffect$new(predictor, feature = "F14")
ale <- ale$results
ale$F11 <- factor(ale$F14, levels = levels(survey.sel$F14))
ggplot(ale, aes(x = F14, y = .value)) +
  geom_bar(stat = "identity") +
  labs(title = var.sel[code == "F14", question.main],
       subtitle = var.sel[code == "F14", question.sub])

ale <- FeatureEffect$new(predictor, method = "ice", feature = "B38")
ale <- ale$results
ale$B38 <- factor(ale$B38, levels = levels(survey.sel$B38))
ggplot(ale, aes(x = B38, y = .value, group = .id)) +
  geom_line() +
  labs(title = var.sel[code == "B38", question.main],
       subtitle = var.sel[code == "B38", question.sub])
