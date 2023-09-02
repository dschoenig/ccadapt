library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


survey.irt <- readRDS(file.survey.irt)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.pred.cat <- variables[code %in% names(survey.irt) & type == "categorical", code]
vars.pred.cont <- variables[code %in% names(survey.irt) & type == "continuous", code]


form <-
  bf(resp ~ exp(logalpha) * eta,
     eta ~ s(A3, by = item, k = 30) + (1|i|item) + (1|id),
     logalpha ~ 1 + (1 |i| item),
     nl = TRUE)

prior.mod <-
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 3)", class = "sds", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")



library(mgcv)

length(unique(survey$A19)) -1

mod <-
  bam(resp ~ s(A3, by = item, bs = "tp") +
      s(item, bs = "re") +
      s(id, bs = "re"),
      data = survey.irt, family=binomial, method = "fREML", discrete = TRUE)

plot(mod, pages = 1)

library(data.table)
library(brms)

source("paths.R")
source("utilities.R")

survey.rf <- readRDS(file.survey.rf)

breaks <- c(-Inf, seq(0, 100, 10))
labels <-
  c("0",
    paste0(paste0("(", breaks[2:(length(breaks)-1)], ","),
           paste0(breaks[3:length(breaks)], "]")))

survey.rf2 <- copy(survey.rf)
survey.rf2$A19 <- cut(survey.rf$A19, breaks = breaks, labels = labels)

tune.grid <- fread(file.rf.parameters)
tune.grid[, resp := factor(resp, levels = vars.y.acc)]
rf.par <- tune.grid[, .SD[which.min(score)], by = resp]


rf.mod2 <- list()
for(i in 1:nrow(rf.par)) {
    mod.resp <- rf.par$resp[i]
    form <- 
      as.formula(paste(mod.resp, "~",
                       paste(mod.preds, collapse = "+")))
  if(rf.par$type[i] == "probability") {
    rf.mod2[[i]] <-
      ranger(form,
             data = as.data.frame(survey.rf2),
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
    rf.mod2[[i]] <-
      ranger(form,
             data = as.data.frame(survey.rf2),
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

names(rf.mod2) <- rf.par$resp

unlist(lapply(rf.mod2, \(x) x$prediction.error))
unlist(lapply(rf.mod, \(x) x$prediction.error))

rf.mod <- rf.mod2

n.var.imp <- 3
rf.mod.sel <- list()
mod.error.i <- list()
mod.drop.i <- list()

for(i in seq_along(rf.mod)) {
 
  message(paste0("Variable selection for model ", i, "/", length(rf.mod), " …"))

  rf.foc <- rf.mod[[i]]
  var.fit <- mod.preds
  survey.drop <- survey.fit

  iter <- length(mod.preds) - n.var.imp

  mod.error.j <- numeric(0)
  mod.drop.j <- numeric(0)

  for(j in 1:iter) {
 
    message(paste0("Iteration ", j, "/", iter, " …"))
    
    mod.imp <- sort(importance(rf.foc))
    var.drop <- names(mod.imp)[1]
    var.fit <- var.fit[var.fit != var.drop]
    survey.drop <- survey.drop[, -var, env = list(var = I(var.drop))]
    
    mod.resp <- rf.par$resp[i]
    form <- 
      as.formula(paste(mod.resp, "~",
                       paste(var.fit, collapse = "+")))

    if(rf.par$type[i] == "probability") {
      rf.foc <-
        ranger(form,
               data = survey.drop,
               num.trees = rf.par$num.trees[i],
               mtry = min(rf.par$mtry[i], length(var.fit)),
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
      rf.foc <-
        ranger(form,
               data = as.data.frame(survey.drop),
               num.trees = rf.par$num.trees[i],
               mtry = min(rf.par$mtry[i], length(var.fit)),
               min.node.size = rf.par$min.node.size[i],
               splitrule = rf.par$splitrule[i],
               respect.unordered.factors = "order",
               scale.permutation.importance = TRUE,
               importance = "permutation",
               seed = rf.par$seed[i],
               num.threads = 4)
    }

    mod.error.j[j] <- rf.foc$prediction.error
    mod.drop.j[j] <- var.drop

  }

  mod.error.i[[i]] <- mod.error.j
  mod.drop.i[[i]] <- mod.drop.j
  
  rf.mod.sel[[i]] <- rf.foc

}







mod.2plo <- readRDS("../results/irt/mod.imp.2pl.log.rds")
mod.2pl <- readRDS("../results/irt/mod.imp.2pl.rds")
mod.2plo <- add_criterion(mod.2plo, criterion = "loo")
mod.2pl <- add_criterion(mod.2pl, criterion = "loo")
loo_compare(mod.2plo, mod.2pl)

fe1 <- fixef(mod.1pl)
fe2 <- fixef(mod.2pl)


summary(mod.2pl)



library(data.table)
library(brms)

source("paths.R")
source("utilities.R")


mod.1pl <- readRDS("../results/irt/mod.imp.1pl.rds")
mod.2pl <- readRDS("../results/irt/mod.imp.2pl.rds")

mod.1pl <- add_criterion(mod.1pl, criterion = "loo")

mod.2pl <- add_criterion(mod.2pl, criterion = "loo")


loo_compare(mod.1pl, mod.2pl)

fe1 <- fixef(mod.1pl)
fe2 <- fixef(mod.2pl)


## MODEL VALIDATION ####################################################

library(DHARMa)

survey.irt <- readRDS(file.survey.irt)

draw.ids <- sample(1:1e4, 1e3)

mod.1pl.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod.1pl, draw.ids = draw.ids)),
  observedResponse = survey.irt$resp,
  fittedPredictedResponse = apply(t(posterior_epred(mod.1pl, draw.ids = draw.ids)), 1, mean),
  integerResponse = TRUE)

plot(mod.1pl.check)


mod.2pl.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod.2pl, draw_ids = draw.ids)),
  observedResponse = survey.irt$resp,
  fittedPredictedResponse = apply(t(posterior_epred(mod.2pl, draw_ids = draw.ids)), 1, mean),
  integerResponse = TRUE)

plot(mod.2pl.check)


plotResiduals(mod.1pl.check, form = as.character(survey.irt$F14))
plotResiduals(mod.1pl.check, form = as.character(survey.irt$F11))

plotResiduals(mod.2pl.check, form = as.character(survey.irt$F14))
plotResiduals(mod.2pl.check, form = as.character(survey.irt$F11))


library(bayesplot)

survey.irt[,obs := 1:.N]

y <- survey.irt[, .(p.item = mean(resp)), by = item][, p.item]

y <- survey.irt$resp
# yrep <- posterior_predict(mod.2pl, draw_ids = draw.ids)
yrep <- posterior_predict(mod.2pl)
group <- survey.irt$item

ppc_stat_grouped(y = y, yrep = yrep, group = survey.irt$item, stat = "mean", binwidth = 0.01) +
  theme_default()


library(shinystan)
launch_shinystan(mod.2pl)

#################################################################################

## Alternative white & blacklists

## STRUCTURAL CONDITIONS #######################################################

## Blacklist

cat.observed <- c("personal_stakes",
                  "threat_appraisal",
                  "coping_appraisal",
                  "control",
                  "adaptation")

var.latent <- var.bn[latent == TRUE, name]

# Only allow links within certain explanatory categories, exclude links within
# the rest, and between all categories
bl.observed <- 
  expand.grid(cat1 = cat.observed,
              cat2 = cat.observed) |>
  as.data.table()
bl.observed <- bl.observed[!(cat1 == cat2 & cat1 %in% cat.observed[1:4])]
bl.observed.l <- list()
for(i in 1:nrow(bl.observed)){
  cond1 <- paste0("latent == FALSE & category.", bl.observed$cat1[i], "==1")
  cond2 <- paste0("latent == FALSE & category.", bl.observed$cat2[i], "==1")
  bl.observed.l[[i]] <-
    expand.grid(from = var.bn[eval(parse(text = cond1)), code],
                to = var.bn[eval(parse(text = cond2)), code]) |>
    as.data.table()
}
bl.observed.dt <- rbindlist(bl.observed.l)

# Disallow all incoming links to latent variables
bl.obslat.dt <-
  expand.grid(from = var.bn[latent == FALSE, code],
              to = var.bn[latent == TRUE, code]) |>
  as.data.table()

# Disallow links from latent variables towards observed variables (including latent) that are not
# in the same category
bl.latobs <-
  expand.grid(lat = var.latent,
              cat = cat.observed) |>
  as.data.table()
bl.latobs <- bl.latobs[as.character(lat) != as.character(cat)]
bl.latobs.l <- list()
for(i in 1:nrow(bl.latobs)){
  cond <- paste0("category.", bl.latobs$cat[i], "==1")
  bl.latobs.l[[i]] <-
    data.table(from = var.bn[latent == TRUE & name == bl.latobs$lat[i], code],
               to = var.bn[eval(parse(text = cond)), code])
}
bl.latobs.dt <- rbindlist(bl.latobs.l)

# Combine blacklists

arc.blacklist <- 
  rbindlist(list(bl.observed.dt, bl.latobs.dt, bl.obslat.dt))[from != to] |>
  unique() |>
  as.matrix()


## Whitelist

# # Force links from latent variables to adaptation variables
# wl.latadp <-
#   data.table(lat = var.latent,
#              cat = "adaptation")
# wl.latadp.l <- list()
# for(i in 1:nrow(wl.latadp)){
#   cond <- paste0("category.", wl.latadp$cat[i], "== 1")
#   wl.latadp.l[[i]] <-
#     data.table(from = var.bn[latent == TRUE & name == wl.latadp$lat[i], code],
#                to = var.bn[eval(parse(text = cond)), code])
# }
# wl.latadp.dt <- rbindlist(wl.latadp.l)

# Force links from latent variables to observed variables of same category
wl.latobs <-
  data.table(lat = var.latent,
             cat = var.latent)
wl.latobs.l <- list()
for(i in 1:nrow(wl.latobs)){
  cond <- paste0("latent == FALSE & category.", wl.latobs$cat[i], "== 1")
  wl.latobs.l[[i]] <-
    data.table(from = var.bn[latent == TRUE & name == wl.latobs$lat[i], code],
               to = var.bn[eval(parse(text = cond)), code])
}
wl.latobs.dt <- rbindlist(wl.latobs.l)


arc.whitelist <- 
  rbindlist(list(wl.latobs.dt)) |>
  unique() |>
  as.matrix()



###



### Alternative fitting

# Learn structure of compartments

cat.fit <- c("personal_stakes",
             "threat_appraisal",
             "coping_appraisal")


bn.struct.cat <- list()

for(i in seq_along(cat.fit)) {
  # i=1
  cond.cat <-
    parse(text = paste0("latent == FALSE & category.", cat.fit[i], " == 1"))
  bn.data.cat <- bn.data[, var.bn[eval(cond.cat), code], with = FALSE]
  bn.struct.cat[[i]] <-
    structural.em(bn.data.cat,
                  maximize = "tabu",
                  maximize.args = list(score = "bic"),
                  fit = "bayes",
                  impute = "bayes-lw",
                  debug = TRUE)
}

arcs.init <- do.call(rbind, lapply(bn.struct.cat, arcs))

bn.init <- empty.graph(names(bn.data))
arcs(bn.init) <- arcs.init

bn.fit(bn.init, data = bn.data, method = "hdir")

graphviz.plot(bn.struct.cat[[i]])

