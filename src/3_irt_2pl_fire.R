args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


resp.type <- as.character(args[1])
k.max <- 10
dim.poly <- 3

# resp.type = "willingness"


if(resp.type == "willingness") {
  file.var.sel.res <- file.var.sel.w.res.fire
  file.survey.fit <- file.survey.fit.w
  file.survey.irt <- file.survey.irt.w.fire
  file.irt.mod.2pl <- file.irt.w.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.w.mod.2pl.nl.fire
  path.results.irt <- path.results.w.irt
}
if(resp.type == "urgency") {
  file.var.sel.res <- file.var.sel.u.res.fire
  file.survey.fit <- file.survey.fit.u
  file.survey.irt <- file.survey.irt.u.fire
  file.irt.mod.2pl <- file.irt.u.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.u.mod.2pl.nl.fire
  path.results.irt <- path.results.u.irt
}
if(resp.type == "categorical") {
  file.var.sel.res <- file.var.sel.c.res.fire
  file.survey.fit <- file.survey.fit.c
  file.survey.irt <- file.survey.irt.c.fire
  file.irt.mod.2pl <- file.irt.c.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.c.mod.2pl.nl.fire
  path.results.irt <- path.results.c.irt
}


survey.fit <- readRDS(file.survey.fit)
variables <- readRDS(file.variables.proc)
cat.levels <- readRDS(file.cat.levels.proc)
sel.res.sum <- readRDS(file.var.sel.res)

vars.adapt <- c("D01", "D02", "D03", "D04", "D05", "D07")
vars.pred <- names(survey.fit)[!names(survey.fit) %in% c(vars.adapt, "id")]
vars.sel <-
  sel.res.sum[size <= size.sel & !is.na(expl),
              sort(unique(expl))]


# Polynomial contrasts for Likert scales

vars.pred.lik <-
  variables[code %in% vars.pred & cat.ord == TRUE &
            cat.scale %in% c("d", "i", "l", "n", "m"),
            code]
for(i in seq_along(vars.pred.lik)) {
  var.scale <- variables[code == vars.pred.lik[i], cat.scale]
  var.lev <- cat.levels[cat.scale == var.scale][order(level.id), level]
  survey.fit[,
             var.ord := factor(var.ord,
                               levels = var.lev, 
                               ordered = TRUE),
             env = list(var.ord = vars.pred.lik[i])]
  contrasts(survey.fit[[vars.pred.lik[i]]], how.many = dim.poly) <- contr.poly(7)
}



vars.pred.cat <- variables[code %in% vars.pred &
                           type == "categorical" &
                           cat.ord == FALSE,
                           code]
vars.pred.cont <- variables[code %in% vars.pred &
                            type == "continuous",
                            code]

vars.pred.lik.poly <- character()
for(i in seq_along(vars.pred.lik)) {
  var.lik <- survey.fit[[vars.pred.lik[i]]]
  cont.mat <- contrasts(var.lik)[var.lik,1:dim.poly]
  poly.names <- paste0(vars.pred.lik[i], "_p", 1:dim.poly)
  vars.pred.lik.poly <- c(vars.pred.lik.poly, poly.names)
  survey.fit[, (poly.names) := as.data.table(cont.mat)]
}

vars.pred.cont.poly <- character()
for(i in seq_along(vars.pred.cont)) {
  var.cont <- survey.fit[[vars.pred.cont[i]]]
  poly.mat <- poly(var.cont, degree = dim.poly)
  poly.names <- paste0(vars.pred.cont[i], "_p", 1:dim.poly)
  vars.pred.cont.poly <- c(vars.pred.cont.poly, poly.names)
  survey.fit[, (poly.names) := as.data.table(poly.mat)]
}


vars.pred.all <- c(vars.pred.cont.poly, vars.pred.lik.poly, vars.pred.cat)

survey.irt <- 
  survey.fit[ , c("id", vars.adapt, vars.sel), with = FALSE] |>
  melt(id.vars = c("id", vars.sel),
       measure.vars = vars.adapt,
       variable.name = "item",
       value.name = "resp",
       value.factor = TRUE)


survey.irt <- survey.irt[!is.na(resp)]


saveRDS(survey.irt, file.survey.irt)


## ITEM-RESPONSE MODEL 2PL ############################################

if(resp.type != "categorical") {

  form.eta <- 
    paste0("eta ~ ",
           paste0(vars.sel, collapse = " + "),
           " + (1 + ", paste0(vars.sel, collapse = " + "), " |i| item)",
           " + (1 | id)") |>
    as.formula()


  prior.irt.2pl <-
    prior("normal(0, 5)", class = "b", nlpar = "eta") +
    prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
    prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
    prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
    prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")


  form.logalpha <- logalpha ~ 1 + (1 |i| item)

  form.irt.2pl <-
    bf(resp ~ exp(logalpha) * eta,
       form.eta,
       form.logalpha,
       nl = TRUE)


  irt.fam <- brmsfamily("bernoulli", "logit")


} else { # Categorical model

  form.eta <- 
    paste0("resp ~ ",
           paste0(vars.sel, collapse = " + "),
           " + (1 + ", paste0(vars.sel, collapse = " + "), " |i| item)",
           " + (1 | id)") |>
    as.formula()


  prior.irt.2pl <-
    prior("normal(0, 5)", class = "Intercept") +
    prior("normal(0, 5)", class = "b") +
    prior("normal(0, 3)", class = "sd", group = "id") +
    prior("normal(0, 3)", class = "sd", group = "item") +
    prior("normal(0, 1)", class = "Intercept", dpar = "disc") +
    prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")


  form.disc <- disc ~ 1 + (1 |i| item)

  form.irt.2pl <-
    bf(form.eta,
       form.disc)

  irt.fam <- brmsfamily("cumulative", "logit")

}


mod.irt.2pl <-
  brm(formula = form.irt.2pl,
      data = survey.irt,
      family = irt.fam,
      init = 0,
      silent = 0,
      chains = 4,
      cores = 4,
      threads = 8,
      warmup = 7500,
      iter = 10000,
      refresh = 25,
      control = list(adapt_delta = 0.99),
      backend = "cmdstanr",
      prior = prior.irt.2pl)

dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

file.result <- file.irt.mod.2pl

message(paste0("Exporting results to ", file.result, " …"))

saveRDS(mod.irt.2pl, file.result)



