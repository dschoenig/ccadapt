library(data.table)
library(stringi)
library(brms)
library(DHARMa)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)


survey.irt <- readRDS(file.survey.irt)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.pred <- variables[code %in% names(survey.irt), code]



## ITEM-RESPONSE MODEL 1PL #############################################


form.imp.1pl <-
  paste0("resp ~ ",
         paste0(vars.pred, collapse = " + "),
         " + (1 + ", paste0(vars.pred, collapse = " + "), " | item)",
         " + (1 | id)") |>
  as.formula() |>
  bf()

prior.imp.1pl <-
  prior("student_t(3, 0, 2.5)",class = "b") +
  prior("normal(0, 3)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")


mod.imp.1pl <-
  brm(formula = form.imp.1pl,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      init = 0,
      silent = 0,
      refresh = 1,
      prior = prior.imp.1pl)


saveRDS(mod.imp.1pl, paste0(path.results.brm, "mod.imp.1pl.rds"))



## ITEM-RESPONSE MODEL 2PL ############################################


form.eta <- 
  paste0("eta ~ ",
         paste0(vars.pred, collapse = " + "),
         " + (1 + ", paste0(vars.pred, collapse = " + "), " |i| item)",
         " + (1 | id)") |>
  as.formula()


form.logalpha <- logalpha ~ 1 + (1 |i| item)

# form.logalpha <- 
#   paste0("logalpha ~ ",
#          paste0(vars.pred, collapse = " + "),
#          " + (1 + ", paste0(vars.pred, collapse = " + "), " |i| item)") |>
#   as.formula()

form.imp.2pl <-
  bf(resp ~ exp(logalpha) * eta,
     form.eta,
     form.logalpha,
     nl = TRUE)

prior.imp.2pl <-
  prior("normal(0, 5)", class = "b", nlpar = "eta") +
  prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
  prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
  prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
  prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

mod.imp.2pl <-
  brm(formula = form.imp.2pl,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      init = 0,
      silent = 0,
      refresh = 1,
      prior = prior.imp.2pl)


saveRDS(mod.imp.2pl, paste0(path.results.brm, "mod.imp.2pl.rds"))


## MODEL VALIDATION ####################################################

# mod.imp.1pl <- readRDS(mod.imp.1pl)

mod.imp.1pl.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod.imp.1pl)),
  observedResponse = survey.irt$resp,
  fittedPredictedResponse = apply(t(posterior_epred(mod.imp.1pl)), 1, mean),
  integerResponse = TRUE)

plot(mod.imp.1pl.check)

plotResiduals(mod.imp.1pl.check, form = as.character(survey.irt$F14))
