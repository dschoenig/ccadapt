library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


survey.irt <- readRDS(file.survey.irt)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.pred <- variables[code %in% names(survey.irt), code]


## ITEM-RESPONSE MODEL 2PL ############################################


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
      chains = 4,
      cores = 4,
      threads = 8,
      warmup = 2500,
      iter = 5000,
      refresh = 25,
      prior = prior.imp.1pl)


dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.imp.1pl, paste0(path.results.irt, "mod.imp.1pl.rds"))

