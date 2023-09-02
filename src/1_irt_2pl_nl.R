library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")

cont.nl <- TRUE
k.max <- 10
var.trans.log <- NULL
# var.trans.log <- c("A3")

survey.irt <- readRDS(file.survey.irt)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.pred.cat <- variables[code %in% names(survey.irt) & type == "categorical", code]
vars.pred.cont <- variables[code %in% names(survey.irt) & type == "continuous", code]


if(!is.null(var.trans.log)) {
  for(i in seq_along(var.trans.log)) {
    var.mean <- variables[code == var.trans.log[i], cont.mean]
    var.sd <- variables[code == var.trans.log[i], cont.sd]
    var.org <- var.mean + (var.sd * survey.irt[[var.trans.log[i]]])
    var.trans <- log(var.org)
    var.trans.std <- (var.trans - mean(var.trans)) / sd(var.trans)
    survey.irt[, var.sel := var.trans.std, env = list(var.sel = var.trans.log[i])]
  }
}


## ITEM-RESPONSE MODEL 2PL ############################################

if(cont.nl == TRUE) {

  vars.pred.cont.term <- character(0)
  for(i in seq_along(vars.pred.cont)) {
    var.k <- min(length(unique(survey.irt[[vars.pred.cont[i]]])), k.max)
    vars.pred.cont.term[i] <- paste0("s(", vars.pred.cont[i],
                                     ", by = item, k = ", var.k, ", bs = 'tp')")
  }

  form.eta <- 
    paste0("eta ~ ",
           paste0(c(vars.pred.cont.term, vars.pred.cat), collapse = " + "),
           " + (1 + ", paste0(vars.pred.cat, collapse = " + "), " |i| item)",
           " + (1 | id)") |>
    as.formula()

  if(length(vars.pred.cont.term) > 0) {
    prior.imp.2pl <-
      prior("normal(0, 5)", class = "b", nlpar = "eta") +
      prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
      prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
      prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
      prior("normal(0, 3)", class = "sds", nlpar = "eta") +
      prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")
  } else {
    prior.imp.2pl <-
      prior("normal(0, 5)", class = "b", nlpar = "eta") +
      prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
      prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
      prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
      prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")
  }

} else {

  form.eta <- 
    paste0("eta ~ ",
           paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "),
           " + (1 + ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), " |i| item)",
           " + (1 | id)") |>
    as.formula()


  prior.imp.2pl <-
    prior("normal(0, 5)", class = "b", nlpar = "eta") +
    prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
    prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
    prior("normal(0, 3)", class = "sd", group = "item", nlpar = "eta") +
    prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")

}


form.logalpha <- logalpha ~ 1 + (1 |i| item)

form.imp.2pl <-
  bf(resp ~ exp(logalpha) * eta,
     form.eta,
     form.logalpha,
     nl = TRUE)


mod.imp.2pl <-
  brm(formula = form.imp.2pl,
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
      prior = prior.imp.2pl)


dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.imp.2pl, file.irt.mod.2pl.nl)

