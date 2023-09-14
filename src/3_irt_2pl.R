args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


cont.nl <- as.logical(args[1])
# cont.nl <- FALSE
k.max <- 10
# resp.type <- as.character(args[2])
# resp.type = "willingness"


if(resp.type == "willingness") {
  file.var.sel.res <- file.var.sel.w.res
  file.survey.fit <- file.survey.fit.w
  file.survey.irt <- file.survey.irt.w
  file.irt.mod.2pl <- file.irt.w.mod.2pl
  file.irt.mod.2pl.nl <- file.irt.w.mod.2pl.nl
}
if(resp.type == "urgency") {
  file.var.sel.res <- file.var.sel.u.res
  file.survey.fit <- file.survey.fit.u
  file.survey.irt <- file.survey.irt.u
  file.irt.mod.2pl <- file.irt.u.mod.2pl
  file.irt.mod.2pl.nl <- file.irt.u.mod.2pl.nl
}
if(resp.type == "categorical") {
  file.var.sel.res <- file.var.sel.c.res
  file.survey.fit <- file.survey.fit.c
  file.survey.irt <- file.survey.irt.c
  file.irt.mod.2pl <- file.irt.c.mod.2pl
  file.irt.mod.2pl.nl <- file.irt.c.mod.2pl.nl
}


survey.fit <- readRDS(file.survey.fit)
variables <- readRDS(file.variables.proc)
sel.res.sum <- readRDS(file.var.sel.res)


vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

vars.pred <-
  sel.res.sum[size <= size.sel & !is.na(expl),
              sort(unique(expl))]

vars.pred.cat <- variables[code %in% vars.pred & type == "categorical", code]
vars.pred.cont <- variables[code %in% vars.pred & type == "continuous", code]

survey.irt <- 
  survey.fit[ , c("id", vars.adapt, vars.pred), with = FALSE] |>
  melt(id.vars = c("id", vars.pred),
       measure.vars = vars.adapt,
       variable.name = "item",
       value.name = "resp")

survey.irt <- survey.irt[!is.na(resp)]

saveRDS(survey.irt, file.survey.irt)


## ITEM-RESPONSE MODEL 2PL ############################################

if(resp.type != categorical) {

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

} else { # Categorical model

   if(cont.nl == TRUE) {

      vars.pred.cont.term <- character(0)
      for(i in seq_along(vars.pred.cont)) {
        var.k <- min(length(unique(survey.irt[[vars.pred.cont[i]]])), k.max)
        vars.pred.cont.term[i] <- paste0("s(", vars.pred.cont[i],
                                         ", by = item, k = ", var.k, ", bs = 'tp')")
      }

      form.eta <- 
        paste0("resp ~ ",
               paste0(c(vars.pred.cont.term, vars.pred.cat), collapse = " + "),
               " + (1 + ", paste0(vars.pred.cat, collapse = " + "), " |i| item)",
               " + (1 | id)") |>
        as.formula()

      if(length(vars.pred.cont.term) > 0) {
        prior.imp.2pl <-
          prior("normal(0, 5)", class = "b") +
          prior("normal(0, 3)", class = "sd", group = "id") +
          prior("normal(0, 3)", class = "sd", group = "item") +
          prior("normal(0, 3)", class = "sds") +
          prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")
      } else {
        prior.imp.2pl <-
          prior("normal(0, 5)", class = "b") +
          prior("normal(0, 3)", class = "sd", group = "id") +
          prior("normal(0, 3)", class = "sd", group = "item") +
          prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")
      }

    } else {

      form.eta <- 
        paste0("resp ~ ",
               paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "),
               " + (1 + ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), " |i| item)",
               " + (1 | id)") |>
        as.formula()


      prior.imp.2pl <-
        prior("normal(0, 5)", class = "b") +
        prior("normal(0, 3)", class = "sd", group = "id") +
        prior("normal(0, 3)", class = "sd", group = "item") +
        prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")

    }

    form.disc <- disc ~ 1 + (1 |i| item)

    form.imp.2pl <-
      bf(form.eta,
         form.disc)

}


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

file.result <- ifelse(cont.nl, file.irt.mod.2pl.nl, file.irt.mod.2pl)

message(paste0("Exporting results to ", file.result, " …"))

saveRDS(mod.imp.2pl, file.result)



