args <- commandArgs(trailingOnly = TRUE)Rscript 1_irt_2pl.R TRUE

library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


cont.nl <- as.logical(args[1])
# cont.nl <- FALSE
k.max <- 10

survey.fit.w <- readRDS(file.survey.fit.w)
variables <- readRDS(file.variables.proc)
sel.res.sum <- readRDS(file.var.sel.res)


vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

vars.pred <-
  sel.res.sum[size <= size.sel & !is.na(expl),
              sort(unique(expl))]

vars.pred.cat <- variables[code %in% vars.pred & type == "categorical", code]
vars.pred.cont <- variables[code %in% vars.pred & type == "continuous", code]

survey.fit.w[, id := 1:.N]

survey.irt <- 
  survey.fit.w[ , c("id", vars.adapt, vars.pred), with = FALSE] |>
  melt(id.vars = c("id", vars.pred),
       measure.vars = vars.adapt,
       variable.name = "item",
       value.name = "resp")

saveRDS(survey.irt, file.survey.irt)



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
      empty = TRUE,
      prior = prior.imp.2pl)


dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

file.result <- ifelse(cont.nl, file.irt.mod.2pl.nl, file.irt.mod.2pl)

message(paste0("Exporting results to ", file.result, " …"))

saveRDS(mod.imp.2pl, file.result)



