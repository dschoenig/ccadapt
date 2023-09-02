library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


# cont.nl <- TRUE
k.max <- 10


survey.fit <- readRDS(file.survey.rf)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)



# Remove ordering from factor variables
var.ord <- names(which(unlist(lapply(survey.fit, is.ordered))))
survey.fit[, (var.ord) := lapply(.SD,
                                 \(x) factor(x,
                                             ordered = FALSE,
                                             levels = levels(x))),
           .SDcols = var.ord]

survey.irt <- copy(survey.fit)

var.cat <- variables[code %in% names(survey.irt) &
                     type == "categorical" &
                     category.adaptation != 1,
                     .(code, cat.ref)]
for(i in 1:nrow(var.cat)) {
  survey.irt[[var.cat$code[i]]] <-
    relevel(survey.irt[[var.cat$code[i]]], var.cat$cat.ref[i])
}

var.cont <- variables[code %in% names(survey.irt) &
                      type == "continuous" &
                      category.adaptation != 1,
                      .(code, cont.mean, cont.sd)]
# var.cont <- var.sel[type == "continuous", .(code, cont.mean, cont.sd)]
if(nrow(var.cont) > 0) {
  for(i in 1:nrow(var.cont)) {
    survey.irt[[var.cont$code[i]]] <-
      (survey.irt[[var.cont$code[i]]] - var.cont$cont.mean[i]) /
      var.cont$cont.sd[i]
  }
}

survey.irt <- copy(survey.irt)

survey.irt[, id := 1:.N]


sel.adaptation <- variables[main == "mgmtChangeACC", code]
items <- c(paste0("y.acc.", 1:length(sel.adaptation)))

survey.irt <-
  melt(copy(survey.irt),
       measure.vars = items,
       variable.name = "item",
       value.name = "resp")


vars.pred.cat <- variables[code %in% names(survey.irt) & type == "categorical", code]
vars.pred.cont <- variables[code %in% names(survey.irt) & type == "continuous", code]

# Fix names to avoid brms error
setnames(survey.irt, stri_replace_first_regex(names(survey.irt), "^(\\w)(\\d)$", "$10$2"))
vars.pred.cat <- stri_replace_first_regex(vars.pred.cat, "^(\\w)(\\d)$", "$10$2")
vars.pred.cont <- stri_replace_first_regex(vars.pred.cont, "^(\\w)(\\d)$", "$10$2")



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


  prior.imp.2pl <-
    prior(horseshoe(df = 3, par_ratio = 0.1, main = TRUE), class = "b", nlpar = "eta") +
    prior(horseshoe(), class = "sds", nlpar = "eta") +
    prior(horseshoe(), class = "sd", group = "item", nlpar = "eta") +
    prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
    prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
    prior("normal(0, 1)", class = "sd", group = "item", nlpar = "logalpha")


} else {

  form.eta <- 
    paste0("eta ~ ",
           paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "),
           " + (1 + ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), " |i| item)",
           " + (1 | id)") |>
    as.formula()


  prior.imp.2pl <-
    prior(horseshoe(df = 3, par_ratio = 0.1, main = TRUE), class = "b", nlpar = "eta") +
    prior(horseshoe(), class = "sd", group = "item", nlpar = "eta") +
    prior("normal(0, 3)", class = "sd", group = "id", nlpar = "eta") +
    prior("normal(0, 1)", class = "b", nlpar = "logalpha") +
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
      control = list(adapt_delta = 0.9),
      prior = prior.imp.2pl)


dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.imp.2pl, file.irt.mod.2pl.hs.nl)

