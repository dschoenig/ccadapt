library(data.table)
library(stringi)
library(brms)

source("paths.R")
source("utilities.R")


survey.irt <- readRDS(file.survey.irt)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.pred <- variables[code %in% names(survey.irt), code]


## VARIABLE TREATMENT #################################################

# Treat A19 as categorical variable (due to limited number of distinct
# responses)

A19.mean <- variables[code == "A19", cont.mean]
A19.sd <- variables[code == "A19", cont.sd]
breaks <- c(-Inf, seq(0, 100, 10))
labels <-
  c("0", paste0(paste0("(", breaks[2:(length(breaks)-1)], ","),
                paste0(breaks[3:length(breaks)], "]")))
survey.irt[,
           A19 := cut(((A19 * A19.sd) + A19.mean),
                      breaks = breaks, labels = labels)]

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
      chains = 4,
      cores = 4,
      threads = 8,
      warmup = 2500,
      iter = 5000,
      refresh = 25,
      prior = prior.imp.2pl)


dir.create(path.results.irt, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.imp.2pl, file.irt.mod.2pl)

