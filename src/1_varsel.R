library(data.table)
library(stringi)
library(brms)
library(projpred)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)

survey.fit <- readRDS(file.survey.rf)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)

vars.all <- names(survey.fit)
survey.fit <- survey.fit[,
                         vars.all[!vars.all %in% c(paste0("D", 1:10), "y.acc.comb")],
                         with = FALSE]

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

# vars.recode <- names(survey.irt)
# vars.recode <- vars.recode[!vars.recode %in% c("id", "resp")]

# recode.key.l <- list()
# for(i in seq_along(vars.recode)) {
#   var.foc <- vars.recode[i]
#   if(is.factor(survey.irt[[var.foc]])) {
#     var.val <- survey.irt[[var.foc]] 
#     var.recode <-
#       data.table(code = var.foc,
#                  level.survey = unique(var.val))
#     var.recode[, level.num := as.numeric(level.survey)]
#     nlev <- nrow(var.recode)
#     var.recode[, level.mod := factor(level.num, levels = as.character(1:nlev))]
#     var.val.rc <- as.numeric(var.val)
#     survey.irt[,
#                var.sel := factor(as.numeric(var.sel), levels = as.character(1:nlev)),
#                env = list(var.sel = var.foc)]
#     recode.key.l[[i]] <- var.recode
#   }
# }

# TEST: smaller df
survey.irt <- survey.irt[item %in% c("y.acc.1")]

## ITEM-RESPONSE MODEL 2PL ############################################


form.sel <- 
  paste0("resp ~ 1 + ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + ")) |>
  as.formula()

prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "b") +
             prior(normal(0, 3), class = "Intercept")

# form.sel <- 
#   paste0("resp ~ 1 + (1 | ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), ")") |>
#   as.formula()

# prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "sd") +
#              prior(normal(0, 3), class = "Intercept")


mod.sel <-
  brm(formula = form.sel,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      silent = 0,
      chains = 4,
      cores = 4,
      warmup = 2000,
      iter = 3000,
      refresh = 100,
      control = list(adapt_delta = 0.9),
      # backend = "cmdstanr",
      prior = prior.sel)


dir.create("../results/varsel", recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.sel, "../results/varsel/mod.1.rds")
# mod.sel <- readRDS("../results/varsel/mod.1.rds")

# mod.ref <- get_refmodel(mod.sel)
# mod.var.sel <- varsel(mod.ref)

n.terms.max <- round(0.25 * length(c(vars.pred.cont, vars.pred.cat)))

mod.ref <- get_refmodel(mod.sel)
mod.var.sel <- cv_varsel(mod.ref, nloo = 100, n.terms_max = n.terms.max)

# plot(mod.var.sel, alpha = 0.05, deltas = TRUE) +
# geom_hline(yintercept = summary(mod.var.sel)$selection$diff[1] * 0.5)

summary(mod.var.sel)
suggest_size(mod.var.sel, alpha = 0.05, type = "upper", pct = 0.25)

# saveRDS(mod.var.sel, "../results/varsel/sel.1.rds")
# mod.var.sel <- readRDS("..results/varsel/sel.1.rds")

# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::cmdstan_make_local(cpp_options = "CXXFLAGS += -ftemplate-depth=2048", append = FALSE)
# cmdstanr::rebuild_cmdstan()

