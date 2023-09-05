args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(stringi)
library(brms)
library(projpred)

source("paths.R")
source("utilities.R")

mod.id <- as.integer(args[1])
# mod.id <- 9

options(mc.cores = 4)

variables <- readRDS(file.variables.proc)
survey.fit.w <- readRDS(file.survey.fit.w)

vars.adapt <- variables[category.adaptation == TRUE, sort(code)]
var.resp <- vars.adapt[mod.id]
vars.pred <- names(survey.fit.w)[!names(survey.fit.w) %in% vars.adapt]

file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")


## SIMPLIFY FACTOR LEVELS #############################################

# Purely to avoid parsing errors in projpred

vars.recode <- names(survey.fit.w)
vars.recode <- vars.recode[!vars.recode %in% vars.adapt]

recode.key.l <- list()
for(i in seq_along(vars.recode)) {
  var.foc <- vars.recode[i]
  if(is.factor(survey.fit.w[[var.foc]])) {
    var.val <- survey.fit.w[[var.foc]] 
    var.recode <-
      data.table(code = var.foc,
                 level.survey = unique(var.val))
    var.recode[, level.num := as.numeric(level.survey)]
    nlev <- nrow(var.recode)
    var.recode[, level.mod := factor(level.num, levels = as.character(1:nlev))]
    var.val.rc <- as.numeric(var.val)
    survey.fit.w[,
               var.sel := factor(as.numeric(var.sel), levels = as.character(1:nlev)),
               env = list(var.sel = var.foc)]
    recode.key.l[[i]] <- var.recode
  }
}

recode.key <- rbindlist(recode.key.l)


## SELECTION MODEL WITH HORSESHOE PRIOR ################################

message(paste0("Performing variable selection for adaptation action '",
               var.resp, "' …"))

form.sel <- 
  paste0(var.resp, " ~ 1 + ", paste0(vars.pred, collapse = " + ")) |>
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
      data = survey.fit.w,
      family = brmsfamily("bernoulli", "logit"),
      silent = 0,
      chains = 4,
      cores = 4,
      warmup = 7500,
      iter = 10000,
      thin = 2,
      refresh = 100,
      control = list(adapt_delta = 0.99),
      # backend = "cmdstanr",
      prior = prior.sel)


dir.create(path.results.varsel, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.sel, file.mod.sel)
# mod.sel <- readRDS(file.mod.sel)


n.terms.max <- round(0.25 * length(vars.pred))

mod.ref <- get_refmodel(mod.sel)
mod.var.sel <- cv_varsel(mod.ref, nterms_max = n.terms.max)
# mod.var.sel <- varsel(mod.ref, nterms_max = n.terms.max)

# plot(mod.var.sel, alpha = 0.05, deltas = TRUE) +
# geom_hline(yintercept = summary(mod.var.sel)$selection$diff[1] * 0.5)

summary(mod.var.sel)
# suggest_size(mod.var.sel, type = "lower", pct = 0.5)

saveRDS(mod.var.sel, file.var.sel)
# mod.var.sel <- readRDS("..results/varsel/sel.1.rds")

# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::cmdstan_make_local(cpp_options = "CXXFLAGS += -ftemplate-depth=2048", append = FALSE)
# cmdstanr::rebuild_cmdstan()

