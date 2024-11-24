args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(stringi)
library(brms)
library(projpred)
library(doParallel)
library(doRNG)


source("paths.R")
source("utilities.R")

mod.id <- as.integer(args[1])
resp.type <- as.character(args[2])
n.threads <- as.numeric(args[3])

# PARAMETERS
# `dim.poly`:
#     Degree of orthogonal polynomials for Likert items and continuous
#     variables.
# `threshold.fit`:
#     Minimum number of observations required for model fit.

dim.poly <- 3
threshold.fit <- 0


options(mc.cores = n.threads)

message(paste0("Response type: `", resp.type, "`"))

if(resp.type == "willingness") {
  file.mod.sel.prefix <- file.mod.sel.w.prefix
  file.var.sel.prefix <- file.var.sel.w.prefix
  file.survey.fit <- file.survey.fit.w
  path.results.varsel <- path.results.w.varsel
}


## SETUP ###############################################################


variables <- readRDS(file.variables.proc)
survey.fit <- readRDS(file.survey.fit)
cat.levels <- readRDS(file.cat.levels.proc)

vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

if(mod.id <= length(vars.adapt)) {
  var.resp <- vars.adapt[mod.id]
  vars.pred <- names(survey.fit)[!names(survey.fit) %in% c(vars.adapt, "id")]
} 
file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")

nobs.orig <- nrow(survey.fit)
survey.fit <- survey.fit[!is.na(resp),, env = list(resp = var.resp)]


# Exclude variables that do not vary between respondents
vars.fit <- names(survey.fit)
vars.vary <-
  vars.fit[survey.fit[, which(survey.fit[, lapply(.SD, \(x) length(unique(x)))] > 1)]]
vars.pred <- vars.pred[vars.pred %in% vars.vary]
survey.fit <- survey.fit[, ..vars.vary]


# Order Likert scales ascending

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
}


## HANDLE CATEGORICAL PREDICTORS ######################################

# Purely to avoid parsing errors in projpred

vars.recode <- names(survey.fit)
vars.recode <- vars.recode[!vars.recode %in% c(vars.adapt, "id")]

recode.key.l <- list()
for(i in seq_along(vars.recode)) {
  var.foc <- vars.recode[i]
  if(is.factor(survey.fit[[var.foc]])) {
    var.val <- survey.fit[[var.foc]] 
    var.ord <- is.ordered(var.val)
    var.recode <-
      data.table(code = var.foc,
                 level.survey = unique(var.val))
    var.recode[, level.num := as.numeric(level.survey)]
    nlev <- length(levels(var.val))
    var.recode[, level.mod := factor(letters[level.num], levels = letters[1:nlev], ordered = var.ord)]
    survey.fit[,
               var.sel := factor(letters[as.numeric(var.sel)],
                                 levels = letters[1:nlev], ordered = var.ord),
               env = list(var.sel = var.foc)]
    var.recode[, level.survey := as.character(level.survey)]
    recode.key.l[[i]] <- var.recode
  }
}

recode.key <- rbindlist(recode.key.l[!sapply(recode.key.l, is.null)], fill = TRUE)
setorder(recode.key, code, level.num)


if(sum(is.na(recode.key$level.mod)) > 0) {
  stop("Variable recoding introduced `NA`s.")
}


# Set polynomial contrasts for Likert scales

for(i in seq_along(vars.pred.lik)) {
  contrasts(survey.fit[[vars.pred.lik[i]]], how.many = dim.poly) <- contr.poly(7)
}



nobs.fit <- nrow(survey.fit)

if(nobs.fit > floor(threshold.fit * nobs.orig)) {
  message(paste0("Using ", nobs.fit, " observations"))
} else {
  message(paste0("Not enough observations to fit the model (n = ", nobs.fit, ").\nQuitting …"))
  quit(save = "no", status = 0)
}



## SELECTION MODEL WITH HORSESHOE PRIOR ################################

message(paste0("Fitting model for adaptation action `", var.resp, "` …"))
message(paste0("Results will be saved to ", file.mod.sel))


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

# form.sel <- 
#   paste0("resp ~ 1 + (1 | ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), ")") |>
#   as.formula()

# prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "sd") +
#              prior(normal(0, 3), class = "Intercept")


mod.fam <- brmsfamily("bernoulli", "logit")

form.sel <- 
  paste0(var.resp,
          " ~ 1 + ",
          paste0(vars.pred.all, collapse = " + ")) |>
  as.formula()
prior.sel <-
  prior(horseshoe(df = 3, par_ratio = 0.1), class = "b") +
  prior(normal(0, 3), class = "Intercept")

names(survey.fit)


mod.sel <-
  brm(formula = form.sel,
      data = survey.fit,
      family = mod.fam,
      silent = 0,
      chains = 4,
      cores = 4,
      # threads = 1,
      threads = floor(n.threads / 4),
      warmup = 7500,
      iter = 10000,
      thin = 2,
      refresh = 100,
      control = list(adapt_delta = 0.99),
      # backend = "cmdstanr",
      # empty = TRUE,
      prior = prior.sel)


dir.create(path.results.varsel, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.sel, file.mod.sel)