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
threshold.fit <- 0
threshold.small <- 500
# mod.id <- 3
# resp.type <- "urgency"
# resp.type <- "willingness"

options(mc.cores = 4)

message(paste0("Response type: `", resp.type, "`"))

if(resp.type == "willingness") {
  file.mod.sel.prefix <- file.mod.sel.w.prefix
  file.var.sel.prefix <- file.var.sel.w.prefix
  file.survey.fit <- file.survey.fit.w
  path.results.varsel <- path.results.w.varsel
}
if(resp.type == "urgency") {
  file.mod.sel.prefix <- file.mod.sel.u.prefix
  file.var.sel.prefix <- file.var.sel.u.prefix
  file.survey.fit <- file.survey.fit.u
  path.results.varsel <- path.results.u.varsel
}


variables <- readRDS(file.variables.proc)
survey.fit <- readRDS(file.survey.fit)

vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

if(mod.id <= length(vars.adapt)) {
  var.resp <- vars.adapt[mod.id]
  vars.pred <- names(survey.fit)[!names(survey.fit) %in% c(vars.adapt, "id")]
  file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
  file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")
} else {
  var.resp <- "Count"
  survey.fit[, Count := apply(.SD, 1, sum, na.rm = TRUE), .SDcols = vars.adapt]
  vars.adapt <- c(vars.adapt, "Count")
  vars.pred <- names(survey.fit)[!names(survey.fit) %in% c(vars.adapt, "id")]
  file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
  file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")
}

nobs.orig <- nrow(survey.fit)
survey.fit <- survey.fit[!is.na(resp),, env = list(resp = var.resp)]


# Exclude variables that do not vary between respondents
vars.fit <- names(survey.fit)
vars.vary <-
  vars.fit[survey.fit[, which(survey.fit[, lapply(.SD, \(x) length(unique(x)))] > 1)]]
vars.pred <- vars.pred[vars.pred %in% vars.vary]
survey.fit <- survey.fit[, ..vars.vary]



## SIMPLIFY FACTOR LEVELS #############################################

# Purely to avoid parsing errors in projpred

vars.recode <- names(survey.fit)
vars.recode <- vars.recode[!vars.recode %in% c(vars.adapt, "id")]

recode.key.l <- list()
for(i in seq_along(vars.recode)) {
  var.foc <- vars.recode[i]
  if(is.factor(survey.fit[[var.foc]])) {
    var.val <- survey.fit[[var.foc]] 
    var.recode <-
      data.table(code = var.foc,
                 level.survey = unique(var.val))
    var.recode[, level.num := as.numeric(level.survey)]
    nlev <- length(levels(var.val))
    var.recode[, level.mod := factor(level.num, levels = as.character(1:nlev))]
    var.val.rc <- as.numeric(var.val)
    survey.fit[,
               var.sel := factor(as.numeric(var.sel), levels = as.character(1:nlev)),
               env = list(var.sel = var.foc)]
    var.recode[, level.survey := as.character(level.survey)]
    recode.key.l[[i]] <- var.recode
  }
}

recode.key <- rbindlist(recode.key.l)
setorder(recode.key, code, level.num)


if(sum(is.na(recode.key$level.mod)) > 0) {
  stop("Variable recoding introduced `NA`s.")
}

nobs.fit <- nrow(survey.fit)

if(nobs.fit > floor(threshold.fit * nobs.orig)) {
  message(paste0("Using ", nobs.fit, " observations"))
} else {
  message(paste0("Not enough observations to fit the model (n = ", nobs.fit, ").\nQuitting …"))
  quit(save = "no", status = 0)
}


## SELECTION MODEL WITH HORSESHOE PRIOR ################################

message(paste0("Performing variable selection for adaptation action `", var.resp, "` …"))
message(paste0("Results will be saved to ", file.mod.sel, " and ", file.var.sel))



# form.sel <- 
#   paste0("resp ~ 1 + (1 | ", paste0(c(vars.pred.cont, vars.pred.cat), collapse = " + "), ")") |>
#   as.formula()

# prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "sd") +
#              prior(normal(0, 3), class = "Intercept")

if(var.resp != "Count") {
  mod.fam <- brmsfamily("bernoulli", "logit")
} else {
  mod.fam <- brmsfamily("poisson")
}

if(nobs.fit > threshold.small) {

  form.sel <- 
    paste0(var.resp, " ~ 1 + ", paste0(vars.pred, collapse = " + ")) |>
    as.formula()

  prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "b") +
               prior(normal(0, 3), class = "Intercept")
  mod.sel <-
    brm(formula = form.sel,
        data = survey.fit,
        family = mod.fam,
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

} else {

  resp.mean <- mean(survey.fit[[var.resp]])

  form.sel <- 
    paste0(var.resp, " ~ 1 + ", paste0(vars.pred, collapse = " + ")) |>
    as.formula()

  int.prior <- paste0("normal(", round(resp.mean, 2), ", 1)")

  prior.sel <- prior(horseshoe(df = 3, par_ratio = 0.1), class = "b") +
               prior_string(int.prior, class = "Intercept")

  mod.sel <-
    brm(formula = form.sel,
        data = survey.fit,
        family = mod.fam,
        silent = 0,
        chains = 4,
        cores = 4,
        # init = 0,
        warmup = 7500,
        iter = 10000,
        thin = 2,
        refresh = 100,
        control = list(adapt_delta = 0.9975,
                       max_treedepth = 12),
        # backend = "cmdstanr",
        prior = prior.sel)

}


dir.create(path.results.varsel, recursive = TRUE, showWarnings = FALSE)

saveRDS(mod.sel, file.mod.sel)
# mod.sel <- readRDS(file.mod.sel)

n.terms.max <- round(0.25 * length(vars.pred))

if(var.resp != "Count") {
  mod.ref <- get_refmodel(mod.sel)
} else {
  mod.ref <- get_refmodel(mod.sel, latent = TRUE)
}


cl <- makeCluster(4)
registerDoParallel(cl)

mod.var.sel <- cv_varsel(mod.ref, nterms_max = n.terms.max, parallel = TRUE)

# mod.var.sel <- cv_varsel(mod.ref, nterms_max = n.terms.max)
# mod.var.sel <- varsel(mod.ref, nterms_max = n.terms.max)

# plot(mod.var.sel, deltas = TRUE, alpha = 0.05)
# plot(mod.var.sel, alpha = 0.05, deltas = TRUE) +
# geom_hline(yintercept = summary(mod.var.sel)$selection$diff[1] * 0.5)

summary(mod.var.sel)
# suggest_size(mod.var.sel, type = "lower", pct = 0.5)

saveRDS(mod.var.sel, file.var.sel)
# mod.var.sel <- readRDS("..results/varsel/sel.1.rds")

# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# cmdstanr::cmdstan_make_local(cpp_options = "CXXFLAGS += -ftemplate-depth=2048", append = FALSE)
# cmdstanr::rebuild_cmdstan()
