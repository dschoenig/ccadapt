args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(brms)
library(stringi)
library(ggplot2)
library(ggdist)
library(colorspace)
library(patchwork)
library(shinystan)
library(DHARMa)
library(bayesplot)

source("paths.R")
source("utilities.R")


resp.type <- as.character(args[1])
resp.type <- "willingness"


if(resp.type == "willingness") {
  file.survey.irt <- file.survey.irt.w
  file.irt.mod.2pl <- file.irt.w.mod.2pl
  path.irt.plots <- path.irt.w.plots
  path.results.irt <- path.results.w.irt
}

file.irt.mod <- file.irt.mod.2pl


## SETUP ###############################################################


# Load model and data

variables <- readRDS(file.variables.proc)
survey.irt <- readRDS(file.survey.irt)
mod.irt <- readRDS(file.irt.mod.2pl)

survey.irt[, .obs := 1:.N]


# Two helper functions to work with posterior draws

as.draws.dt <- function(x, y = NULL, id.name = ".obs", draw.name = ".draw", value.name = "value") {
  xt <- t(x)
  dimnames(xt) <- list(1:nrow(xt), 1:ncol(xt))
  xdt <- 
    as.data.table(xt, keep.rownames = id.name) |>
    melt(id.vars = id.name, variable.name = draw.name, value.name = value.name)
    xdt[, id.col := as.integer(id.col), env = list(id.col = id.name)]
  if(!is.null(y)) {
    xm <- merge(xdt, y, by = id.name)
  }
  return(xm)
}

as.draws.mat <- function(x, draw.var = ".draw", obs.vars = ".obs") {
  cast.form <- as.formula(paste0(draw.var, " ~ ", paste(obs.vars, collapse = " + ")))
  xc <- dcast(x, cast.form)
  xcols <- names(xc)
  xsel <- xcols[!xcols %in% draw.var]
  xmat <- as.matrix(xc[, ..xsel])
  return(xmat)
}


mod.coef <- as_draws_matrix(mod.irt)
post.pred <- posterior_predict(mod.irt)
post.pred.dt <- as.draws.dt(post.pred, survey.irt, value.name = "resp.pred")

survey.sum <- survey.irt[, .(mean = mean(resp)), by = c("id")][order(id)]



## MCMC DIAGNOSTICS ####################################################


post.sum <- summary(mod.coef)
setDT(post.sum)

post.sum[rhat >= 1.01]
post.sum[,
         .(
           ess_bulk.min = min(ess_bulk),
           ess_bulk.max = max(ess_bulk),
           ess_tail.min = min(ess_tail),
           ess_tail.max = max(ess_tail)
           )]


# Inspect with ShinyStan

launch_shinystan(mod.coef)



## GRAPHICAL PPC #######################################################

y <- survey.sum$mean
yhat <-
  post.pred.dt[, mean(resp.pred), by = c(".draw", "id")][order(id, .draw)] |>
  as.draws.mat(draw.var = ".draw", obs.vars = c("id"))

# Inspect histogramss of 100 randomly chosen response vectors
draw.ids <- sample(1:1e4, 100)
ppc_hist(y = y, yrep = yhat[draw.ids,], binwidth = 0.1) +
  theme_default()

# Histograms by response item
draw.ids <- sample(1:1e4, 1000)
pp_check(mod.irt,
         type = "stat_grouped", group = "item",
         stat = "mean", draw_ids = draw.ids,
         binwidth = 0.005) +
  theme_default()



## RESIDUAL DIAGNOSTICS ################################################


post.pred.mean <- apply(post.pred, 2, mean)

dim(post.pred)

mod.res <-
  createDHARMa(simulatedResponse = t(post.pred),
              observedResponse = survey.irt$resp,
              fittedPredictedResponse = post.pred.mean,
              integerResponse = TRUE)

# Residuals vs predicted
plotResiduals(mod.res)

# Residuals by items
plotResiduals(mod.res, survey.irt$item)

# Resdiuals by respondent ID
plotResiduals(mod.res, survey.irt$id, asFactor = TRUE)




