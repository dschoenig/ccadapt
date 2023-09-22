library(data.table)
library(brms)
library(stringi)
library(ggplot2)
library(ggdist)
library(colorspace)
library(patchwork)
library(bayesplot)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)

resp.type <- "willingness"
# resp.type <- "urgency"
cont.nl <- FALSE
# draw.ids <- sample(1:1e4, 100)

if(resp.type == "willingness") {
  file.survey.irt <- file.survey.irt.w
  file.irt.mod.2pl <- file.irt.w.mod.2pl
  path.irt.plots <- path.irt.w.plots
  path.results.irt <- path.results.w.irt
}
if(resp.type == "urgency") {
  file.survey.irt <- file.survey.irt.u
  file.irt.mod.2pl <- file.irt.u.mod.2pl
  path.irt.plots <- path.irt.u.plots
  path.results.irt <- path.results.u.irt
}

if(cont.nl == TRUE) {
  file.irt.mod <- file.irt.mod.2pl.nl
  suffix.nl <- ".nl"
} else {
  file.irt.mod <- file.irt.mod.2pl
  suffix.nl <- ""
}

# file.irt.pred <- paste0(path.results.irt, "predictions.agg.", mar.type, suffix.nl, ".csv")
# file.irt.comp <- paste0(path.results.irt, "comparisons.agg.", mar.type, suffix.nl, ".csv")
# file.irt.pred.ex <- paste0(path.results.irt, "predictions.agg", suffix.nl, ".csv")
# file.irt.comp.ex <- paste0(path.results.irt, "comparisons.agg", suffix.nl, ".csv")


variables <- readRDS(file.variables.proc)
survey.irt <- readRDS(file.survey.irt)
mod.irt <- readRDS(file.irt.mod.2pl)

survey.irt[, .obs := 1:.N]



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



post.pred <- posterior_predict(mod.irt)
post.pred.dt <- as.draws.dt(post.pred, survey.irt, value.name = "resp.pred")

survey.sum <- survey.irt[, .(mean = mean(resp)), by = c("id")][order(id)]


y <- survey.sum$mean
yhat <-
  post.pred.dt[, mean(resp.pred), by = c(".draw", "id")][order(id, .draw)] |>
  as.draws.mat(draw.var = ".draw", obs.vars = c("id"))

draw.ids <- sample(1:1e4, 100)

ppc_hist(y = y, yrep = yhat[draw.ids,], binwidth = 0.1) +
  theme_default()

ppc_dens_overlay(y = y, yrep = yhat[draw.ids,], n_dens = 10) +
  theme_default()


draw.ids <- sample(1:1e4, 1000)

pp_check(mod.irt, type = "stat_grouped", group = "item", stat = "mean", draw_ids = draw.ids, binwidth = 0.005) + theme_default()
