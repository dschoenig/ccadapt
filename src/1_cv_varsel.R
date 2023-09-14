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
cv.type <- as.character(args[3])
n.threads <- as.numeric(args[4])
# mod.id <- 1
# resp.type <- "urgency"
# resp.type <- "willingness"
# resp.type <- "willingness.ord"
# resp.type <- "categorical"
# cv.type <- "loo"
# n.threads <- 4

message(paste0("Response type: `", resp.type, "`"))

if(resp.type == "willingness") {
  file.mod.sel.prefix <- file.mod.sel.w.prefix
  file.var.sel.prefix <- file.var.sel.w.prefix
  path.results.varsel <- path.results.w.varsel
}
if(resp.type == "willingness.ord") {
  file.mod.sel.prefix <- file.mod.sel.wo.prefix
  file.var.sel.prefix <- file.var.sel.wo.prefix
  path.results.varsel <- path.results.wo.varsel
}
if(resp.type == "urgency") {
  file.mod.sel.prefix <- file.mod.sel.u.prefix
  file.var.sel.prefix <- file.var.sel.u.prefix
  path.results.varsel <- path.results.u.varsel
}
if(resp.type == "categorical") {
  file.mod.sel.prefix <- file.mod.sel.c.prefix
  file.var.sel.prefix <- file.var.sel.c.prefix
  path.results.varsel <- path.results.c.varsel
}
if(resp.type == "categorical.sd") {
  file.mod.sel.prefix <- file.mod.sel.cs.prefix
  file.var.sel.prefix <- file.var.sel.cs.prefix
  path.results.varsel <- path.results.cs.varsel
}


variables <- readRDS(file.variables.proc)

vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

if(mod.id <= length(vars.adapt)) {
  var.resp <- vars.adapt[mod.id]
  file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
  file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")
} else {
  var.resp <- "Count"
  vars.adapt <- c(vars.adapt, "Count")
  file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
  file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")
}

mod.sel <- readRDS(file.mod.sel)

n.terms.max <- round(0.25 * (length(names(mod.sel$data))-1))

if(var.resp != "Count") {
  mod.ref <- get_refmodel(mod.sel)
} else {
  mod.ref <- get_refmodel(mod.sel, latent = TRUE)
}

cl <- makeCluster(n.threads)
registerDoParallel(cl)

if(cv.type == "loo") {
  mod.var.sel <- cv_varsel(mod.ref, nterms_max = n.terms.max, parallel = TRUE)
}

if(cv.type == "kfold") {
  mod.var.sel <- cv_varsel(mod.ref, method = "kfold", K = 10, nterms_max = n.terms.max, parallel = TRUE)
}

summary(mod.var.sel)

saveRDS(mod.var.sel, file.var.sel)
