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

method <- "forward"
parallel <- TRUE


message(paste0("Response type: `", resp.type, "`"))

if(resp.type == "willingness") {
  file.mod.sel.prefix <- file.mod.sel.w.prefix
  file.var.sel.prefix <- file.var.sel.w.prefix
  path.results.varsel <- path.results.w.varsel
}


## SETUP ###############################################################

variables <- readRDS(file.variables.proc)

vars.adapt <- variables[category.adaptation == TRUE, sort(code)]

var.resp <- vars.adapt[mod.id]
file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")

message(paste0("Performing variable selection for adaptation action `", var.resp, "` …"))
message(paste0("Results will be saved to ", file.var.sel))

mod.sel <- readRDS(file.mod.sel)


summary(mod.sel)

# Full reference model for projection
n.terms.max <- round(0.125 * (length(names(mod.sel$data))-1))
mod.ref <- get_refmodel(mod.sel)


## VARIABLE SELECTION VIA CROSS VALIDATION #############################


cl <- makeCluster(n.threads)
registerDoParallel(cl)


if(cv.type == "loo") {
  mod.var.sel <- cv_varsel(mod.ref, method = method,
                           nterms_max = n.terms.max, parallel = parallel)

}

if(cv.type == "kfold") {
  mod.var.sel <- cv_varsel(mod.ref, method = method,
                           cv_method = "kfold", K = 10,
                           nterms_max = n.terms.max, parallel = parallel)
}

summary(mod.var.sel)

saveRDS(mod.var.sel, file.var.sel)
