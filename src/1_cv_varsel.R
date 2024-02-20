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
nloo <- 10

# mod.id <- 1
# resp.type <- "willingness"
# cv.type <- "loo"
# n.threads <- 4

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
}
if(mod.id == length(vars.adapt) + 1) {
  var.resp <- "Count"
}
if(mod.id == length(vars.adapt) + 2) {
  var.resp <- "Count_fire"
}
file.mod.sel <- paste0(file.mod.sel.prefix, var.resp, ".rds")
file.var.sel <- paste0(file.var.sel.prefix, var.resp, ".rds")


message(paste0("Performing variable selection for adaptation action `", var.resp, "` …"))
message(paste0("Results will be saved to ", file.var.sel))

mod.sel <- readRDS(file.mod.sel)


summary(mod.sel)

n.terms.max <- round(0.125 * (length(names(mod.sel$data))-1))

if(var.resp %in% c("Count", "Count_fire") | resp.type == "categorical") {
  mod.ref <- get_refmodel(mod.sel, latent = TRUE)
} else {
  mod.ref <- get_refmodel(mod.sel)
}


cl <- makeCluster(n.threads)
registerDoParallel(cl)


if(cv.type == "nloo") {
  mod.var.sel <- cv_varsel(mod.ref, method = method,
                           nloo = nloo, nterms_max = n.terms.max, parallel = parallel)
}

if(cv.type == "loo") {

  mod.var.sel <- cv_varsel(mod.ref, method = method,
                           # nterms_max = n.terms.max, parallel = TRUE)
                           nterms_max = n.terms.max, parallel = parallel)

}

if(cv.type == "kfold") {
  mod.var.sel <- cv_varsel(mod.ref, method = method,
                           cv_method = "kfold", K = 10,
                           nterms_max = n.terms.max, parallel = parallel)
}

summary(mod.var.sel)

saveRDS(mod.var.sel, file.var.sel)



# Try to create folds such that each category is present in at least two
# folds (avoids error in k-fold CV). Fails if some categories are only
# present once.
# 
# mod.data <- as.data.table(mod.sel$data)
# mod.data[,.id := 1:.N]
# vars.adapt <- variables[category.adaptation == TRUE, sort(code)]
# vars.cat <- intersect(names(mod.data), variables[type == "categorical", code])
# vars.cat <- vars.cat[vars.cat %notin% vars.adapt]
# k <- 10
# idx.fold.l <- rep(list(NULL), k)
# idx.fold.all <- integer()
# n.obsi <- nrow(mod.data)
# k.n <- integer()
# for(i in seq_along(vars.cat)) {
#   print(i)
#   mod.data.sub <- copy(mod.data)
#   var.lev <- unique(mod.data.sub[[vars.cat[i]]])
#   for(j in seq_along(idx.fold.l)){
#     mod.data.sub[.id %in% idx.fold.l[[j]], fold := j]
#   }
#   cat.sat <- 
#     mod.data.sub[!is.na(fold),
#                  .(n = length(unique(fold))),
#                  by = c(vars.cat[i])
#                  ][n > 1,
#                    as.character(var),
#                    env = list(var = vars.cat[i])]
#   if(length(cat.sat) < length(var.lev)) {
#     mod.data.sub <-
#       mod.data.sub[var %notin% cat.sat,, env = list(var = vars.cat[i])]
#     fold.sam <- sample(1:k, 2)
#     id.fold <-
#         mod.data.sub[,
#                      .(.id = sample(.id, 2), fold = fold.sam),
#                      by = c(vars.cat[i])]
#     for(f in fold.sam) {
#       idx.fold.l[[f]] <- unique(c(idx.fold.l[[f]], id.fold[fold == f, .id]))
#     }
#     idx.fold.all <- unique(c(idx.fold.all, id.fold[, .id]))
#   }
# }

# idx.fold.l <- lapply(idx.fold.l, unique)

# for(j in seq_along(idx.fold.l)){
#   mod.data[.id %in% idx.fold.l[[j]], fold := j]
# }

# test.var <- logical(length(vars.cat))
# for(i in seq_along(vars.cat)) {
#   test.var[i] <-
#     mod.data[,
#              .(n = length(unique(fold))),
#              by = c(vars.cat[i])][, all(n > 1)]
# }
