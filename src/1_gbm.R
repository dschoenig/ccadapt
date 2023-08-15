library(data.table)
library(stringi)
library(h2o)
library(ggplot2)

source("paths.R")
source("utilities.R")

survey <- readRDS(file.survey.proc)
questions <- readRDS(file.questions)
dependencies <- readRDS(file.questions.dependencies)

survey <- readRDS(file.survey.proc)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)


## SELECT OBSERVATIONS AND VARIABLES ##########################################

# For global model: Identify questions that ask for additional information given
# specific responses to other questions

var.omit <-
  c(
    "A39", "A43", "A51",
    # … see conditioning on respondents below.
    variables[main == "infoSources", code],
    # … asked depending on who takes part in decision making.
    variables[main == "plantSelecReasons", code],
    # … only asked if selection criteria changed.
    variables[main == "pastChangeChemicals", code],
    # … specific for respondants not from Québec.
    variables[main == "pastChangeReasons", code],
    # … only asked if changed management in the past.
    variables[main == "futureOpportunWhy", code],
    # … only asked if CC is seen as an opportunity.
    variables[main == "mgmtChangeACCWhyNo", code],
    # … only asked if no intention to implement any measures.
    variables[main == "ACCIntended", code],
    # … only asked if any intention to implement measures.
    variables[main == "ACCEfficiency", code],
    # … only asked if any intention to implement measures.
    variables[main == "CCExperience", code],
    # … only asked if events linked to CC have been observed.
    variables[main == "CCWhen", code],
    # … only asked if future impacts are expected.
    variables[main == "CCSurface", code],
    # … only asked if future impacts are expected.
    variables[main == "professionalForester", code]
    # … asked depending on education level.
  ) 

variables.full <- variables[!(code %in% var.omit)]

# Of these variables, select only variables that have a category assigned

var.sel <- 
  variables.full[!(is.na(category.personal_stakes) |
                   is.na(category.threat_appraisal) |
                   is.na(category.coping_appraisal) |
                   is.na(category.control) |
                   is.na(category.adaptation))]

# Exclude variables that do not vary between respondents

var.include <-
  names(survey)[survey[, which(survey[, lapply(.SD, \(x) length(unique(x)))] > 1)]]

var.sel <- var.sel[code %in% var.include]

# Subset responses to survey
survey.sub <-
  with(survey,
       which(
             A39 == "No" &
             # … only respondants with managed forests.
             A43 == "No" &
             # … only respondants that take part in decision making.
             A51 == "Yes"
             # … only respondants that take part in decision making in the future.
            ))

survey.sel <-
  survey[survey.sub, var.sel$code, with = FALSE]
  # survey[survey.sub, var.sel$code, with = FALSE][, lapply(.SD, \(x) sum(is.na(x)))]


## RESPONSE VARIABLES ##########################################################

# Select response variables
sel.adaptation <- var.sel[main == "mgmtChangeACC", code]


y.acc <- matrix(FALSE, nrow = nrow(survey.sel), ncol = length(sel.adaptation))
colnames(y.acc) <- paste0("y.acc.", 1:ncol(y.acc))
for(i in seq_along(sel.adaptation)) {
  y.acc[,i] <-
    as.logical(survey.sel[[sel.adaptation[i]]] %in%
               c("Yes, within the next 5 years",
                 "Yes, in 6 to 10 years"))
    # as.numeric(survey.sel[[sel.adaptation[i]]] %in%
    #            c("Yes, within the next 5 years",
    #              "Yes, in 6 to 10 years"))
}

y.acc.comb <- rowSums(y.acc)

## PREPARE DATA FOR MODEL ##############################################

mod.preds <- var.sel[! code %in% sel.adaptation, code]

survey.fit <-
  cbind(y.acc.comb,
        y.acc,
        survey.sel)
fwrite(survey.fit, file.survey.fit) 


survey.sel |>
fwrite(file.survey.fit) 

## GRADIENT BOOSTING ###################################################

mod.resp <- "y.acc.1"
# mod.resp <- sel.adaptation[1]

h2o.init(nthreads = 4, max_mem_size = "24G")

survey.train <- h2o.importFile(file.survey.fit)
# survey.splits <-
#   h2o.splitFrame(data = survey.fit,
#                  ratios = c(0.6,0.2),
#                  destination_frames = c("train.hex",
#                                         "valid.hex",
#                                         "test.hex"),
#                  seed = 1234)
# survey.train <- survey.splits[[1]]
# survey.calib <- survey.splits[[2]]
# survey.test  <- survey.splits[[3]]

# survey.fit <- h2o.importFile(file.survey.fit)
# survey.splits <-
#   h2o.splitFrame(data = survey.fit,
#                  ratios = c(0.8),
#                  destination_frames = c("train.hex",
#                                         "test.hex"),
#                  seed = 1234)
# survey.train <- survey.splits[[1]]
# survey.test  <- survey.splits[[2]]


# n.mod <- 100 
# q.modsel <- 0.1 
# n.modsel <- floor(n.mod * q.modsel)

n.mod <- 20
q.modsel <- 0.2
n.modsel <- floor(n.mod * q.modsel)

n.cat.max <-
  max(unlist(lapply(survey.train,
                    \(x) ifelse(is.factor(x),
                                length(levels(x)),
                                0))))

hyper_params = list(
  ## restrict the search to the range of max_depth established above
  max_depth = seq(10, 30, by = 5),
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.1),
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.1),
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.1),
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.05),
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(survey.train))-1,2),
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = seq(2,20,2),
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = seq(2, 8, 1),
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4, 1e-2),
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal")
)

search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",
  ## limit the runtime to 60 minutes
  # max_runtime_secs = 3600,
  ## build no more than 100 models
  max_models = n.mod
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  # stopping_rounds = n.modsel,
  # stopping_metric = "logloss",
  # stopping_tolerance = 1e-3
)

survey.grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  ## which algorithm to run
  algorithm = "gbm",
  ## identifier for the grid, to later retrieve it
  grid_id = "survey_grid",
  ## standard model parameters
  x = mod.preds,
  y = mod.resp,
  training_frame = survey.train,
  nfolds = 5,
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,
  ## learning rate annealing: learning_rate shrinks by 1% after every tree
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,
  # calibrate_model = TRUE,
  # calibration_frame = survey.calib,
  balance_classes = TRUE,
  score_tree_interval = 10,
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-2, stopping_metric = "misclassification",
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234
)

gbm.test <- h2o.gbm(
  x = mod.preds,
  # y = mod.resp,
  y = "y.acc.2",
  training_frame = survey.train,
  nfolds = 5,
  ntrees = 10000,
  learn_rate = 0.05,
  learn_rate_annealing = 0.99,
  balance_classes = TRUE,
  score_tree_interval = 10,
  max_runtime_secs = 3600,
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "misclassification",
  seed = 1234)

h2o.confusionMatrix(gbm.test, xval = TRUE)

rf.test <- h2o.randomForest(
  x = mod.preds,
  y = mod.resp,
  # y = "y.acc.2",
  training_frame = survey.train,
  nfolds = 5,
  ntrees = 1000,
  binomial_double_trees = TRUE,
  score_tree_interval = 10,
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "misclassification",
  seed = 1234)


h2o.confusionMatrix(rf.test, xval = TRUE)

h2o.confusionMatrix(gbm.test, xval = TRUE)
## Sort the grid models by logloss
survey.grid.sort <- h2o.getGrid("survey_grid", sort_by = "logloss", decreasing = FALSE)
# survey.grid.sort <- h2o.getGrid("survey_grid", sort_by = "AUC", decreasing = TRUE)
survey.grid.sort

# gbm.ll <- numeric(0)
# # gbm.auc <- numeric(0)
# for(i in 1:n.mod) {
#   survey.gbm <- h2o.getModel(survey.grid.sort@model_ids[[i]])
#   gbm.ll[[i]] <- h2o.logloss(h2o.performance(survey.gbm, xval = TRUE))
#   # gbm.auc[[i]] <- h2o.auc(h2o.performance(survey.gbm, xval = TRUE))
# }

# gbm.top <- order(gbm.ll, decreasing = FALSE)[1:n.modsel]
# # gbm.top <- order(gbm.auc, decreasing = TRUE)[1:n.modsel]

survey.gbm <- list()
survey.imp <- list()
for(i in 1:n.modsel) {
  mod.path <- paste0(getwd(), "/", path.results.gbm)
  dir.create(mod.path, recursive = TRUE)
  survey.gbm[[i]] <- h2o.getModel(survey.grid.sort@model_ids[[i]])
  survey.imp[[i]] <- h2o.varimp(survey.gbm[[i]])
  h2o.save_mojo(survey.gbm[[i]], path = mod.path, file = paste0("gbm_", i), force = TRUE)
}

saved_model <- h2o.upload_mojo(paste0(mod.path, "gbm_1"))

h2o.confusionMatrix(survey.gbm[[5]])


h2o.auc(h2o.performance(saved_model, xval = TRUE))
h2o.hit_ratio_table(saved_model, xval = TRUE)

survey.imp.dt <-
  rbindlist(lapply(survey.imp, as.data.table), idcol = "model") |>
  DT(, .(importance.mean = mean(scaled_importance)), by = variable)

survey.imp.dt[, importance.rank := frank(-importance.mean)]

survey.imp.dt[order(-importance.mean)][1:20]

var.sel[code %in% survey.imp$variable[1:10]]

survey.gbm@parameters

data(VerbAgg)

unique(VerbAgg$item)

h2o.shutdown()
