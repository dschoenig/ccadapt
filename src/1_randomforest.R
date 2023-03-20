library(data.table)
library(stringi)
library(ranger)
library(ggplot2)
library(iml)

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
  names(survey.sel)[survey.sel[, which(survey.sel[, lapply(.SD, \(x) length(unique(x)))] > 1)]]

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


y.acc <- matrix(0, nrow = nrow(survey.sel), ncol = length(sel.adaptation))
colnames(y.acc) <- paste0("y.acc.", 1:ncol(y.acc))
for(i in seq_along(sel.adaptation)) {
  y.acc[,i] <-
    as.numeric(survey.sel[[sel.adaptation[i]]] %in%
               c("Yes, within the next 5 years",
                 "Yes, in 6 to 10 years"))
}

y.acc.comb <- rowSums(y.acc)


## QUANTILE RANDOM FOREST ######################################################

preds <- 
  var.sel[! code %in% sel.adaptation, code] |>
  paste(collapse = " + ")
form <- as.formula(paste("y.acc.comb ~", preds))

survey.fit <- cbind(y.acc.comb, survey.sel)

rf <-
  ranger(form,
         data = survey.fit,
         num.trees = 1000,
         # probability = TRUE,
         # quantreg = TRUE,
         importance = "permutation")

var.imp <- importance(rf)

var.imp.dt <-
  data.table(code = names(var.imp), importance = var.imp) |>
  merge(var.sel)

var.imp.dt <- var.imp.dt[order(-importance)]
var.imp.dt[, code := factor(code, levels = rev(code))]
var.imp.dt[, name := factor(name, levels = rev(name))]

ggplot(var.imp.dt[1:50],
       aes(x = name, y= importance,fill=importance)) + 
 geom_bar(stat="identity", position="dodge") +
 coord_flip() +
 ylab("Variable Importance") +
 xlab("") +
 guides(fill=F) +
 scale_fill_viridis_c()

library(iml)

predictor <- Predictor$new(rf, data = survey.sel, y = y.acc.comb)


# Plot age effect
ale <- FeatureEffect$new(predictor, feature = "F11")
ale <- ale$results
ale$F11 <- factor(ale$F11, levels = levels(survey.sel$F11))
ggplot(ale, aes(x = F11, y = .value)) +
  geom_bar(stat = "identity")


# Plot logging effect
ale <- FeatureEffect$new(predictor, feature = "B38")
ale <- ale$results
ale$B38 <- factor(ale$B38, levels = levels(survey.sel$B38))
ggplot(ale, aes(x = B38, y = .value)) +
  geom_bar(stat = "identity") +
  labs(title = var.sel[code == "B38", question.main],
       subtitle = var.sel[code == "B38", question.sub])


ggplot(survey.fit, aes(x = B38 , y = y.acc.comb)) +
  geom_violin()


ale <- FeatureEffect$new(predictor, feature = "F14")
ale <- ale$results
ale$F11 <- factor(ale$F14, levels = levels(survey.sel$F14))
ggplot(ale, aes(x = F14, y = .value)) +
  geom_bar(stat = "identity") +
  labs(title = var.sel[code == "F14", question.main],
       subtitle = var.sel[code == "F14", question.sub])

ale <- FeatureEffect$new(predictor, method = "ice", feature = "B38")
ale <- ale$results
ale$B38 <- factor(ale$B38, levels = levels(survey.sel$B38))
ggplot(ale, aes(x = B38, y = .value, group = .id)) +
  geom_line() +
  labs(title = var.sel[code == "B38", question.main],
       subtitle = var.sel[code == "B38", question.sub])
