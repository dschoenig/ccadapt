library(data.table)
library(stringi)
library(brms)
library(posterior)
library(bayesplot)

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
    # … see conditioning below.
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


## HORSESHOE MODEL FOR AGGREGATED OBSERVATIONS #################################

preds <- 
  var.sel[! code %in% sel.adaptation, code] |>
  paste(collapse = " + ")
form <- as.formula(paste("y.acc.comb ~", preds))

survey.fit <- cbind(y.acc.comb, survey.sel)

hs <- c(
        # prior(normal(0, 1), class = "intercept"),
        prior(horseshoe(df = 3, par_ratio = 0.1), class = "b")
        # prior(student_t(3, 0, 1), class = "sigma")
        )


mod.brm <-
  brm(form,
      data = survey.fit,
      family = negbinomial,
      prior = hs,
      cores = 4,
      chains = 4,
      iter = 2000,
      warmup = 1000
      # , control = list(adapt_delta = 0.99, max_treedepth = 15)
      )

# saveRDS(mod.brm, "mod.brm.rds")
mod.brm <- readRDS("mod.brm.rds")

summary(mod.brm)


## CATEGORICAL MODEL FOR SINGLE ADAPTATION #####################################

preds <- 
  var.sel[! code %in% sel.adaptation, code] |>
  paste(collapse = " + ")
form.d1 <- as.formula(paste("D1 ~", preds))

hs <- c(
        # prior(normal(0, 1), class = "intercept"),
        prior(horseshoe(df = 3, par_ratio = 0.1), class = "b")
        # prior(student_t(3, 0, 1), class = "sigma")
        )

mod.brm <-
  brm(form.d1,
      data = survey.sel,
      family = categorical,
      prior = hs,
      cores = 4,
      chains = 4,
      iter = 2000,
      warmup = 1000,
      control = list(adapt_delta = 0.99, max_treedepth = 15)
      )

# saveRDS(mod.brm, "mod.brm.rds")
mod.brm <- readRDS("mod.brm.rds")

summary(mod.brm)









para.sum <- as.data.table(summary(as_draws_matrix(mod.brm)))

para.beta <- para.sum[grep("b_", variable)]

para.beta[(q5 > 0 & q95 > 0) | (q5 < 0 & q95 < 0)]

para.beta.top <- para.beta[order(-abs(median))][1:round(0.1 * nrow(para.beta)),]

question.select <-
  sapply(questions$name,
         FUN = \(x) stri_detect_fixed(para.beta.top$variable, x)) |>
  apply(2, any)

questions[question.select,]



