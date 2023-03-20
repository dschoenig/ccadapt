library(data.table)
library(bnlearn)

source("paths.R")
source("utilities.R")

survey <- readRDS(file.survey.proc)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)



## SELECT OBSERVATIONS AND VARIABLES ##########################################

# For global model: Identify questions that ask for additional information given
# specific responses to other questions

var.omit <-
  c("A43",
    # … see conditioning below.
    "A51",
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




## PREPARE DATA ################################################################

var.bn <- var.sel
bn.data <- survey.sel


# Discretize continuous variables

var.disc <- 
  list(
       A3 = c(0, 25, 50, 100, 1000, max(bn.data$A3)),
       A18 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A19 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A20 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A21 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A22 = c(-.Machine$double.eps, 20, 40, 60, 80, 100)
       # C22 = c(-.Machine$double.eps, 20, 40, 60, 80, 100)
       )

var.disc.labels <- 
  list(
       A3 = c("[0,25]", "(25,50]", "(50,100)", "(100:1000]", ">1000"),
       A18 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A19 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A20 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A21 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A22 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]")
       # C22 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]")
       )

for(i in seq_along(var.disc)) {
  var <- names(var.disc)[i]
  bn.data[[var]] <-
    cut(bn.data[[var]],
            breaks = var.disc[[var]],
            labels = var.disc.labels[[var]],
            ordered_result = TRUE)
}


## STRUCTURAL CONDITIONS #######################################################

## Blacklist

cat.expl <- c("personal_stakes",
               "threat_appraisal",
               "coping_appraisal",
               "control")

# Disallow links from adaptation variables towards other variables (but allow
# the reverse)
bl.adpexpl <- 
  expand.grid(cat1 = "adaptation",
              cat2 = cat.expl) |>
  as.data.table()

bl.adpexpl.l <- list()
for(i in 1:nrow(bl.adpexpl)){
  cond1 <- paste0("category.", bl.adpexpl$cat1[i], "==1")
  cond2 <- paste0("category.", bl.adpexpl$cat2[i], "==1")
  bl.adpexpl.l[[i]] <-
    expand.grid(from = var.bn[eval(parse(text = cond1)), code],
                to = var.bn[eval(parse(text = cond2)), code]) |>
    as.data.table()
}
bl.adpexpl.dt <- rbindlist(bl.adpexpl.l)

arc.blacklist <- as.matrix(bl.adpexpl.dt)

## BN FITTING ##################################################################


bn.init <- bn.fit(empty.graph(names(bn.data)), bn.data)
bn.struct <-
  mmhc(bn.data,
       blacklist = arc.blacklist,
       debug = TRUE)

bn.mod <- bn.fit(bn.struct, bn.data, method = "bayes")

graphviz.plot(bn.mod)

