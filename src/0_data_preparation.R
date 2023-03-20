library(data.table)
library(stringi)

source("paths.R")

# Cleaning steps that have already been done by Timothée (email from 2022-04-26):
  # *   Suppression du répondants id = 152, 157, 159, 405, 410 (car ce sont des tests que j’ai faits)
  # *   Suppression du répondant id = 32 (car il a répondu avoir 5,000,000 ha... Plusieurs données étranges : quelqu'un qui dépend à 100% de la forêt, qui a tous ses boisés en co-propriété, qui s'identifie comme peuple autochtone, qui a des postures très environnementales, ...). Je dirais que c'est quelqu'un qui a répondu un peu sur un mode "activisme environnemental".
  # *   Retiré les colonnes inutiles (ex : Seed, « date initiated », « date finished », « introA », « introB », etc.)
  # *   Variable landlord, répondant id = 700 : j'ai transformé une mauvaise réponse à « Do you own woodlots in Canada ? » de Yes > No, car les réponses suivantes étaient bonnes
  # *   Variables woodlotSize (pour ceux qui ont seulement 1 parcelle) et woodlotSizeTotal (pour ceux qui ont plusieurs parcelles) : conversion des acres en hectares (ha = acres/2,471), des pieds carrés en ha (ha = square feet/2,471) et des arpents en hectares (ha = arpent/2,925). Fusion des variables woodlotSize et woodlotSizeTotal en une variable unique woodlotSize.
  # *   Fusion des variables coOwnership et coOwnershipMultiPlots pour obtenir coOwnership
  # *   Rectification des variables forestManager « I_decide_everything_by_myself » = yes en « I_decide_everything_by_myself » = no pour les répondants 19 37 41 47 50 52 54 56 69 85 90 96 101 104 107 118 123 131 134 154 167 173 175 178 195 205 207 213 214 220 221 239 248 250 274 280 281 285 294 297 302 308 312 324 329 359 363 389 436 440 446 459 472 473 478 485 506 524 526 533 534 541 545 549 564 569 578 582 624 627 629 640 644 695 703 704 705 706 708 724 732 743 745 756 757 767 791 793 796 799 805 811 814 832 834 849 856 883 893 901 905 908 926 937 939 946 953 962 968 974 977 984 987 1004 qui répondaient aussi que « I_make_decisions_together_with_people_close_to_me_(relatives,_friends,_other_co-owners » = yes et/ou que « I_seek_advice_from_a_forestry_professional » = yes
  # *   Suppression des variables "time" chronométrant le temps mis à répondre à chaque question (colonnes HV et suivantes)

survey.raw <- fread(file.survey.raw)

# Relevant columns for analysis
q.idx <- 5:208

# Qualitative variables
var.qual <-
  c("forestReasonsExplain",
    "forestManager[other]",
    "commentsA",
    "pastChangePlantSelec[other]",
    "pastChangeReasonsOth",
    "futureOpportunWhy",
    "commentsB",
    "commentsD",
    "climateExperience[other]",
    "CCWhen[other]",
    "commentsC",
    "ACCLeadership[other]",
    "commentsE",
    "surveySource[SQ002comment]",
    "surveySource[SQ003comment]",
    "surveySource[SQ004comment]",
    "surveySource[SQ005comment]",
    "surveySource[other]",
    "surveySource[othercomment]",
    "gender[other]",
    "education[other]",
    "job[other]",
    "woodlotVisit[other]",
    "commentsFinal")

# Continuous variables
var.cont <-
  c("woodlotSize",
    "plotAgeDistrib",
    "CCSurface")

# Categorical variables
var.cat <-
  c("landlord" = "a",
    "woodlotsNumber" = "b",
    "coOwnership" = "c",
    "provinces" = "a",
    "forestReasons" = "d",
    "forestManager" = "a",
    "delegateMgmtWhy" = "d",
    "delegateMgmtLater" = "e",
    "noMgmtLaterWhy" = "d",
    "infoSources" = "d",
    "pastChangeRegen" = "f",
    "pastChangeSpecies" = "a",
    "pastChangePlantSelec" = "a",
    "plantSelecReasons" = "g",
    "pastChangeCut" = "h",
    "pastChangeChemicals" = "a",
    "pastChangeConservati" = "h",
    "pastChangeReasons" = "d",
    "future" = "i",
    "mgmtChangeACC" = "j",
    "mgmtChangeACCWhyNo" = "d",
    "ACCIntended" = "d",
    "ACCEfficiency" = "k",
    "ACCAssistance" = "l",
    "climateKnower" = "i",
    "climateExperience" = "a",
    "CCExperience" = "m",
    "CCImpacts" = "n",
    "CCWhen" = "o",
    "ACCLeadership" = "a",
    "ACCSocial" = "m",
    "surveySource" = "a",
    "age" = "p",
    "gender" = "q",
    "autochtonous" = "r",
    "woodlotMoney" = "s",
    "education" = "t",
    "professionalForester" = "a",
    "job" = "u",
    "woodlotVisit" = "v")

cat.levels <-
  list(a = c("No",
             "Yes"),
       b = c("1",
             "2",
             "3-5",
             "6-10",
             "More than 10"),
       c = c("No",
             "Yes",
             "Yes, some of my woodlots",
             "Yes, all of my woodlots"),
       d = c("Not at all important",
             "Not important",
             "Rather not important",
             "Neutral",
             "Rather important",
             "Important",
             "Extremely important"),
       e = c("No",
             "Yes",
             "I do not know"),
       f = c("I have not changed anything.",
             "I have increased the share of natural regeneration.",
             "I have increased the share of planting."),
       g = c("No",
             "Yes",
             "I do not remember"),
       h = c("It decreased",
             "It did not change",
             "It increased",
             "It depends on woodlots"),
       i = c("Absolutely disagree",
             "Disagree",
             "Somewhat disagree",
             "Neutral",
             "Somewhat agree",
             "Agree",
             "Absolutely agree"),
       j = c("No",
             "Unlikely",
             "Yes, within the next 5 years",
             "Yes, in 6 to 10 years",
             "I do not know this action"),
       k = c(
             "Absolutely ineffective",
             "Ineffective",
             "Somehow ineffective",
             "Uncertain efficacy",
             "Somehow effective",
             "Effective",
             "Very effective"),
       l = c(
             "Not at all needed",
             "Not needed",
             "Somewhat not needed",
             "Neutral",
             "Somewhat needed",
             "Needed",
             "Definitely needed"),
       m = c(
             "Strongly disagree",
             "Disagree",
             "Somewhat disagree",
             "Neither agree or disagree",
             "Somewhat agree",
             "Agree",
             "Strongly agree"),
       n = c(
             "Not at all",
             "No",
             "Rather no",
             "Neutral",
             "Rather yes",
             "Yes",
             "Yes, I’m really sure"),
       o = c("In more than 50 years",
             "In 30 to 49 years",
             "In 10 to 29 years",
             "In 0 to 9 years",
             "They are already apparent",
             "Other"),
       p = c("18-29 years old",
             "30-39 years old",
             "40-49 years old",
             "50-59 years old",
             "60-69 years old",
             "70 years old or older",
             "I prefer not to answer"
             ),
       q = c(
             "Female",
             "Male",
             "I prefer not to answer",
             "Other"),
       r = c("No",
             "Yes",
             "I prefer not to answer"),
       s = c("0 %",
             "1 to 10 %",
             "11 to 30 %",
             "31 to 50 %",
             "51 to 75 %",
             "76 to 100 %",
             "I prefer not to answer"),
       t = c("Primary school or equivalent",
             "Secondary school or equivalent",
             "Vocational school or equivalent",
             "University degree or equivalent",
             "I prefer not to say",
             "Other"),
       u = c("No professional activity",
             "Retired",
             "Primary sector (agriculture, livestock, fishing, forestry, mining or oil extraction, etc.)",
             "Secondary sector (food-processing, construction, production of goods and equipment, etc.)",
             "Tertiary sector (commerce, administration, education, health, etc.)",
             "Other",
             "I prefer not to answer"),
       v = c("Never",
             "Once every five years",
             "Once a year",
             "Several times a year",
             "Every month",
             "Every week",
             "Every day",
             "Other"))

cat.ord <- c("b", "d", "i", "k", "l", "m", "n") 


variables <- fread(file.variables)

variables <- variables[id %in% q.idx]

variables[type == "categorical",
          cat.scale := var.cat[main]]
variables[type == "categorical",
          cat.ord := cat.scale %in% cat.ord]

setcolorder(variables, c("id", "code", "section",
                         "name", "main", "sub",
                         "type", "cat.scale", "cat.ord",
                         "category.personal_stakes",
                         "category.threat_appraisal",
                         "category.coping_appraisal",
                         "category.control",
                         "category.adaptation",
                         "question.main", "question.sub"))

# deal with NAs and dependent questions
dependencies <-
  data.table(
    main =
      c("delegateMgmtWhy", "noMgmtLaterWhy", "infoSources",
        "pastChangeRegen", "pastChangeSpecies", "pastChangePlantSelec",
        "plantSelecReasons", "pastChangeCut", "pastChangeChemicals",
        "pastChangeConservati", "pastChangeReasons", "pastChangeReasonsOth",
        "futureOpportunWhy", "commentsB",
        unique(variables[section == "D", main]),
        "mgmtChangeACCWhyNo", "ACCIntended", "ACCEfficiency",
        "CCExperience", "CCWhen", "CCSurface",
        "professionalForester"),
    dep.main = 
      c("forestManager", "delegateMgmtLater", "forestManager",
        "forestManager", "forestManager", "forestManager",
        "pastChangePlantSelec", "forestManager", "provinces",
        "forestManager", "forestManager", "forestManager",
        "future", "forestManager", 
        rep("delegateMgmtLater", length(unique(variables[section == "D", main]))),
        "mgmtChangeACC", "mgmtChangeACC", "mgmtChangeACC", 
        "climateExperience", "CCImpacts", "CCImpacts", 
        "education"),
    dep.sub =
      c("SQ004", NA, NA,
        NA, NA, NA,
        "SQ003", NA, "Quebec",
        NA, NA, NA,
        "SQ006", NA,
        rep(NA, length(unique(variables[section == "D", main]))),
        NA, NA, NA,
        NA, NA, NA,
        NA),
    dep.other =
      c(NA, NA, NA,
        NA, NA, NA,
        NA, NA, NA,
        NA, "Other `pastChange` questions", "Other `pastChange` questions",
        NA, NA,
        rep(NA, length(unique(variables[section == "D", main]))),
        NA, NA, NA,
        NA, NA, NA,
        NA)
    )


survey <-
  copy(survey.raw[, variables$id, with = FALSE]) |>
  setnames(variables$code)


# Variables types

codes.qual <- variables[type == "qualitative", code]
for(i in seq_along(codes.qual)) {
  survey[[codes.qual[i]]] <- as.character(survey[[codes.qual[i]]])
  empty.idx <- which(survey[[codes.qual[i]]] == "") 
  survey[[codes.qual[i]]][empty.idx] <- NA
}

codes.cont <- variables[type == "continuous", code]
for(i in seq_along(codes.cont)) {
  survey[[codes.cont[i]]] <- as.numeric(survey[[codes.cont[i]]])
}

codes.cat <- variables[type == "categorical", code]
scales.cat <- variables[type == "categorical", cat.scale]
for(i in seq_along(codes.cat)) {
  survey[[codes.cat[i]]] <-
    factor(as.character(survey[[codes.cat[i]]]),
           levels = cat.levels[[scales.cat[i]]],
           ordered = variables[code == codes.cat[i], cat.ord])
}



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


saveRDS(survey, file.survey.proc)
saveRDS(variables, file.variables.proc)
saveRDS(dependencies, file.questions.dependencies)

