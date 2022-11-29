library(data.table)
library(bnlearn)

source("paths.R")
source("utilities.R")

survey <- readRDS(file.survey.proc)
questions <- readRDS(file.questions)
dependencies <- readRDS(file.questions.dependencies)

var.bn <-
  list(latent = 
         data.table(name = c("personal_stakes", 
                             "threat_appraisal",
                             "coping_appraisal"),
                    category.personal_stakes = c(1, 0, 0),
                    category.threat_appraisal = c(0, 1, 0),
                    category.coping_appraisal = c(0, 0, 1),
                    category.control = 0, 
                    category.adaptation = 0),
       observed = questions[,
                            .(name,
                              category.personal_stakes,
                              category.threat_appraisal,
                              category.coping_appraisal,
                              category.control,
                              category.adaptation)]) |>
  rbindlist(idcol = "type", fill = TRUE) |>
  na.omit()


## STRUCTURAL CONDITIONS #######################################################

## Blacklist

cat.observed <- c("personal_stakes",
                  "threat_appraisal",
                  "coping_appraisal",
                  "control",
                  "adaptation")

var.latent <- var.bn[type == "latent", name]


# Only allow links inside certain explanatory categories
bl.observed <- 
  expand.grid(cat1 = cat.observed,
              cat2 = cat.observed) |>
  as.data.table()
bl.observed <- bl.observed[!(cat1 == cat2 & cat1 %in% cat.observed[1:3])]

bl.observed.l <- list()
for(i in 1:nrow(bl.observed)){
  cond1 <- paste0("category.", bl.observed$cat1[i], "==1")
  cond2 <- paste0("category.", bl.observed$cat2[i], "==1")
  bl.observed.l[[i]] <-
    expand.grid(from = var.bn[eval(parse(text = cond1)), name],
                to = var.bn[eval(parse(text = cond2)), name]) |>
    as.data.table()
}
rbindlist(bl.observed.l)

# Disallow links from latent variable towards variables of the same category
# (but allow the reverse)
bl.latobs <-
  data.table(lat = var.latent,
              cat = var.latent)
bl.latobs.l <- list()
for(i in 1:nrow(bl.latobs)){
  cond <- paste0("category.", bl.latobs$cat[i], "==1")
  bl.latobs.l[[i]] <-
    data.table(from = bl.latobs$lat[i],
               to = var.bn[eval(parse(text = cond)), name])
}
rbindlist(bl.latobs.l)

# Combine blacklists

arc.blacklist <- rbindlist(c(bl.observed.l, bl.latobs.l))

## Whitelist

# Force links from latent variables to adaptation variables
wl.latadp <-
  data.table(lat = var.latent,
             cat = "adaptation")
wl.latadp.l <- list()
for(i in 1:nrow(wl.latadp)){
  cond <- paste0("category.", wl.latadp$cat[i], "==1")
  wl.latadp.l[[i]] <-
    data.table(from = wl.latadp$lat[i],
               to = var.bn[eval(parse(text = cond)), name])
}
rbindlist(wl.latadp.l)

# Force links from control variables to adaptation variables
wl.conadp <-
  data.table(cat1 = "control",
             cat2 = "adaptation")
wl.conadp.l <- list()
for(i in 1:nrow(wl.conadp)){
  cond1 <- paste0("category.", wl.conadp$cat1[i], "==1")
  cond2 <- paste0("category.", wl.conadp$cat2[i], "==1")
  wl.conadp.l[[i]] <-
    expand.grid(from = var.bn[eval(parse(text = cond1)), name],
                to = var.bn[eval(parse(text = cond2)), name]) |>
    as.data.table()
}
rbindlist(wl.conadp.l)

arc.whitelist <- rbindlist(c(wl.latadp.l, wl.conadp.l))


## STRUCTURE LEARNING ##########################################################

