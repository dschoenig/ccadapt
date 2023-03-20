library(data.table)
library(bnlearn)

source("paths.R")
source("utilities.R")

survey <- readRDS(file.survey.proc)
variables <- readRDS(file.variables.proc)
dependencies <- readRDS(file.questions.dependencies)




## PREPARE DATA ################################################################

# Variables definition

var.bn <- 
  variables[!(is.na(category.personal_stakes) |
              is.na(category.threat_appraisal) |
              is.na(category.coping_appraisal) |
              is.na(category.control) |
              is.na(category.adaptation))]
var.bn[, latent := FALSE]

var.latent <- c("personal_stakes", 
                "threat_appraisal",
                "coping_appraisal",
                "control")
var.bn <- rbind(data.table(id = NA,
                           latent = "TRUE",
                           code = paste0("L", 1:4),
                           name = var.latent,
                           type = "categorical",
                           cat.scale = "latent",
                           cat.ord = TRUE,
                           category.personal_stakes = c(1, 0, 0, 0),
                           category.threat_appraisal = c(0, 1, 0, 0),
                           category.coping_appraisal = c(0, 0, 1, 0),
                           category.control = c(0, 0, 0, 1), 
                           category.adaptation = 0),
                var.bn, fill = TRUE)

code.latent <- var.bn[latent == TRUE, code]
scale.latent <- as.character(1:5)

# Prepare data for fitting

bn.data <-
  copy(survey[, var.bn[latent == FALSE, code], with = FALSE])


# Discretize continuous variables

var.disc <- 
  list(
       A3 = c(0, 25, 50, 100, 1000, max(bn.data$A3)),
       A18 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A19 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A20 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A21 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       A22 = c(-.Machine$double.eps, 20, 40, 60, 80, 100),
       C22 = c(-.Machine$double.eps, 20, 40, 60, 80, 100)
       )

var.disc.labels <- 
  list(
       A3 = c("[0,25]", "(25,50]", "(50,100)", "(100:1000]", ">1000"),
       A18 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A19 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A20 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A21 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       A22 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]"),
       C22 = c("[0,20]", "(20,40]", "(40,60]", "(60,80]", "(80,100]")
       )

for(i in seq_along(var.disc)) {
  var <- names(var.disc)[i]
  bn.data[[var]] <-
    cut(bn.data[[var]],
            breaks = var.disc[[var]],
            labels = var.disc.labels[[var]],
            ordered_result = TRUE)
}

# Include latent variables

bn.data[, (code.latent) := NA]
bn.data[,
        (code.latent) := lapply(.SD, \(x) factor(x,
                                                 levels = scale.latent,
                                                 ordered = TRUE)),
        .SDcols = code.latent]
      

## STRUCTURAL CONDITIONS #######################################################

## Blacklist

cat.observed <- c("personal_stakes",
                  "threat_appraisal",
                  "coping_appraisal",
                  "control",
                  "adaptation")

var.latent <- var.bn[latent == TRUE, name]

# Only allow links within certain explanatory categories, exclude links within
# the rest, and between all categories
bl.observed <- 
  expand.grid(cat1 = cat.observed,
              cat2 = cat.observed) |>
  as.data.table()
bl.observed <- bl.observed[!(cat1 == cat2 & cat1 %in% cat.observed[1:4])]
bl.observed.l <- list()
for(i in 1:nrow(bl.observed)){
  cond1 <- paste0("latent == FALSE & category.", bl.observed$cat1[i], "==1")
  cond2 <- paste0("latent == FALSE & category.", bl.observed$cat2[i], "==1")
  bl.observed.l[[i]] <-
    expand.grid(from = var.bn[eval(parse(text = cond1)), code],
                to = var.bn[eval(parse(text = cond2)), code]) |>
    as.data.table()
}
bl.observed.dt <- rbindlist(bl.observed.l)

# Disallow all incoming links to latent variables
bl.obslat.dt <-
  expand.grid(from = var.bn[latent == FALSE, code],
              to = var.bn[latent == TRUE, code]) |>
  as.data.table()

# Disallow links from latent variables towards observed variables (including latent) that are not
# in the same category
bl.latobs <-
  expand.grid(lat = var.latent,
              cat = cat.observed) |>
  as.data.table()
bl.latobs <- bl.latobs[as.character(lat) != as.character(cat)]
bl.latobs.l <- list()
for(i in 1:nrow(bl.latobs)){
  cond <- paste0("category.", bl.latobs$cat[i], "==1")
  bl.latobs.l[[i]] <-
    data.table(from = var.bn[latent == TRUE & name == bl.latobs$lat[i], code],
               to = var.bn[eval(parse(text = cond)), code])
}
bl.latobs.dt <- rbindlist(bl.latobs.l)

# Combine blacklists

arc.blacklist <- 
  rbindlist(list(bl.observed.dt, bl.latobs.dt, bl.obslat.dt))[from != to] |>
  unique() |>
  as.matrix()


## Whitelist

# # Force links from latent variables to adaptation variables
# wl.latadp <-
#   data.table(lat = var.latent,
#              cat = "adaptation")
# wl.latadp.l <- list()
# for(i in 1:nrow(wl.latadp)){
#   cond <- paste0("category.", wl.latadp$cat[i], "== 1")
#   wl.latadp.l[[i]] <-
#     data.table(from = var.bn[latent == TRUE & name == wl.latadp$lat[i], code],
#                to = var.bn[eval(parse(text = cond)), code])
# }
# wl.latadp.dt <- rbindlist(wl.latadp.l)

# Force links from latent variables to observed variables of same category
wl.latobs <-
  data.table(lat = var.latent,
             cat = var.latent)
wl.latobs.l <- list()
for(i in 1:nrow(wl.latobs)){
  cond <- paste0("latent == FALSE & category.", wl.latobs$cat[i], "== 1")
  wl.latobs.l[[i]] <-
    data.table(from = var.bn[latent == TRUE & name == wl.latobs$lat[i], code],
               to = var.bn[eval(parse(text = cond)), code])
}
wl.latobs.dt <- rbindlist(wl.latobs.l)


arc.whitelist <- 
  rbindlist(list(wl.latobs.dt)) |>
  unique() |>
  as.matrix()


## STRUCTURE LEARNING ##########################################################

# Learn structure of compartments

cat.fit <- c("personal_stakes",
             "threat_appraisal",
             "coping_appraisal")


bn.struct.cat <- list()

for(i in seq_along(cat.fit)) {
  # i=1
  cond.cat <-
    parse(text = paste0("latent == FALSE & category.", cat.fit[i], " == 1"))
  bn.data.cat <- bn.data[, var.bn[eval(cond.cat), code], with = FALSE]
  bn.struct.cat[[i]] <-
    structural.em(bn.data.cat,
                  maximize = "tabu",
                  maximize.args = list(score = "bic"),
                  fit = "bayes",
                  impute = "bayes-lw",
                  debug = TRUE)
}

lapply(bn.data, \(x) sum(is.na(x)))

var.bn[code == "A51"]

arcs.init <- do.call(rbind, lapply(bn.struct.cat, arcs))

bn.init <- empty.graph(names(bn.data))
arcs(bn.init) <- arcs.init

bn.fit(bn.init, data = bn.data, method = "hdir")

graphviz.plot(bn.struct.cat[[i]])

bn.data.init <- copy(bn.data)
bn.data.init[,
             (code.latent) := lapply(.SD,
                                     \(x) factor(sample(levels(x),
                                                        nrow(bn.data.init),
                                                        replace = TRUE),
                                                 levels = levels(x),
                                                 ordered = TRUE)),
             .SDcols = code.latent]

bn.init <- bn.fit(empty.graph(names(bn.data)), bn.data.init)
bn.struct <-
  structural.em(bn.data,
                maximize = "tabu",
                start = bn.init,
                maximize.args = list(whitelist = arc.whitelist,
                                     blacklist = arc.blacklist),
                fit = "bayes",
                impute = "bayes-lw",
                debug = TRUE)

which(unlist(lapply(bn.data, is.numeric)))



summary(bn.data)



graphviz.plot(bn.init)

bn.struct.cat

names(bn.data)

bn.data.init <- copy(bn.data)
code.latent <- var.bn[latent == TRUE, code]
bn.data[, (code.latent) := as.numeric(NA)]
bn.data.init[, (code.latent) := 0]


# Initialization

bn.empty <- empty.graph(nodes = var.bn$code)
bn.init <- bn.fit(bn.empty, bn.data.init)

bn.net(bn.init)

?bnlearn:::check.args.against.assumptions

bn.struct <-
  structural.em(bn.data,
                start = bn.init,
                maximize.args = list(blacklist = arc.blacklist),
                debug = TRUE)

arc.whitelist

survey[, lapply(.SD, \(x) sum(is.na(x)))]

bnlearn:::check.arcs.against.assumptions

dag.r <- random.graph(var.bn$code, method = "melancon")

# Apply blacklist
for(i in 1:nrow(arc.blacklist)) {
  dag.r <- drop.arc(dag.r,
                   arc.blacklist[i, 1], arc.blacklist[i, 2])
}


for(i in 1:nrow(arc.blacklist)) {
  dag.r <- drop.arc(dag.r,
                   arc.blacklist[i, 1], arc.blacklist[i, 2])
}

for(i in 1:nrow(arc.whitelist)) {
  dag.r <- set.arc(dag.r,
                   arc.whitelist[i, 1], arc.whitelist[i, 2])
}


bn.init <- bn.fit(dag.r, bn.data.init)


data(learning.test)
# learn with incomplete data.
incomplete.data = learning.test
incomplete.data[1:100, 1] = NA
incomplete.data[101:200, 2] = NA
incomplete.data[1:200, 5] = NA
structural.em(incomplete.data)
## Not run:
# learn with a latent variable.
incomplete.data = learning.test
incomplete.data[seq(nrow(incomplete.data)), 1] = NA
start = bn.fit(empty.graph(names(learning.test)), learning.test)
wl = data.frame(from = c("A", "A"), to = c("B", "D"))
lrnd <- structural.em(incomplete.data, start = start,
maximize.args = list(whitelist = wl), debug = TRUE)
## End(Not run)

lrnd




str(learning.test)


res = cpdag(model2network("[A][C][F][B|A][D|A:C][E|B:F]"))
res

## use debug = TRUE to get more information.
updated = set.arc(res, "A", "B")
