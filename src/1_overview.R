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

# Choose variables
sel.q <- questions[type != "qualitative" &
                   section != "D" &
                   main != "infoSources" &
                   !main %in% c("forestManager",
                                "delegateMgmtWhy",
                                "delegateMgmtLater",
                                "noMgmtLaterWhy"),
                   name]

sel.n <- character()
for(i in 1:ncol(survey)) {
    ll <- length(unique((survey[[i]])))
    if(ll > 1) {
      sel.n <- c(sel.n, names(survey)[i])
    }
}
sel.x <- intersect(sel.q, sel.n)


# Condition on subpopulation and select variables
sel.obs <- with(survey,
                which(forestManager_SQ004 == "No" & delegateMgmtLater == "Yes"))

survey.x <-
  survey[sel.obs, ..sel.x]

# Reference value for variables that are based on other variables
x.vars <- names(survey.x)
for(i in seq_along(x.vars)) {
  var <- survey.x[[x.vars[i]]] 
  if(any(is.na(var) & is.factor(var))) {
    survey.x[[x.vars[i]]] <- na2baselevel(var, "noanswer")
  }
}

# Replace NAs for estimated impact in terms of surface
survey.x[, CCSurface := na_replace(CCSurface, 0)]

# Create response variables
sel.adaptation <- questions[main == "mgmtChangeACC", name]
y.acc <- matrix(0, nrow = nrow(survey), ncol = length(sel.adaptation))
colnames(y.acc) <- paste0("y.acc.", 1:ncol(y.acc))
for(i in seq_along(sel.adaptation)) {
  y.acc[,i] <-
    as.numeric(survey[[sel.adaptation[i]]] %in%
               c("Yes, within the next 5 years",
                 "Yes, in 6 to 10 years"))
}
y.acc.comb <- rowSums(y.acc)
y.acc.bin <- as.numeric(y.acc.comb > 0)


## VARIABLE SELECTION

# Find 


# Assemble model frame
y.resp <- cbind(y.acc, y.acc.comb, y.acc.bin)
survey.mod <- cbind(y.resp[sel.obs,], survey.x)
colnames(survey.mod) <- paste(colnames(survey.mod), "..")



preds <- paste(names(survey.x), collapse = " + ")
# form <- as.formula(paste("y.acc.comb ~", preds))
form <- as.formula(paste("y.acc.bin ~", preds))

hs <- c(
        # prior(normal(0, 1), class = "intercept"),
        prior(horseshoe(df = 3, par_ratio = 0.1), class = "b")
        # prior(student_t(3, 0, 1), class = "sigma")
        )

# mod.brm <-
#   brm(form, data = survey.mod, family = bernoulli, prior = hs,
#       cores = 4,
#       chains = 4,
#       iter = 10000,
#       warmup = 8000,
#       control = list(adapt_delta = 0.99, max_treedepth = 15)
#       )

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



