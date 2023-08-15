library(data.table)
library(brms)
library(stringi)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)




## ITEM-RESPONSE MODELS ################################################





var.cat <- var.sel[code %in% vars.pred & type == "categorical", .(code, cat.ref)]
for(i in 1:nrow(var.cat)) {
  survey.irt[[var.cat$code[i]]] <-
    relevel(survey.irt[[var.cat$code[i]]], var.cat$cat.ref[i])
}

var.cont <- var.sel[code %in% vars.pred & type == "continuous", .(code, cat.ref)]
# var.cont <- var.sel[type == "continuous", .(code, cont.mean, cont.sd)]
if(nrow(var.cont) > 0) {
  for(i in 1:nrow(var.cont)) {
    survey.irt[[var.cont$code[i]]] <-
      (survey.irt[[var.cont$code[i]]] - var.cont$cont.mean[i]) /
      var.cont$cont.sd[i]
  }
}

survey.irt[, id := 1:.N]

survey.irt <-
  melt(survey.irt,
       measure.vars = items,
       variable.name = "item",
       value.name = "resp")

form.imp.1pl <-
  paste0("resp ~ ",
         paste0(vars.pred, collapse = " + "),
         " + (1 + ", paste0(vars.pred, collapse = " + "), " | item)",
         " + (1 | id)") |>
  as.formula() |>
  bf()

prior.imp.1pl <-
  prior("student_t(3, 0, 2.5)",class = "b") +
  prior("normal(0, 3)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

# form.test <-
#   paste0("resp ~ ",
#          paste0(vars.pred, collapse = " + ")) |>
#   as.formula()


# str(model.matrix(form.test, survey.irt))

# library(lme4)
# gmod <- glmer(form.imp.1pl,
#               data = survey.irt,
#               verbose = 1,
#               family = binomial)

mod.imp.1pl <-
  brm(formula = form.imp.1pl,
      data = survey.irt,
      family = brmsfamily("bernoulli", "logit"),
      init = 0,
      silent = 0,
      refresh = 1,
      prior = prior.imp.1pl)


saveRDS(mod.imp.1pl, paste0(path.results.brm, "mod.imp.1pl.rds"))


b.imp <-
  fixef(mod.imp.1pl, summary = FALSE) |>
  as.data.table(keep.rownames = "draw") |>
  melt(id.vars = "draw",
       variable.factor = FALSE,
       variable.name = "effect",
       value.name = "pop")

b.names <- unique(b.imp$effect)

ran.item <- ranef(mod.imp.1pl, pars = b.names, summary = FALSE)$item

ran.imp.l <- list()
for(i in seq_along(b.names)) {
  ran.imp.l[[i]] <-
    ran.item[,,b.names[i]] |>
    as.data.frame() |>
    as.data.table(keep.rownames = "draw") |>
    melt(id.vars = "draw", variable.factor = FALSE, variable.name = "item", value.name = "group")
}
names(ran.imp.l) <- b.names

ran.imp <- rbindlist(ran.imp.l, idcol = "effect")

effects.item <- merge(b.imp, ran.imp, by = c("draw", "effect"))
effects.item[, draw := as.numeric(draw)]

effects.item[, comb := (pop + group)]

effects.item.sum <- 
  effects.item[, .(mean = mean(comb),
                   q5 = quantile(comb, 0.025),
                   q95 = quantile(comb, 0.975)),
                by = c("item", "effect")]

# "I have tried to increase the diversity of species in my forests"
effects.item.sum[effect == "B2Yes"]

# Do you identify as First Nation, Métis or Inuit?
effects.item.sum[effect == "F14Yes"]

# Hunting, trapping or fishing
effects.item.sum[sign(q5) == sign(q95) & effect %like% "A31"]

effects.item.sum[sign(q5) == sign(q95) & effect != "Intercept"]


library(DHARMa)

mod.imp.1pl.check <- createDHARMa(
  simulatedResponse = t(posterior_predict(mod.imp.1pl)),
  observedResponse = survey.irt$resp,
  fittedPredictedResponse = apply(t(posterior_epred(mod.imp.1pl)), 1, mean),
  integerResponse = TRUE)

plot(mod.imp.1pl.check)

plotResiduals(mod.imp.1pl.check, form = as.character(survey.irt$F14))
