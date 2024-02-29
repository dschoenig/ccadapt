library(data.table)
library(brms)
library(stringi)
library(ggplot2)
library(ggdist)
library(colorspace)
library(patchwork)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)

resp.type <- "willingness"
dim.poly <- 3
# resp.type <- "urgency"
# pred.scales <- c("prob", "linpred")
pred.scales <- c("prob")
mar.type <- "cf"
cont.pred.n <- 21
cont.diff.frac <- 100
slope.res <- "fine"
var.log <- c("A03")
draw.ids <- NULL
ci.et.width <- 0.9
q.ci.l <- (1-ci.et.width)/2
q.ci.u <- 1-q.ci.l


# # For testing
# cont.pred.n <- 5
# slope.res <- "coarse"
# draw.ids <- sample(1:1e4, 100)

if(resp.type == "willingness") {
  file.survey.fit <- file.survey.fit.w
  file.survey.irt <- file.survey.irt.w.fire
  file.irt.mod.2pl <- file.irt.w.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.w.mod.2pl.nl.fire
  path.irt.plots <- path.irt.w.plots
  path.results.irt <- path.results.w.irt
}
if(resp.type == "urgency") {
  file.survey.fit <- file.survey.fit.w
  file.survey.irt <- file.survey.irt.u.fire
  file.irt.mod.2pl <- file.irt.u.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.u.mod.2pl.nl.fire
  path.irt.plots <- path.irt.u.plots
  path.results.irt <- path.results.u.irt
}


file.irt.pred.all <- paste0(path.results.irt, "predictions.all.", mar.type, ".fire.rds")
file.irt.pred <- paste0(path.results.irt, "predictions.", mar.type, ".fire.csv")
file.irt.comp <- paste0(path.results.irt, "comparisons.", mar.type, ".fire.csv")
file.irt.pred.ex <- paste0(path.results.irt, "predictions", ".fire.csv")
file.irt.comp.ex <- paste0(path.results.irt, "comparisons", ".fire.csv")

if(file.exists(file.irt.pred.all)) {
pred.var.all <- readRDS(file.irt.pred.all)
}

variables <- readRDS(file.variables.proc)
cat.levels <- readRDS(file.cat.levels.proc)

survey.fit <- readRDS(file.survey.fit)
survey.irt <- readRDS(file.survey.irt)
mod.irt <- readRDS(file.irt.mod.2pl)


base.size <- 9
base.family <- "IBMPlexSansCondensed"

plot_theme <-
  theme_light(base_family = base.family,
              base_size = base.size) +
  theme(
        plot.title = element_text(hjust = 0,
                                  face = "bold",
                                  margin = margin(l = 0, b = base.size/3, t = base.size/3)),
        plot.tag = element_text(face = "bold"),
        axis.line.x = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.line.y = element_line(color = "black",
                                   linewidth = rel(0.5)),
        axis.title.x = element_text(margin = margin(t = base.size/2)),
        axis.title.y = element_text(margin = margin(r = base.size/2)),
        axis.text.y = element_text(color = "black", size = rel(1)),
        axis.text.x = element_text(color = "black"),
        legend.title = element_text(margin = margin(b = base.size/3)),
        legend.position = "right",
        legend.justification = "center",
        legend.key.size = unit(base.size, "pt"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.spacing.x = unit(base.size, "pt"),
        panel.spacing.y = unit(base.size/2, "pt"),
        plot.margin = margin(3, 3, 3, 3),
        strip.text = element_text(size = rel(0.8),
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2)),
        strip.background = element_rect(fill = "gray90", colour = NA))

guide_fill <-
  guides(fill = guide_bins(key.width = 3,
                           key.height = 3))



# Polynomial contrasts for Likert scales


vars.adapt <- variables[category.adaptation == TRUE, sort(code)]
vars.pred.all <- variables[code %notin% vars.adapt &
                       code %in% names(survey.fit),
                       code]

vars.pred.lik <-
  variables[code %in% vars.pred.all & cat.ord == TRUE &
            cat.scale %in% c("d", "i", "l", "n", "m"),
            code]
vars.pred.cont <-
  variables[code %in% vars.pred.all &
            type == "continuous",
            code]
vars.pred.cat <-
  variables[code %in% vars.pred.all &
            type == "categorical" &
            cat.ord == FALSE,
            code]


for(i in seq_along(vars.pred.lik)) {
  var.scale <- variables[code == vars.pred.lik[i], cat.scale]
  var.lev <- cat.levels[cat.scale == var.scale][order(level.id), level]
  survey.fit[,
             var.ord := factor(var.ord,
                               levels = var.lev, 
                               ordered = TRUE),
             env = list(var.ord = vars.pred.lik[i])]
  contrasts(survey.fit[[vars.pred.lik[i]]], how.many = dim.poly) <- contr.poly(7)
}
vars.pred.lik.poly <- character()
for(i in seq_along(vars.pred.lik)) {
  var.lik <- survey.fit[[vars.pred.lik[i]]]
  cont.mat <- contrasts(var.lik)[var.lik,1:dim.poly]
  poly.names <- paste0(vars.pred.lik[i], "_p", 1:dim.poly)
  vars.pred.lik.poly <- c(vars.pred.lik.poly, poly.names)
  survey.fit[, (poly.names) := as.data.table(cont.mat)]
}
vars.pred.cont.poly <- character()
for(i in seq_along(vars.pred.cont)) {
  var.cont <- survey.fit[[vars.pred.cont[i]]]
  poly.mat <- poly(var.cont, degree = dim.poly)
  poly.names <- paste0(vars.pred.cont[i], "_p", 1:dim.poly)
  vars.pred.cont.poly <- c(vars.pred.cont.poly, poly.names)
  survey.fit[, (poly.names) := as.data.table(poly.mat)]
}

poly.names <- c("L", "Q", "C")
var.codes.poly <-
  CJ(code = c(vars.pred.lik, vars.pred.cont),
     poly.id = 1:3)
var.codes.poly[, code.poly := paste0(code, "_p", poly.id)]
var.codes.poly[, poly.name := poly.names[poly.id]]
var.codes.poly[, code.name := paste0(code, " ", poly.name)]



vars.irt.poly <- setdiff(names(survey.irt), c("id", "resp", "item"))

vars.irt <-
  c(var.codes.poly[code.poly %in% vars.irt.poly, unique(code)],
    vars.irt.poly[vars.irt.poly %notin% var.codes.poly$code.poly]) |>
  sort()


vars.pred <-
  variables[code %in% vars.irt,
            .(code,
              type, cat.scale, cat.ref, cat.ord, cont.mean, cont.sd,
              category.personal_stakes, category.threat_appraisal,
              category.coping_appraisal, category.control)
            ]

items.ref <- levels(survey.irt$item)
items.ref <- factor(items.ref, levels = items.ref)
names(items.ref) <- items.ref
# items.code <- variables[category.adaptation == 1, code]
# items.code <- factor(items.code, levels = items.code)
# names(items.code) <- items.ref



vars.cat <-
  vars.pred[type == "categorical",
            .(id.mar = 0,
              code, cat.ref)] |>
  dcast(id.mar ~ code, value.var = "cat.ref")
vars.cont <- 
  vars.pred[type == "continuous",
            .(id.mar = 0, code, cont.mean)] |>
  dcast( id.mar ~ code, value.var = "cont.mean") |>
  _[, -"id.mar"]
vars.ref <-
  cbind(vars.cat, vars.cont) |>
  merge(CJ(id.mar = 0,
           item = factor(items.ref, levels = items.ref)),
        by = "id.mar")


pred.all.l <- list()
slope.all.l <- list()
pred.sum.l <- list()
comp.sum.l <- list()
n.sum.l <- list()

for(p in seq_along(pred.scales)) {
  if(pred.scales[p] == "prob") plot.type <- "prob"
  if(pred.scales[p] == "linpred") plot.type <- "lp"
  plot.file <- paste0(path.irt.plots, plot.type, ".", mar.type, ".fire.pdf")
  cairo_pdf(plot.file, onefile = TRUE, width = 8.5, height = 11)
}

# vars.irt <- vars.irt[1:2]
# vars.irt <- "A03"
# vars.irt <- "F15"
# vars.irt <- "A22"
# vars.irt <- c("A03", "A06", "A22")
# vars.irt <- c("A03", "F15")
# vars.irt <- c("A03", "F21")
# survey.irt <- survey.irt[id %in% sample(1:.N, 100)]

for(i in seq_along(vars.irt)) {
  
  message(paste0("Processing variable ", i, "/", length(vars.irt), " …"))

  var.foc <- vars.irt[i]
  var.type <- variables[code == var.foc, type]
  var.ord <- variables[code == var.foc, cat.ord]

  if(var.type == "continuous") {
    var.mean <- variables[code == var.foc, cont.mean]
    var.sd <- variables[code == var.foc, cont.sd]
    if(var.foc %in% var.log) {
      var.org <-  var.mean + (var.sd * survey.fit[[var.foc]])
      var.lev <- exp(seq(log(min(var.org)),
                         log(max(var.org)),
                         length.out = cont.pred.n))
      var.lev <- (var.lev - var.mean) / var.sd
    } else {
    var.lev <- seq(min(survey.irt[[var.foc]]),
                   max(survey.irt[[var.foc]]),
                   length.out = cont.pred.n)
    }
  }
  if(var.type == "categorical") {
    var.lev <- 
      survey.fit[,
                 factor(levels(var.sel),
                        levels = levels(var.sel),
                        ordered = var.ord),
                 env = list(var.sel = var.foc)]
  }


  if(mar.type == "cf") {
    var.dt <- 
      survey.irt[rep(1:.N, times = length(var.lev))]
    var.dt[,
           var.sel := rep(var.lev, each = nrow(survey.irt)),
           env = list(var.sel = var.foc)]
  }

  if(mar.type == "mem") {
    var.dt <-
      vars.ref[rep(1:.N, times = length(var.lev)),
               -var.sel,
               env = list(var.sel = I(var.foc))]
    var.dt[,
           var.sel := rep(var.lev, each = nrow(vars.ref)),
           env = list(var.sel = var.foc)]
    var.dt[, id := NA]
  }

  var.dt[,
         `:=`(obs = 1:.N,
              id.mar = i,
              code.mar = var.foc)]

  # Handle contrasts for predictions

  if(var.foc %in% vars.pred.lik) {
    contrasts(var.dt[[var.foc]], how.many = dim.poly) <- contr.poly(7)
    var.lik <- var.dt[[var.foc]]
    cont.mat <- contrasts(var.lik)[var.lik,1:dim.poly]
    poly.names <- paste0(var.foc, "_p", 1:dim.poly)
    var.dt[, (poly.names) := as.data.table(cont.mat)]
  }

  if(var.foc %in% vars.pred.cont) {
    poly.coefs <- attr(poly(survey.fit[[var.foc]], 3), "coefs")
    var.cont <- var.dt[[var.foc]]
    cont.mat <- poly(var.cont, degree = 3, coefs = poly.coefs)
    poly.names <- paste0(var.foc, "_p", 1:dim.poly)
    var.dt[, (poly.names) := as.data.table(cont.mat)]
  }

  if(file.exists(file.irt.pred.all)) {

    pred.var <- pred.var.all$pred[code.mar == var.foc]

  } else {

    pred.var.l <- list()

    # Predictions per level

    message("Generating predictions …")

    for(j in seq_along(var.lev)) {
      # For loop to keep memory footprint lower

      message(paste0("Level ", j , "/", length(var.lev), " …"))

      var.lev.foc <- var.lev[j]
      var.lev.dt <- var.dt[var.sel == var.lev.foc,
                           env = list(var.sel = var.foc)]

      if(mar.type == "mem") {
        vars.re <- names(mod.irt$data)
        vars.re <- vars.re[vars.re %in% variables$code]
        re.form <-
          paste0("~ (1 + ", paste0(vars.re, collapse = " + "), " | item)") |>
          as.formula()
          pred.mod <- t(posterior_epred(mod.irt,
                                        newdata = var.lev.dt,
                                        re_formula = re.form,
                                        draw_ids = draw.ids))
          pred.mod.lp <- t(posterior_linpred(mod.irt,
                                             newdata = var.lev.dt,
                                             re_formula = re.form,
                                             draw_ids = draw.ids))
      }
      if(mar.type == "cf") {
        pred.mod <- t(posterior_epred(mod.irt,
                                      newdata = var.lev.dt,
                                      draw_ids = draw.ids))
        pred.mod.lp <- t(posterior_linpred(mod.irt,
                                           newdata = var.lev.dt,
                                           draw_ids = draw.ids))
      }

      colnames(pred.mod) <- paste0("draw_", 1:ncol(pred.mod))
      colnames(pred.mod.lp) <- paste0("draw_", 1:ncol(pred.mod))

      pred.mod.obs <-
        as.data.table(as.data.frame(pred.mod)) |>
        _[, obs := var.lev.dt$obs] |>
        melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
             value.name = "prob")
      pred.mod.lp.obs <-
        as.data.table(as.data.frame(pred.mod.lp)) |>
        _[, obs := var.lev.dt$obs] |>
        melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
             value.name = "linpred")

      pred.var.lev <-
        merge(pred.mod.obs, pred.mod.lp.obs, by = c("obs", "draw")) |>
        merge(var.lev.dt[,.(obs, item.code = item)], by = "obs")

      pred.var.lev[, draw := as.integer(draw)]
      pred.var.lev[, code.mar := var.foc]

      if(var.type == "continuous") {
        pred.var.lev[,
                     lev := var.lev.foc,
                     env = list(var.sel = var.foc)]
      }
      if(var.type == "categorical") {
        pred.var.lev[,
                     lev := factor(var.lev.foc,
                                   levels = var.lev,
                                   ordered = var.ord),
                     env = list(var.sel = var.foc)]
      }

      pred.var.l[[j]] <-
        pred.var.lev[,
                     .(
                       linpred = mean(linpred),
                       prob = mean(prob)),
                     by = c("draw", "item.code", "code.mar", "lev")]

      rm(pred.mod.obs, pred.mod.lp.obs, var.lev.dt, pred.var.lev)
      gc()

    }

    pred.var <- rbindlist(pred.var.l)

    rm(pred.var.l)
    gc()
    } # End if no pred.var.all 


  if(var.type == "categorical") {

    message("Comparing levels …")

    # Comparison between levels

    var.lev <-
          cat.levels[cat.scale == vars.pred[code == var.foc, cat.scale]
                     ][order(level.id), level]
    
    pred.var[, lev := factor(lev, levels = var.lev)]

    var.comb <- 
      CJ(lev1 = factor(var.lev, levels = var.lev),
         lev2 = factor(var.lev, levels = var.lev))
    var.comb <- var.comb[lev1 != lev2]
    var.comb[, comp.id := 1:.N]
   
    var.comp.l <- list()
    for(j in 1:nrow(var.comb)) {

      var.lev.comp <-
        merge(pred.var[lev ==  var.comb[j, lev1],
                       .(item.code, draw, lev1 = lev,
                         prob1 = prob, linpred1 = linpred)],
              pred.var[lev ==  var.comb[j, lev2],
                       .(item.code, draw, lev2 = lev,
                         prob2 = prob, linpred2 = linpred)],
              by = c("item.code", "draw"))

      var.comp.l[[j]] <-
        var.lev.comp[,
                     .(code.mar = var.foc,
                       lev1 = var.comb[j, lev1],
                       lev2 = var.comb[j, lev2],
                       prob.diff.median = median(prob1 - prob2),
                       prob.diff.ci.l = quantile(prob1 - prob2, q.ci.l),
                       prob.diff.ci.u = quantile(prob1 - prob2, q.ci.u),
                       linpred.diff.median = median(linpred1 - linpred2),
                       linpred.diff.ci.l = quantile(linpred1 - linpred2, q.ci.l),
                       linpred.diff.ci.u = quantile(linpred1 - linpred2, q.ci.u),
                       p.diff.pos = sum(prob1 > prob2)/.N,
                       p.diff.neg = sum(prob1 < prob2)/.N),
                     by = "item.code"]

    }

    var.comp <- rbindlist(var.comp.l, idcol = "comp.id")
    setorder(var.comp, item.code, comp.id)
  } # End categorical


  if(var.type == "continuous") {

    message("Calculating slopes (central difference) …")

    if(file.exists(file.irt.pred.all)) {

      slope.var <- pred.var.all$slope[code.mar == var.foc]

    } else {

      if(slope.res == "coarse") {

        message("Coarse approximation …")

        slope.var <-
          pred.var[order(item.code, draw, lev),
                   .(code.mar,
                     lev,
                     prob,
                     lev.bw = lev[c(1, 1:(.N-1))],
                     lev.fw = lev[c(2:.N, .N)],
                     prob.bw = prob[c(1, 1:(.N-1))],
                     prob.fw = prob[c(2:.N, .N)],
                     linpred.bw = linpred[c(1, 1:(.N-1))],
                     linpred.fw = linpred[c(2:.N, .N)]
                     ),
                   by = c("item.code", "draw")]

        slope.var[,
                  `:=`(prob.slope = (prob.fw - prob.bw) / (lev.fw - lev.bw),
                       linpred.slope = (linpred.fw - linpred.bw) / (lev.fw - lev.bw))]


      } # End coarse

      if(slope.res == "fine") {

        message("Fine approximation …")

        diff.h <-
          (max(survey.irt[[var.foc]]) - min(survey.irt[[var.foc]])) / cont.diff.frac


        slope.var.l <- list()

        for(j in seq_along(var.lev)) {
          # For loop to keep memory footprint lower

          message(paste0("Level ", j , "/", length(var.lev), " …"))

          var.lev.foc <- var.lev[j]

          var.lev.dt <- 
            var.dt[var.sel == var.lev.foc,
                   env = list(var.sel = var.foc)]

          var.lev.fw <- copy(var.lev.dt)
          var.lev.bw <- copy(var.lev.dt)

          var.lev.fw[,
                     var.sel := var.lev.foc + diff.h/2,
                     env = list(var.sel = var.foc)]
          var.lev.bw[,
                     var.sel := var.lev.foc - diff.h/2,
                     env = list(var.sel = var.foc)]


          if(mar.type == "mem") {
            vars.re <- names(mod.irt$data)
            vars.re <- vars.re[vars.re %in% variables$code]
            re.form <-
              paste0("~ (1 + ", paste0(vars.re, collapse = " + "), " | item)") |>
              as.formula()
            pred.mod.fw <- t(posterior_epred(mod.irt,
                                             newdata = var.lev.fw,
                                             re_formula = re.form,
                                             draw_ids = draw.ids))
            pred.mod.bw <- t(posterior_epred(mod.irt,
                                             newdata = var.lev.bw,
                                             re_formula = re.form,
                                             draw_ids = draw.ids))
            pred.mod.lp.fw <- t(posterior_linpred(mod.irt,
                                                  newdata = var.lev.fw,
                                                  re_formula = re.form,
                                                  draw_ids = draw.ids))
            pred.mod.lp.bw <- t(posterior_linpred(mod.irt,
                                                  newdata = var.lev.bw,
                                                  re_formula = re.form,
                                                  draw_ids = draw.ids))
          }
          if(mar.type == "cf") {
            pred.mod.fw <- t(posterior_epred(mod.irt,
                                             newdata = var.lev.fw,
                                             draw_ids = draw.ids))
            pred.mod.bw <- t(posterior_epred(mod.irt,
                                             newdata =  var.lev.bw,
                                             draw_ids = draw.ids))
            pred.mod.lp.fw <- t(posterior_linpred(mod.irt,
                                                  newdata = var.lev.fw,
                                                  draw_ids = draw.ids))
            pred.mod.lp.bw <- t(posterior_linpred(mod.irt,
                                                  newdata =  var.lev.bw,
                                                  draw_ids = draw.ids))
          }

          pred.mod.prob <- (pred.mod.fw - pred.mod.bw) / diff.h
          colnames(pred.mod.prob) <- paste0("draw_", 1:ncol(pred.mod.prob))

          pred.mod.linpred <- (pred.mod.lp.fw - pred.mod.lp.bw) / diff.h
          colnames(pred.mod.linpred) <- paste0("draw_", 1:ncol(pred.mod.linpred))

          slope.var.lev.prob <-
            as.data.table(as.data.frame(pred.mod.prob)) |>
            _[, obs := var.lev.dt$obs] |>
            melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
                 value.name = "prob.slope") |>
            merge(var.lev.dt[,.(obs, item)])

          slope.var.lev.linpred <-
            as.data.table(as.data.frame(pred.mod.linpred)) |>
            _[, obs := var.lev.dt$obs] |>
            melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
                 value.name = "linpred.slope") |>
            merge(var.lev.dt[,.(obs, item)])

          slope.var.lev <- merge(slope.var.lev.linpred,
                                 slope.var.lev.prob,
                                 by = c("obs", "draw", "item"))


          slope.var.lev[, draw := as.integer(draw)]
          slope.var.lev[, item.code := items.ref[as.character(item)]]
          slope.var.lev[, code.mar := var.foc]

          slope.var.lev[,
                       lev := var.lev.foc,
                       env = list(var.sel = var.foc)]

          slope.var.l[[j]] <-
            slope.var.lev[,
                         .(prob.slope = mean(prob.slope),
                           linpred.slope = mean(linpred.slope)),
                         by = c("draw", "item.code", "code.mar", "lev")]

          gc()

        }

        slope.var <- rbindlist(slope.var.l)

      } # End fine

    } # End if no pred.var.all

      var.comp <-
        slope.var[,
                  .(prob.slope.median = median(prob.slope),
                    prob.slope.ci.l = quantile(prob.slope, q.ci.l),
                    prob.slope.ci.u = quantile(prob.slope, q.ci.u),
                    linpred.slope.median = median(linpred.slope),
                    linpred.slope.ci.l = quantile(linpred.slope, q.ci.l),
                    linpred.slope.ci.u = quantile(linpred.slope, q.ci.u),
                    p.diff.pos = sum(prob.slope > 0)/.N,
                    p.diff.neg = sum(prob.slope < 0)/.N),
                  by = c("item.code", "code.mar", "lev")]

  } # End continuous


  var.comp[p.diff.pos + p.diff.neg < 1,
           `:=`(p.diff.pos = p.diff.pos + (1 - (p.diff.pos + p.diff.neg)) / 2,
                p.diff.neg = p.diff.neg + (1 - (p.diff.pos + p.diff.neg)) / 2)]

  # Summarize for subsequent export


  if(var.type == "categorical") {

    setorder(pred.var, item.code, code.mar, lev, draw)
    setorder(var.comp, item.code, code.mar, lev1, lev2)

    var.n <-
      melt(survey.fit,
           measure.vars = vars.adapt,
           variable.name = "item") |>
      _[,
        .(n = .N),
        by = c("item", var.foc)
        ][item %in% names(items.ref),
          .(code.mar = var.foc,
            item.code = items.ref[as.character(item)],
            lev = var.sel,
            n),
          env = list(var.sel = var.foc)
          ]
    var.n[, lev := factor(lev, levels = levels(pred.var$lev))]
    setorder(var.n, code.mar, item.code, lev)

    n.sum.l[[i]] <- var.n
    

    pred.sum.l[[i]] <-
      pred.var[,
               .(
                 linpred.median = median(linpred),
                 linpred.q5 = quantile(linpred, 0.05),
                 linpred.q25 = quantile(linpred, 0.25),
                 linpred.q75 = quantile(linpred, 0.75),
                 linpred.q95 = quantile(linpred, 0.95),
                 prob.median = median(prob),
                 prob.q5 = quantile(prob, 0.05),
                 prob.q25 = quantile(prob, 0.25),
                 prob.q75 = quantile(prob, 0.75),
                 prob.q95 = quantile(prob, 0.95)),
               by = c("code.mar", "item.code", "lev")
               ]

    comp.sum.l[[i]] <-
      var.comp[,
               .(code.mar,
                 item.code,
                 lev1,
                 lev2,
                 prob.diff.median,
                 prob.diff.ci.l,
                 prob.diff.ci.u,
                 linpred.diff.median,
                 linpred.diff.ci.l,
                 linpred.diff.ci.u,
                 p.diff.pos,
                 p.diff.neg)]

    pred.all.l[[i]] <- pred.var

  }


  if(var.type == "continuous") {

    pred.var[, lev := (lev*var.sd) + var.mean]
    slope.var[, lev := (lev*var.sd) + var.mean]
    var.comp[, lev := (lev*var.sd) + var.mean]

    setorder(pred.var, item.code, code.mar, lev, draw)
    setorder(slope.var, item.code, code.mar, lev, draw)
    setorder(var.comp, item.code, code.mar, lev1, lev2)

    var.n <-
      melt(survey.fit,
           measure.vars = vars.adapt,
           variable.name = "item") |>
      _[,
        .(var.sel,
          item,
          lev = (var.lev[which.min(abs(var.sel - var.lev))] * var.sd) + var.mean),
        by = .I,
        env = list(var.sel = var.foc)
        ][,
          .(n = .N),
          by = c("item", "lev")
          ][,
            .(code.mar = var.foc,
              item.code = items.ref[as.character(item)],
              lev,
              n),
            ]

    n.sum.l[[i]] <- var.n

    pred.sum.l[[i]] <-
      pred.var[order(code.mar, item.code, lev),
               .(
                    linpred.median = median(linpred),
                    linpred.q5 = quantile(linpred, 0.05),
                    linpred.q25 = quantile(linpred, 0.25),
                    linpred.q75 = quantile(linpred, 0.75),
                    linpred.q95 = quantile(linpred, 0.95),
                    prob.median = median(prob),
                    prob.q5 = quantile(prob, 0.05),
                    prob.q25 = quantile(prob, 0.25),
                    prob.q75 = quantile(prob, 0.75),
                    prob.q95 = quantile(prob, 0.95)),
               by = c("code.mar", "item.code", "lev")
               ]

    comp.sum.l[[i]] <- var.comp

    pred.all.l[[i]] <- pred.var
    slope.all.l[[i]] <- slope.var

  }

  # Prepare plots

  message("Creating plots …")

  for(p in seq_along(pred.scales)) {
   
    dev.set(dev.list()[[p]])

    pred.scale <- pred.scales[p]
 
    if(resp.type == "willingness") {
      resp.desc <- "Willingness"
      fill.desc <- "Certainty that\nwillingness\nincreases"
    }
    if(resp.type == "urgency") {
      resp.desc <- "Urgency"
      fill.desc <- "Certainty that\nurgency\nincreases"
    }

    if(var.type == "categorical") {

      item.lev <- as.character(sort(unique(pred.var$item)))

      pred.p <- list()
      prob.p <- list()
      # ref.line <- list()
      # plim <- list()

      if(pred.scale == "prob") {
        pred.var[, pred.plot := prob]
      }
      if(pred.scale == "linpred") {
        pred.var[, pred.plot := linpred]
      }

      var.cat.ref <- variables[code == var.foc, cat.ref]

      for(k in seq_along(item.lev)) {

        item.foc <- item.lev[k]

        # item.foc.code <- items.code[as.character(item.foc)]

        item.desc <- 
          variables[code == item.foc, paste0(code, ": ", question.sub)] |>
          stri_wrap(width = 42) |>
          paste(collapse = "\n")

        if(pred.scale == "prob") {
          pred.desc <- paste0(resp.desc, " to adapt\n(probability)")
        }
        if(pred.scale == "linpred") {
          pred.desc <- paste0(resp.desc, " to adapt\n(log odds)")
        }

        q.main <-
          variables[code == var.foc,
                    paste0("Main question: ", question.main)] |>
          stri_wrap(width = 96)
        q.sub <-
          variables[code == var.foc,
                    paste0("Sub-question or option: ", question.sub)] |>
          stri_wrap(width = 96)
        if(variables[code == var.foc, main] == "provinces") {
          q.sub <-
            variables[code == var.foc,
                      paste0("Sub-question or option: ", sub)] |>
            stri_wrap(width = 96)
        }
        var.desc <- paste(c(q.main, q.sub), collapse = "\n")

        ref.line <-
          pred.var[item.code == item.foc & lev == var.cat.ref,
                   median(pred.plot)]
        
        plim <-
          pred.var[item.code == item.foc,
                   .(q.l = quantile(pred.plot, 0.01),
                     q.u = quantile(pred.plot, 0.99)),
                   by = "lev"
                   ][,
                     c(min(q.l), q.u = max(q.u))]

        ylab <-
          var.n[item.code == item.foc,
                paste0(lev, " (", n, ")")]

        pred.p[[k]] <-
          ggplot(pred.var[item.code == item.foc]) +
            stat_halfeye(aes(x = pred.plot, y = lev),
                              .width = c(0.5, 0.9),
                              fill = "grey80",
                              scale = 0.8) +
            geom_vline(xintercept = ref.line, linewidth = 0.3, linetype = "dashed") + 
            coord_fixed(ratio = (plim[2] - plim[1])/length(var.lev),
                        xlim = plim) +
            scale_y_discrete(labels = ylab) +
            labs(y = NULL, x = pred.desc,
                 subtitle = item.desc) +
            plot_theme +
            theme(plot.margin = margin(base.size, base.size, base.size, base.size))


        prob.p[[k]] <-
          ggplot(var.comp[item.code == item.foc]) +
          geom_tile(aes(y = lev1, x = lev2, fill = p.diff.pos), na.rm = TRUE) +
          geom_point(data = var.comp[item.code == item.foc &
                                        (p.diff.pos > 0.95 | p.diff.pos < 0.05)],
                     aes(y = lev1, x = lev2)) +
          scale_fill_binned_divergingx("Roma", rev = TRUE, mid = 0.5,
                                       breaks = c(0,0.01, 0.05, 0.1, 0.25, 0.5,
                                                  0.75, 0.9, 0.95, 0.99, 1),
                                       limits = c(0, 1)) +
          coord_fixed() +
          # scale_x_discrete(drop = FALSE) +
          # scale_y_discrete(limits = rev, drop = FALSE) +
          # facet_wrap(vars(item), ncol = 2) +
          labs(x = NULL, y = NULL, fill = fill.desc) +
          plot_theme +
          # guide_fill +
          theme(axis.text.x = element_text(color = "black",
                                           angle = 45,
                                           hjust = 1,
                                           vjust = 1),
                legend.key.size = unit(2*base.size, "pt"),
                plot.margin = margin(base.size, base.size, base.size, base.size))
      }

      # plot.path <- paste0(path.plots.irt, pred.scale, "/")
      # dir.create(plot.path, recursive = TRUE, showWarnings = FALSE)
      # files <- paste0(plot.path, var.foc, "_", 1:5, ".pdf")

    } # end categorical


    if(var.type == "continuous") {

      item.lev <- as.character(sort(unique(pred.var$item)))

      pred.p <- list()
      prob.p <- list()
      # ref.line <- list()
      # plim <- list()

      if(pred.scale == "prob") {
        pred.var[, pred.plot := prob]
        slope.var[, slope.plot := prob.slope]
      }
      if(pred.scale == "linpred") {
        pred.var[, pred.plot := linpred]
        slope.var[, slope.plot := linpred.slope]
      }

      for(k in seq_along(item.lev)) {

        item.foc <- item.lev[k]

        # item.foc.code <- items.code[as.character(item.foc)]

        item.desc <- 
          variables[code == item.foc, paste0(code, ": ", question.sub)] |>
          stri_wrap(width = 42) |>
          paste(collapse = "\n")

        if(pred.scale == "prob") {
          pred.desc <- paste0(resp.desc, " to adapt\n(probability)")
        }
        if(pred.scale == "linpred") {
          pred.desc <- paste0(resp.desc, " to adapt\n(log odds)")
        }

        slope.desc <- paste0("Slope\n(instantaneous change in ", tolower(resp.desc), ")")

        q.main <-
          variables[code == var.foc,
                    paste0("Main question: ", question.main)] |>
          stri_wrap(width = 96)
        q.sub <-
          variables[code == var.foc,
                    paste0("Sub-question or option: ", question.sub)] |>
          stri_wrap(width = 96)
        if(variables[code == var.foc, main] == "provinces") {
          q.sub <-
            variables[code == var.foc,
                      paste0("Sub-question or option: ", sub)] |>
            stri_wrap(width = 96)
        }
        var.desc <- paste(c(q.main, q.sub), collapse = "\n")

        plim <-
          pred.var[item.code == item.foc,
                   .(q.l = quantile(pred.plot, 0.01),
                     q.u = quantile(pred.plot, 0.99)),
                   by = "lev"
                   ][,
                     c(min(q.l), q.u = max(q.u))]

        slim <-
          slope.var[item.code == item.foc,
                   .(q.l = quantile(slope.plot, 0.01),
                     q.u = quantile(slope.plot, 0.99)),
                   by = "lev"
                   ][,
                     c(min(q.l), q.u = max(q.u))]

        vlim <-
          pred.var[item.code == item.foc,
                   .(q.l = min(lev),
                     q.u = max(lev))
                   ][,
                     c(min(q.l), q.u = max(q.u))]


        
        pred.p[[k]] <-

          ggplot(pred.var[item.code == item.foc]) +
            stat_lineribbon(aes(x = lev, y = pred.plot),
                            .width = c(0.5, 0.8, 0.9, 0.95),
                            linewidth = 0.5) +
            geom_point(data = pred.sum.l[[i]][item.code == item.foc],
                       mapping = aes(x = lev, y = prob.median), size = 1) +
            geom_rug(data = data.table(rug = var.mean + (var.sd * survey.irt[[var.foc]])),
                     mapping = aes(x = rug, y = 1),
                     sides = "b",
                     # position = position_jitter(width = (vlim[2] - vlim[1])/100, height = 0),
                     alpha = 0.1) +
            # scale_fill_discrete_divergingx("Roma", nmax = 5, order = 1:3) +
            scale_fill_brewer(palette = "Purples") +
            coord_fixed(ratio = (vlim[2] - vlim[1])/(plim[2] - plim[1]),
                        ylim = plim) +
            labs(x = var.foc, y = pred.desc,
                 subtitle = item.desc,
                 fill = "Credible\ninterval") +
            plot_theme +
            theme(plot.margin = margin(base.size, base.size, base.size, base.size))

        # prob.p[[k]] <-
        #   ggplot(slope.var[item.code == item.foc]) +
        #     stat_lineribbon(aes(x = lev, y = slope.plot),
        #                     .width = c(0.5, 0.8, 0.9, 0.95),
        #                     linewidth = 0.5) +
        #     geom_rug(data = data.table(rug = var.mean + (var.sd * survey.irt[[var.foc]])),
        #              mapping = aes(x = rug, y = 1),
        #              sides = "b",
        #              # position = position_jitter(width = (vlim[2] - vlim[1])/100, height = 0),
        #              alpha = 0.1) +
        #     geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.3) +
        #     # scale_fill_discrete_divergingx("Roma", nmax = 5, order = 1:3) +
        #     scale_fill_brewer(palette = "Purples") +
        #     coord_fixed(ratio = (vlim[2] - vlim[1])/(slim[2] - slim[1]),
        #                 ylim = slim) +
        #     labs(x = var.foc,
        #          y = slope.desc,
        #          fill = "Credible\ninterval") +
        #     plot_theme +
        #     theme(plot.margin = margin(base.size, base.size, base.size, base.size))


        prob.p[[k]] <-
          ggplot(comp.sum.l[[i]][item.code == item.foc]) +
            geom_hline(yintercept = 0, linetype = "dotted", linewidth = 0.3) +
            geom_linerange(aes(x = lev,
                               ymin = prob.slope.ci.l,
                               ymax = prob.slope.ci.u),
                           linewidth = 0.3) +
            geom_line(aes(x = lev, y = prob.slope.median),
                      linewidth = 0.5) +
            geom_point(aes(x = lev, y = prob.slope.median,
                                     fill = p.diff.pos),
                       shape = 21, colour = 1, size = 2, stroke = 0.3) +
            scale_fill_binned_divergingx("Roma", rev = TRUE, mid = 0.5,
                                         breaks = c(0,0.01, 0.05, 0.1, 0.25, 0.5,
                                                    0.75, 0.9, 0.95, 0.99, 1),
                                         limits = c(0, 1)) +
            geom_rug(data = data.table(rug = var.mean + (var.sd * survey.irt[[var.foc]])),
                     mapping = aes(x = rug, y = 1),
                     sides = "b",
                     # position = position_jitter(width = (vlim[2] - vlim[1])/100, height = 0),
                     alpha = 0.1) +
            coord_fixed(ratio = (vlim[2] - vlim[1])/(slim[2] - slim[1]),
                        ylim = slim) +
            labs(x = var.foc,
                 y = slope.desc,
                 fill = fill.desc) +
            plot_theme +
            theme(legend.key.size = unit(2*base.size, "pt"),
                  plot.margin = margin(base.size, base.size, base.size, base.size))


        if(var.foc %in% var.log) {
          pred.p[[k]] <-
            pred.p[[k]] +
              scale_x_continuous(trans = "log",
                                 breaks = scales::breaks_log(),
                                 # labels = scales::label_log()
                                 ) +
              coord_fixed(ratio = (log(vlim[2]) - log(vlim[1]))/(plim[2] - plim[1]),
                          ylim = plim)
          prob.p[[k]] <-
            prob.p[[k]] +
              scale_x_continuous(trans = "log",
                                 breaks = scales::breaks_log(),
                                 # labels = scales::label_log()
                                 ) +
              coord_fixed(ratio = (log(vlim[2]) - log(vlim[1]))/(slim[2] - slim[1]),
                          ylim = slim)
        }


      }

      # plot.path <- paste0(path.plots.irt, pred.scale, "/")
      # dir.create(plot.path, recursive = TRUE, showWarnings = FALSE)
      # files <- paste0(plot.path, var.foc, "_", 1:5, ".pdf")



      
    } # end continuous

    for(f in 1:3) { 

      plot.idx <- (1:2)+((f-1)*2)

      plot.print <-
        wrap_plots(c(pred.p[plot.idx], prob.p[plot.idx]), byrow = FALSE, ncol = 2) +
          plot_annotation(title = var.foc,
                          subtitle = var.desc,
                          caption = paste0(var.foc, " [", f, "/3]"),
                          theme = plot_theme +
                                  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5,
                                                             unit = "inch")))

      print(plot.print)

    } # end printing plots

  } # end looping over predictor scales

}

graphics.off()

pred.all <- rbindlist(pred.all.l, fill = TRUE)
slope.all <- rbindlist(slope.all.l, fill = TRUE)
pred.sum <- rbindlist(pred.sum.l, fill = TRUE)
comp.sum <- rbindlist(comp.sum.l, fill = TRUE)
n.sum <- rbindlist(n.sum.l, fill = TRUE)

# names(comp.sum.l) <- vars.irt
# comp.sum <- rbindlist(comp.sum.l, fill = TRUE, idcol = "code.mar")

saveRDS(list(pred = pred.all, slope = slope.all), file.irt.pred.all)

pred.sum <- merge(pred.sum, n.sum, all.x = TRUE, sort = FALSE)
setnames(pred.sum, c("code.mar", "item.code", "lev"), c("var.code", "adapt.code", "var.level"))
setorder(pred.sum, var.code, adapt.code)

setnames(comp.sum,
         c("code.mar", "item.code",
           "lev", "lev1", "lev2",
           "p.diff.pos", "p.diff.neg"),
         c("var.code", "adapt.code",
           "var.level.cont", "var.level.1", "var.level.2",
           "cert.pos", "cert.neg"),
         skip_absent = TRUE)
setorder(comp.sum, var.code, adapt.code)
comp.cols.ord <-
  c("adapt.code", "var.code",
    "var.level.1", "var.level.2",
    "var.level.cont",
    "prob.diff.median", "prob.diff.ci.l", "prob.diff.ci.u",
    "linpred.diff.median", "linpred.diff.ci.l", "linpred.diff.ci.u",
    "prob.slope.median", "prob.slope.ci.l", "prob.slope.ci.u",
    "linpred.slope.median", "linpred.slope.ci.l", "linpred.slope.ci.u",
    "cert.pos", "cert.neg")
comp.cols.absent <- setdiff(comp.cols.ord, names(comp.sum))
comp.sum[, (comp.cols.absent) := NA]
setcolorder(comp.sum, comp.cols.ord)

fwrite(pred.sum, file.irt.pred)
fwrite(comp.sum, file.irt.comp)

pred.ex.cols <- names(pred.sum)
pred.ex.cols <- pred.ex.cols[grep("linpred", pred.ex.cols, invert = TRUE)]

pred.sum.ex <- pred.sum[, ..pred.ex.cols]
# pred.ex.newcols <- stri_replace_all_fixed(names(pred.sum.ex), "prob", "prop")
# setnames(pred.sum.ex, pred.ex.newcols)

comp.ex.cols <- names(comp.sum)
comp.ex.cols <- comp.ex.cols[grep("linpred", comp.ex.cols, invert = TRUE)]

comp.sum.ex <- comp.sum[, ..comp.ex.cols]
# comp.ex.newcols <- stri_replace_all_fixed(names(comp.sum.ex), "prob", "prop")
# setnames(comp.sum.ex, comp.ex.newcols)

fwrite(pred.sum.ex, file.irt.pred.ex)
fwrite(comp.sum.ex, file.irt.comp.ex)


# comp.sum[var.code == "A26" & adapt.code == "D05"]
