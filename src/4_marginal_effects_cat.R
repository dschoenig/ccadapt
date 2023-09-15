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

cont.nl <- FALSE
pred.scales <- c("prob")
mar.type <- "cf"
cont.pred.n <- 21
cont.diff.frac <- 100
slope.res <- "fine"
var.log <- c("A03")
draw.ids <- NULL

cont.nl <- FALSE
resp.types <- c("willingness", "urgency")
mar.type <- "cf"
cont.pred.n <- 3
cont.diff.frac <- 100
slope.res <- "fine"
var.log <- c("A03")
draw.ids <- sample(1:1e4, 50)

ci.et.width <- 0.9
q.ci.l <- (1-ci.et.width)/2
q.ci.u <- 1-q.ci.l

file.survey.irt <- file.survey.irt.c
file.irt.mod.2pl <- file.irt.c.mod.2pl
path.irt.plots <- path.irt.c.plots
path.results.irt <- path.results.c.irt

if(cont.nl == TRUE) {
  file.irt.mod <- file.irt.mod.2pl.nl
  suffix.nl <- ".nl"
} else {
  file.irt.mod <- file.irt.mod.2pl
  suffix.nl <- ""
}

file.irt.pred.w <- paste0(path.results.irt, "willingness.predictions.", mar.type, suffix.nl, ".csv")
file.irt.comp.w <- paste0(path.results.irt, "willingness.comparisons.", mar.type, suffix.nl, ".csv")
file.irt.pred.w.ex <- paste0(path.results.irt, "willingness.predictions", suffix.nl, ".csv")
file.irt.comp.w.ex <- paste0(path.results.irt, "willingness.comparisons", suffix.nl, ".csv")

file.irt.pred.u <- paste0(path.results.irt, "urgency.predictions.", mar.type, suffix.nl, ".csv")
file.irt.comp.u <- paste0(path.results.irt, "urgency.comparisons.", mar.type, suffix.nl, ".csv")
file.irt.pred.u.ex <- paste0(path.results.irt, "urgency.predictions", suffix.nl, ".csv")
file.irt.comp.u.ex <- paste0(path.results.irt, "urgency.comparisons", suffix.nl, ".csv")


variables <- readRDS(file.variables.proc)
cat.levels <- readRDS(file.cat.levels.proc)

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




vars.irt <- variables[code %in% names(survey.irt), code]

vars.pred <-
  variables[code %in% vars.irt,
            .(code,
              type, cat.scale, cat.ref, cat.ord, cont.mean, cont.sd,
              category.personal_stakes, category.threat_appraisal,
              category.coping_appraisal, category.control)
            ]

items.ref <- levels(survey.irt$item)
items.code <- variables[category.adaptation == 1, code]
items.code <- factor(items.code, levels = items.code)
names(items.code) <- items.ref



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


pred.l <- list()
pred.sum.l <- list()
comp.sum.l <- list()
n.sum.l <- list()


for(p in seq_along(resp.types)) {
  plot.file <- paste0(path.irt.plots, resp.types[p], ".", mar.type, suffix.nl, ".pdf")
  cairo_pdf(plot.file, onefile = TRUE, width = 8.5, height = 11)
}

# vars.irt <- vars.irt[1:2]
# vars.irt <- "F14"
# vars.irt <- "A22"
vars.irt <- c("A22", "A23")
# vars.irt <- c("A03", "A06", "A22")
# survey.irt <- survey.irt[id %in% sample(1:.N, 100)]

for(i in seq_along(vars.irt)) {
  
  message(paste0("Processing variable ", i, "/", length(vars.irt), " …"))

  var.foc <- vars.irt[i]
  var.type <- variables[code == var.foc, type]

  if(var.type == "continuous") {
    var.mean <- variables[code == var.foc, cont.mean]
    var.sd <- variables[code == var.foc, cont.sd]
    if(var.foc %in% var.log) {
      var.org <-  var.mean + (var.sd * survey.irt[[var.foc]])
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
      survey.irt[,
                 factor(levels(var.sel), levels = levels(var.sel)),
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
        pred.mod <- posterior_epred(mod.irt,
                                    newdata = var.lev.dt,
                                    re_formula = re.form,
                                    draw_ids = draw.ids)

    }
    if(mar.type == "cf") {
      pred.mod <- 
        posterior_epred(mod.irt,
                        newdata = var.lev.dt,
                        draw_ids = draw.ids)
    }

    pred.mod.w <- t(pred.mod[,,3] + pred.mod[,,4])
    pred.mod.u <- t(pred.mod[,,4])

    colnames(pred.mod.w) <- paste0("draw_", 1:ncol(pred.mod.w))
    colnames(pred.mod.u) <- paste0("draw_", 1:ncol(pred.mod.u))

    pred.mod.w.obs <-
      as.data.table(as.data.frame(pred.mod.w)) |>
      _[, obs := var.lev.dt$obs] |>
      melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
           value.name = "prob")
    pred.mod.w.obs[, resp.type := "willingness"]

    pred.mod.u.obs <-
      as.data.table(as.data.frame(pred.mod.u)) |>
      _[, obs := var.lev.dt$obs] |>
      melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
           value.name = "prob")
    pred.mod.u.obs[, resp.type := "urgency"]

    pred.var.lev <-
      rbind(pred.mod.w.obs, pred.mod.u.obs) |>
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
                   lev := factor(var.lev.foc, levels = var.lev),
                   env = list(var.sel = var.foc)]
    }

    pred.var.l[[j]] <-
      pred.var.lev[,
                   .(prob = mean(prob)),
                   by = c("resp.type", "draw", "item.code", "code.mar", "lev")]

    gc()

  }

  pred.var <- rbindlist(pred.var.l)


  if(var.type == "categorical") {

    message("Comparing levels …")

    # Comparison between levels

    var.cat.ref <- variables[code == var.foc, cat.ref]
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
                       .(resp.type, item.code, draw, lev1 = lev,
                         prob1 = prob)],
              pred.var[lev ==  var.comb[j, lev2],
                       .(resp.type, item.code, draw, lev2 = lev,
                         prob2 = prob)],
              by = c("resp.type", "item.code", "draw"))

      var.comp.l[[j]] <-
        var.lev.comp[,
                     .(lev1 = var.comb[j, lev1],
                       lev2 = var.comb[j, lev2],
                       prob.diff.median = median(prob1 - prob2),
                       prob.diff.ci.l = quantile(prob1 - prob2, q.ci.l),
                       prob.diff.ci.u = quantile(prob1 - prob2, q.ci.u),
                       p.diff.pos = sum(prob1 > prob2)/.N,
                       p.diff.neg = sum(prob1 < prob2)/.N),
                     by = c("resp.type", "item.code")]

    }

    var.comp.dt <- rbindlist(var.comp.l, idcol = "comp.id")
    setorder(var.comp.dt, item.code, comp.id)
  }


  if(var.type == "continuous") {

    message("Calculating slopes (central difference) …")

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
                   prob.fw = prob[c(2:.N, .N)]
                   ),
                 by = c("resp.type", "item.code", "draw")]

      slope.var[, `:=`(prob.slope = (prob.fw - prob.bw) / (lev.fw - lev.bw))]

    }

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
          pred.mod.fw <- 
            posterior_epred(mod.irt,
                            newdata = var.lev.fw,
                            re_formula = re.form,
                            draw_ids = draw.ids)
          pred.mod.bw <- 
            posterior_epred(mod.irt,
                            newdata = var.lev.bw,
                            re_formula = re.form,
                            draw_ids = draw.ids)
        }
        if(mar.type == "cf") {
          pred.mod.fw <- 
            posterior_epred(mod.irt,
                            newdata = var.lev.fw,
                            draw_ids = draw.ids)
          pred.mod.bw <- 
            posterior_epred(mod.irt,
                            newdata = var.lev.bw,
                            draw_ids = draw.ids)
        }

        pred.mod.w.fw <- t(pred.mod.fw[,,3] + pred.mod.fw[,,4])
        pred.mod.w.bw <- t(pred.mod.bw[,,3] + pred.mod.bw[,,4])
        pred.mod.u.fw <- t(pred.mod.fw[,,4])
        pred.mod.u.bw <- t(pred.mod.bw[,,4])

        pred.mod.w.prob <- (pred.mod.w.fw - pred.mod.w.bw) / diff.h
        colnames(pred.mod.w.prob) <- paste0("draw_", 1:ncol(pred.mod.w.prob))

        pred.mod.u.prob <- (pred.mod.u.fw - pred.mod.u.bw) / diff.h
        colnames(pred.mod.u.prob) <- paste0("draw_", 1:ncol(pred.mod.u.prob))

        slope.var.lev.w.prob <-
          as.data.table(as.data.frame(pred.mod.w.prob)) |>
          _[, obs := var.lev.dt$obs] |>
          melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
               value.name = "prob.slope") |>
          merge(var.lev.dt[,.(obs, item)])
        slope.var.lev.w.prob[, resp.type := "willingness"]

        slope.var.lev.u.prob <-
          as.data.table(as.data.frame(pred.mod.u.prob)) |>
          _[, obs := var.lev.dt$obs] |>
          melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
               value.name = "prob.slope") |>
          merge(var.lev.dt[,.(obs, item)])
        slope.var.lev.u.prob[, resp.type := "urgency"]

        slope.var.lev <- rbind(slope.var.lev.w.prob,
                               slope.var.lev.u.prob)

        slope.var.lev[, draw := as.integer(draw)]
        slope.var.lev[, item.code := items.code[as.character(item)]]
        slope.var.lev[, code.mar := var.foc]

        slope.var.lev[,
                     lev := var.lev.foc,
                     env = list(var.sel = var.foc)]

        slope.var.l[[j]] <-
          slope.var.lev[,
                       .(prob.slope = mean(prob.slope)),
                       by = c("resp.type", "draw", "item.code", "code.mar", "lev")]

        gc()

      }

      slope.var <- rbindlist(slope.var.l)

    } # end fine

      var.comp.dt <-
        slope.var[,
                  .(prob.slope.median = median(prob.slope),
                    prob.slope.ci.l = quantile(prob.slope, q.ci.l),
                    prob.slope.ci.u = quantile(prob.slope, q.ci.u),
                    p.diff.pos = sum(prob.slope > 0)/.N,
                    p.diff.neg = sum(prob.slope < 0)/.N),
                  by = c("resp.type", "item.code", "code.mar", "lev")]

  }


  # Summarize for subsequent export


  if(var.type == "categorical") {

    var.n <-
      survey.irt[,
                 .(n = .N),
                 by = c("item", var.foc)
                 ][,
                   .(code.mar = var.foc,
                     item.code = items.code[as.character(item)],
                     lev = var.sel,
                     n),
                   env = list(var.sel = var.foc)
                   ]
    var.n[, lev := factor(lev, levels = levels(pred.var$lev))]
    setorder(var.n, code.mar, item.code, lev)


    pred.sum.l[[i]] <-
      pred.var[order(code.mar, item.code, lev),
               .(prob.median = median(prob),
                 prob.q5 = quantile(prob, 0.05),
                 prob.q25 = quantile(prob, 0.25),
                 prob.q75 = quantile(prob, 0.75),
                 prob.q95 = quantile(prob, 0.95)),
               by = c("resp.type", "code.mar", "item.code", "lev")
               ]

    comp.sum.l[[i]] <-
      var.comp.dt[,
                  .(code.mar = var.foc,
                    item.code,
                    lev1,
                    lev2,
                    prob.diff.median,
                    prob.diff.ci.l,
                    prob.diff.ci.u,
                    p.diff.pos,
                    p.diff.neg)]

    n.sum.l[[i]] <- var.n

  }


  if(var.type == "continuous") {

    pred.var[, lev := (lev*var.sd) + var.mean]
    slope.var[, lev := (lev*var.sd) + var.mean]
    var.comp.dt[, lev := (lev*var.sd) + var.mean]

    pred.sum.l[[i]] <-
      pred.var[order(code.mar, item.code, lev),
               .(prob.median = median(prob),
                 prob.q5 = quantile(prob, 0.05),
                 prob.q25 = quantile(prob, 0.25),
                 prob.q75 = quantile(prob, 0.75),
                 prob.q95 = quantile(prob, 0.95)),
               by = c("resp.type", "code.mar", "item.code", "lev")
               ]

    comp.sum.l[[i]] <-
      var.comp.dt[order(code.mar, item.code, lev)]

    var.n <-
      survey.irt[,
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
                       item.code = items.code[as.character(item)],
                       lev,
                       n),
                     ]

    n.sum.l[[i]] <- var.n

  }

  # Prepare plots

  message("Creating plots …")

  for(p in seq_along(resp.types)) {
   
    dev.set(dev.list()[[p]])

    resp.type <- resp.types[p]
  

    if(var.type == "categorical") {

      item.lev <- levels(pred.var$item)

      pred.p <- list()
      prob.p <- list()
      # ref.line <- list()
      # plim <- list()

      pred.var.p <- pred.var[resp.type == resp.sel, env = list(resp.sel = I(resp.type))]

      for(k in seq_along(item.lev)) {

        item.foc <- item.lev[k]

        # item.foc.code <- items.code[as.character(item.foc)]

        item.desc <- 
          variables[code == item.foc, paste0(code, ": ", question.sub)] |>
          stri_wrap(width = 42) |>
          paste(collapse = "\n")

        if(resp.type == "willingness") {
          pred.desc <- "Willingness to adapt\n(propensity)"
          p.desc <- "Certainty that\nwillingness\nincreases"
        }
        if(resp.type == "urgency") {
          pred.desc <- "Urgency to adapt\n(propensity)"
          p.desc <- "Certainty that\nurgency\nincreases"
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
          pred.var.p[item.code == item.foc & lev == var.cat.ref,
                     median(prob)]
        
        plim <-
          pred.var.p[item.code == item.foc,
                     # .(q.l = quantile(prob, 0.01),
                     #   q.u = quantile(prob, 0.99)),
                     .(q.l = quantile(prob, 0),
                       q.u = quantile(prob, 1)),
                     by = "lev"
                     ][,
                       c(min(q.l), q.u = max(q.u))]

        ylab <-
          var.n[item.code == item.foc,
                paste0(lev, " (", n, ")")]

        pred.p[[k]] <-
          ggplot(pred.var.p[item.code == item.foc]) +
            stat_halfeye(aes(x = prob, y = lev),
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
          ggplot(var.comp.dt[item.code == item.foc & resp.type == resp.sel,
                             env = list(resp.sel = I(resp.type))]) +
          geom_tile(aes(y = lev1, x = lev2, fill = p.diff.pos), na.rm = TRUE) +
          geom_point(data = var.comp.dt[item.code == item.foc &
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
          labs(x = NULL, y = NULL, fill = p.desc) +
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

      item.lev <- levels(pred.var$item)

      pred.p <- list()
      prob.p <- list()
      # ref.line <- list()
      # plim <- list()

      pred.var.p <- pred.var[resp.type == resp.sel, env = list(resp.sel = I(resp.type))]
      slope.var.p <- slope.var[resp.type == resp.sel, env = list(resp.sel = I(resp.type))]

      for(k in seq_along(item.lev)) {

        item.foc <- item.lev[k]

        # item.foc.code <- items.code[as.character(item.foc)]

        item.desc <- 
          variables[code == item.foc, paste0(code, ": ", question.sub)] |>
          stri_wrap(width = 42) |>
          paste(collapse = "\n")

        if(resp.type == "willingness") {
          pred.desc <- "Willingness to adapt\n(propensity)"
          p.desc <- "Certainty that\nwillingness\nincreases"
          slope.desc <- "Slope\n(instantaneous change in willingness)"
        }
        if(resp.type == "urgency") {
          pred.desc <- "Urgency to adapt\n(propensity)"
          p.desc <- "Certainty that\nurgency\nincreases"
          slope.desc <- "Slope\n(instantaneous change in urgency)"
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

        plim <-
          pred.var.p[item.code == item.foc,
                     .(q.l = quantile(prob, 0.01),
                       q.u = quantile(prob, 0.99)),
                     by = "lev"
                     ][,
                       c(min(q.l), q.u = max(q.u))]

        slim <-
          slope.var.p[item.code == item.foc,
                     .(q.l = quantile(prob.slope, 0.01),
                       q.u = quantile(prob.slope, 0.99)),
                     by = "lev"
                     ][,
                       c(min(q.l), q.u = max(q.u))]

        vlim <-
          pred.var.p[item.code == item.foc,
                     .(q.l = min(lev),
                       q.u = max(lev))
                     ][,
                       c(min(q.l), q.u = max(q.u))]

        pred.p[[k]] <-
          ggplot(pred.var.p[item.code == item.foc]) +
            stat_lineribbon(aes(x = lev, y = prob),
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
          ggplot(comp.sum.l[[i]][item.code == item.foc & resp.type == resp.sel,
                                 env = list(resp.sel = I(resp.type))]) +
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
                 fill = "Certainty that\nwillingness\nincreases") +
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

    for(f in 1:5) { 

      plot.idx <- (1:2)+((f-1)*2)

      plot.print <-
        wrap_plots(c(pred.p[plot.idx], prob.p[plot.idx]), byrow = FALSE, ncol = 2) +
          plot_annotation(title = var.foc,
                          subtitle = var.desc,
                          caption = paste0(var.foc, " [", f, "/5]"),
                          theme = plot_theme +
                                  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5,
                                                             unit = "inch")))

      print(plot.print)

    } # end printing plots

  } # end looping over predictor scales

}

graphics.off()

pred.sum <- rbindlist(pred.sum.l, fill = TRUE)
comp.sum <- rbindlist(comp.sum.l, fill = TRUE)
n.sum <- rbindlist(n.sum.l, fill = TRUE)

pred.sum <- merge(pred.sum, n.sum, all.x = TRUE, sort = FALSE)
setnames(pred.sum, c("code.mar", "item.code", "lev"), c("var.code", "adapt.code", "var.level"))
setorder(pred.sum, var.code, adapt.code)

setnames(comp.sum,
         c("code.mar", "item.code",
           "lev", "lev1", "lev2",
           "p.diff.pos", "p.diff.neg"),
         c("var.code", "adapt.code",
           "var.level.cont", "var.level.1", "var.level.2",
           "cert.pos", "cert.neg"))
setorder(comp.sum, var.code, adapt.code)
setcolorder(comp.sum,
             c("resp.type",
               "adapt.code", "var.code",
               "var.level.1", "var.level.2",
               "var.level.cont",
               "prob.diff.median", "prob.diff.ci.l", "prob.diff.ci.u",
               "linpred.diff.median", "linpred.diff.ci.l", "linpred.diff.ci.u",
               "prob.slope.median", "prob.slope.ci.l", "prob.slope.ci.u",
               "linpred.slope.median", "linpred.slope.ci.l", "linpred.slope.ci.u",
               "cert.pos", "cert.neg"))



fwrite(pred.sum[resp.type == "willingness"], file.irt.pred.w)
fwrite(comp.sum[resp.type == "willingness"], file.irt.comp.w)

fwrite(pred.sum[resp.type == "urgency"], file.irt.pred.u)
fwrite(comp.sum[resp.type == "urgency"], file.irt.comp.u)

