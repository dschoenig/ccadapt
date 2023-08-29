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

survey <- readRDS(file.survey.proc)
variables <- readRDS(file.variables.proc)
cat.levels <- readRDS(file.cat.levels.proc)
dependencies <- readRDS(file.questions.dependencies)

survey.fit <- readRDS(file.survey.rf)
survey.irt <- readRDS(file.survey.irt)
# survey.irt <- survey.irt[, .SD[1], by = item]

mod.irt <- readRDS(file.irt.mod.2pl)

pred.scales <- c("prob", "linpred")
# mar.type <- "cf"
mar.type <- "mem"
ci.et.width <- 0.9
q.ci.l <- (1-ci.et.width)/2
q.ci.u <- 1-q.ci.l

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




vars.irt <- names(survey.irt)
vars.irt <- sort(vars.irt[vars.irt %in% variables$code])

vars.pred <-
  variables[code %in% vars.irt,
            .(code,
              type, cat.scale, cat.ref, cat.ord, cont.mean, cont.sd,
              category.personal_stakes, category.threat_appraisal,
              category.coping_appraisal, category.control)
            ]

items.ref <- levels(survey.irt$item)
items.code <- paste0("D", 1:10)
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

for(p in seq_along(pred.scales)) {
  plot.file <- paste0(path.plots.irt, pred.scales[p], ".", mar.type, ".pdf")
  cairo_pdf(plot.file, onefile = TRUE, width = 8.5, height = 11)
}

# vars.irt <- vars.irt[1]
# vars.irt <- "F14"

for(i in seq_along(vars.irt)) {
  
  message(paste0("Processing variable ", i, "/", length(vars.irt), " …"))

  var.foc <- vars.irt[i]

  var.lev <- 
    survey.irt[,
               factor(levels(var.sel), levels = levels(var.sel)),
               env = list(var.sel = var.foc)]


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
      pred.mod <- t(posterior_epred(mod.irt, var.lev.dt, re_formula = re.form))
    }
    if(mar.type == "cf") {
      pred.mod <- t(posterior_epred(mod.irt, var.lev.dt))
    }

    colnames(pred.mod) <- paste0("draw_", 1:ncol(pred.mod))

    pred.var.lev <-
      as.data.table(as.data.frame(pred.mod)) |>
      _[, obs := var.lev.dt$obs] |>
      melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
           value.name = "prob") |>
      _[, linpred := qlogis(prob)] |>
      merge(var.lev.dt[,.(obs, item)])

    pred.var.lev[, draw := as.integer(draw)]
    pred.var.lev[, item.code := items.code[as.character(item)]]
    pred.var.lev[, code.mar := var.foc]
    pred.var.lev[,
                 lev := factor(var.lev.foc, levels = var.lev),
                 env = list(var.sel = var.foc)]

    pred.var.l[[j]] <-
      pred.var.lev[,
                   .(
                     linpred = mean(linpred),
                     prob = mean(prob)),
                   by = c("draw", "item.code", "code.mar", "lev")]

    gc()

  }

  pred.var <- rbindlist(pred.var.l)

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
                     .(item.code, draw, lev1 = lev,
                       prob1 = prob, linpred1 = linpred)],
            pred.var[lev ==  var.comb[j, lev2],
                     .(item.code, draw, lev2 = lev,
                       prob2 = prob, linpred2 = linpred)],
            by = c("item.code", "draw"))

    var.comp.l[[j]] <-
      var.lev.comp[,
                   .(lev1 = var.comb[j, lev1],
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

  var.comp.dt <- rbindlist(var.comp.l, idcol = "comp.id")
  setorder(var.comp.dt, item.code, comp.id)


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

  # Summarize for subsequent export

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

  comp.sum.l[[i]] <-
    var.comp.dt[,
                .(code.mar = var.foc,
                  item.code, lev1, lev2,
                  prob.diff.median,
                  prob.diff.ci.l,
                  prob.diff.ci.u,
                  linpred.diff.median,
                  linpred.diff.ci.l,
                  linpred.diff.ci.u,
                  p.diff.pos,
                  p.diff.neg)]

  n.sum.l[[i]] <- var.n

  # Prepare plots

  for(p in seq_along(pred.scales)) {
   
    dev.set(dev.list()[[p]])

    pred.scale <- pred.scales[p]
   
    item.lev <- levels(pred.var$item)

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

    for(k in seq_along(item.lev)) {

      item.foc <- item.lev[k]

      # item.foc.code <- items.code[as.character(item.foc)]

      item.desc <- 
        variables[code == item.foc, paste0(code, ": ", question.sub)] |>
        stri_wrap(width = 42) |>
        paste(collapse = "\n")

      if(pred.scale == "prob") {
        pred.desc <- "Willingness to adapt\n(probability)"
      }
      if(pred.scale == "linpred") {
        pred.desc <- "Willingness to adapt\n(log odds)"
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
        ggplot(var.comp.dt[item.code == item.foc]) +
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
        labs(x = NULL, y = NULL, fill = "Certainty that\nwillingness\nincreases") +
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

    }


  }

}

graphics.off()

pred.sum <- rbindlist(pred.sum.l)
comp.sum <- rbindlist(comp.sum.l)
n.sum <- rbindlist(n.sum.l)

pred.sum <- merge(pred.sum, n.sum, all.x = TRUE, sort = FALSE)
setnames(pred.sum, c("code.mar", "item.code", "lev"), c("var.code", "adapt.code", "var.level"))
setorder(pred.sum, var.code, adapt.code)

setnames(comp.sum,
         c("code.mar", "item.code", "lev1", "lev2", "p.diff.pos", "p.diff.neg"),
         c("var.code", "adapt.code", "var.level.1", "var.level.2", "cert.diff.pos", "cert.diff.neg"))
setorder(comp.sum, var.code, adapt.code)

fwrite(pred.sum, file.irt.pred.mem)
fwrite(comp.sum, file.irt.comp.mem)
      



