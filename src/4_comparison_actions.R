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
cont.nl <- FALSE
# resp.type <- "urgency"
# pred.scales <- c("prob", "linpred")
pred.scales <- c("prob")
draw.ids <- NULL
ci.et.width <- 0.9
q.ci.l <- (1-ci.et.width)/2
q.ci.u <- 1-q.ci.l


# # For testing
# cont.pred.n <- 5
# slope.res <- "coarse"
# draw.ids <- sample(1:1e4, 100)

if(resp.type == "willingness") {
  file.survey.irt <- file.survey.irt.w.fire
  file.irt.mod.2pl <- file.irt.w.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.w.mod.2pl.nl.fire
  path.irt.plots <- path.irt.w.plots
  path.results.irt <- path.results.w.irt
}
if(resp.type == "urgency") {
  file.survey.irt <- file.survey.irt.u.fire
  file.irt.mod.2pl <- file.irt.u.mod.2pl.fire
  file.irt.mod.2pl.nl <- file.irt.u.mod.2pl.nl.fire
  path.irt.plots <- path.irt.u.plots
  path.results.irt <- path.results.u.irt
}

if(cont.nl == TRUE) {
  file.irt.mod <- file.irt.mod.2pl.nl
  suffix.nl <- "nl."
} else {
  file.irt.mod <- file.irt.mod.2pl
  suffix.nl <- "."
}

file.irt.pred <- paste0(path.results.irt, "actions.predictions", suffix.nl, "fire.csv")
file.irt.comp <- paste0(path.results.irt, "actions.comparisons", suffix.nl, "fire.csv")
file.irt.pred.ex <- paste0(path.results.irt, "actions.predictions", suffix.nl, "fire.csv")
file.irt.comp.ex <- paste0(path.results.irt, "actions.comparisons", suffix.nl, "fire.csv")


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


survey.pred <- copy(survey.irt)
survey.pred[, obs := 1:.N]

pred.mod <- t(posterior_epred(mod.irt,
                              newdata = survey.pred,
                              draw_ids = draw.ids))
pred.mod.lp <- t(posterior_linpred(mod.irt,
                                   newdata = survey.pred,
                                   draw_ids = draw.ids))

colnames(pred.mod) <- paste0("draw_", 1:ncol(pred.mod))
colnames(pred.mod.lp) <- paste0("draw_", 1:ncol(pred.mod.lp))

pred.mod.obs <-
  as.data.table(as.data.frame(pred.mod)) |>
  _[, obs := survey.pred$obs] |>
  melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
       value.name = "prob")

pred.mod.lp.obs <-
  as.data.table(as.data.frame(pred.mod.lp)) |>
  _[, obs := survey.pred$obs] |>
  melt(measure.vars = measure(draw, pattern = "draw_(.*)"),
       value.name = "linpred")

pred.var <-
  merge(pred.mod.obs, pred.mod.lp.obs, by = c("obs", "draw")) |>
  merge(survey.pred[, .(obs, item)], by = c("obs")) |>
  _[,
    .(prob = mean(prob), linpred = mean(linpred)),
    by = c("item", "draw")]

pred.var[, draw := as.integer(draw)]
setorder(pred.var, item, draw)


item.lev <- sort(unique(pred.var$item))
    
var.comb <- 
  CJ(lev1 = item.lev,
     lev2 = item.lev)
var.comb <- var.comb[lev1 != lev2]
var.comb[, comp.id := 1:.N]

var.comp.l <- list()
for(j in 1:nrow(var.comb)) {
  var.lev.comp <-
    merge(pred.var[item ==  var.comb[j, lev1],
                   .(draw, lev1 = item,
                     prob1 = prob, linpred1 = linpred)],
          pred.var[item ==  var.comb[j, lev2],
                   .(draw, lev2 = item,
                     prob2 = prob, linpred2 = linpred)],
          by = c("draw"))

  var.comp.l[[j]] <-
    var.lev.comp[,
                 .(item1 = var.comb[j, lev1],
                   item2 = var.comb[j, lev2],
                   prob.diff.median = median(prob1 - prob2),
                   prob.diff.ci.l = quantile(prob1 - prob2, q.ci.l),
                   prob.diff.ci.u = quantile(prob1 - prob2, q.ci.u),
                   linpred.diff.median = median(linpred1 - linpred2),
                   linpred.diff.ci.l = quantile(linpred1 - linpred2, q.ci.l),
                   linpred.diff.ci.u = quantile(linpred1 - linpred2, q.ci.u),
                   p.diff.pos = sum(prob1 > prob2)/.N,
                   p.diff.neg = sum(prob1 < prob2)/.N)]
}


var.comp.dt <- rbindlist(var.comp.l, idcol = "comp.id")
setorder(var.comp.dt, comp.id)

pred.sum <-
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
           by = c("item")
           ]

comp.sum <-
  var.comp.dt[,
              .(item1,
                item2,
                prob.diff.median,
                prob.diff.ci.l,
                prob.diff.ci.u,
                linpred.diff.median,
                linpred.diff.ci.l,
                linpred.diff.ci.u,
                p.diff.pos,
                p.diff.neg)]



message("Creating plots …")

for(p in seq_along(pred.scales)) {
  if(pred.scales[p] == "prob") plot.type <- "prob"
  if(pred.scales[p] == "linpred") plot.type <- "lp"
  plot.file <- paste0(path.irt.plots, "actions.", plot.type, suffix.nl, "fire.pdf")
  cairo_pdf(plot.file, onefile = TRUE, width = 8.5, height = 11)
}

for(p in seq_along(pred.scales)) {
 
  dev.set(dev.list()[[p]])

  pred.scale <- pred.scales[p]

  if(resp.type == "willingness") {
    resp.desc <- "Willingness"
    fill.desc <- "Certainty that\nwillingness\nis higher"
  }
  if(resp.type == "urgency") {
    resp.desc <- "Urgency"
    fill.desc <- "Certainty that\nurgency\nis higher"
  }

  if(pred.scale == "prob") {
    pred.var[, pred.plot := prob]
  }
  if(pred.scale == "linpred") {
    pred.var[, pred.plot := linpred]
  }

  item.desc <- 
    variables[code == "D01", question.main] |>
    stri_wrap(width = 42) |>
    paste(collapse = "\n")

  if(pred.scale == "prob") {
    pred.desc <- paste0(resp.desc, " to adapt\n(probability)")
  }
  if(pred.scale == "linpred") {
    pred.desc <- paste0(resp.desc, " to adapt\n(log odds)")
  }

  ref.line <-
    pred.var[, median(pred.plot)]
  
  plim <-
    pred.var[,
             .(q.l = quantile(pred.plot, 0.01),
               q.u = quantile(pred.plot, 0.99)),
             by = "item"
             ][,
               c(min(q.l), q.u = max(q.u))]

  ylab <- as.character(unique(sort(survey.irt$item)))

  pred.p <-
    ggplot(pred.var) +
      stat_halfeye(aes(x = pred.plot, y = item),
                        .width = c(0.5, 0.9),
                        fill = "grey80",
                        scale = 0.8) +
      geom_vline(xintercept = ref.line, linewidth = 0.3, linetype = "dashed") + 
      coord_fixed(ratio = (plim[2] - plim[1])/length(ylab),
                  xlim = plim) +
      scale_y_discrete(limits = rev) +
      labs(y = NULL, x = pred.desc,
           subtitle = NULL) +
      plot_theme +
      theme(plot.margin = margin(base.size, base.size, base.size, base.size))


  prob.p <-
    ggplot(var.comp.dt) +
    geom_tile(aes(y = item1, x = item2, fill = p.diff.pos), na.rm = TRUE) +
    geom_point(data = var.comp.dt[(p.diff.pos > 0.95 | p.diff.pos < 0.05)],
               aes(y = item1, x = item2)) +
    scale_fill_binned_divergingx("Roma", rev = TRUE, mid = 0.5,
                                 breaks = c(0,0.01, 0.05, 0.1, 0.25, 0.5,
                                            0.75, 0.9, 0.95, 0.99, 1),
                                 limits = c(0, 1)) +
    coord_fixed() +
    # scale_x_discrete(drop = FALSE) +
    scale_y_discrete(limits = rev) +
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


    plot.print <-
      wrap_plots(pred.p, prob.p, byrow = FALSE, ncol = 2) +
        plot_annotation(title = "Adapation actions",
                        subtitle = item.desc,
                        theme = plot_theme +
                                theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5,
                                                           unit = "inch")))

    print(plot.print)

}


graphics.off()

setnames(pred.sum, c("item"), c("adapt.code"))
setorder(pred.sum, adapt.code)

setnames(comp.sum,
         c("p.diff.pos", "p.diff.neg"),
         c("cert.pos", "cert.neg"))

setorder(comp.sum, item1, item2)
setcolorder(comp.sum,
             c("prob.diff.median", "prob.diff.ci.l", "prob.diff.ci.u",
               "linpred.diff.median", "linpred.diff.ci.l", "linpred.diff.ci.u",
               "cert.pos", "cert.neg"))


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







