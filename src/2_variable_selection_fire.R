args <- commandArgs(trailingOnly = TRUE)

library(data.table)
library(stringi)
library(brms)
library(projpred)
library(ggplot2)
library(ggrepel)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)

resp.type <- as.character(args[1])
resp.type <- "willingness"

message(paste0("Response type: `", resp.type, "`"))

if(resp.type == "willingness") {
  file.var.sel.prefix <- file.var.sel.w.prefix
  file.var.sel.res <- file.var.sel.w.res.fire
  file.var.sel.res.csv <- file.var.sel.w.res.fire.csv
  file.var.sel.plot <- file.var.sel.w.plot.fire
}
if(resp.type == "urgency") {
  file.var.sel.prefix <- file.var.sel.u.prefix
  file.var.sel.res <- file.var.sel.u.res.fire
  file.var.sel.res.csv <- file.var.sel.u.res.fire.csv
  file.var.sel.plot <- file.var.sel.u.plot.fire
}
if(resp.type == "categorical") {
  file.var.sel.prefix <- file.var.sel.c.prefix
  file.var.sel.res <- file.var.sel.c.res.fire
  file.var.sel.res.csv <- file.var.sel.c.res.fire.csv
  file.var.sel.plot <- file.var.sel.c.plot.fire
}


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
        strip.text = element_text(size = rel(1.2),
                                  # face = "bold",
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(base.size*2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size)),
        # strip.background = element_rect(fill = "gray90", colour = NA)
        strip.background = element_rect(fill = NA, colour = NA)
  )



variables <- readRDS(file.variables.proc)

if(resp.type == "categorical") {
  vars.adapt <- c("D01", "D02", "D03", "D04", "D05", "D07")
} else {
  vars.adapt <- c("D01", "D02", "D03", "D04", "D05", "D07", "Count_fire")
  vars.adapt.p <- c("D01", "D02", "D03", "D04", "D05", "D07", "Count")
  vars.adapt <- c("D01", "D02", "D03", "D04", "D05", "D07")
  vars.adapt.p <- c("D01", "D02", "D03", "D04", "D05", "D07")
  vars.adapt.p <- factor(vars.adapt.p, levels = vars.adapt.p)
  names(vars.adapt.p) <- vars.adapt
}

# vars.adapt <- vars.adapt[4]

files.var.sel <- paste0(file.var.sel.prefix, vars.adapt, ".rds")
var.sel.exists <- file.exists(files.var.sel)

vars.adapt <- vars.adapt[var.sel.exists]
files.var.sel <- files.var.sel[var.sel.exists]

sel.res.adapt <- list()
for(i in seq_along(vars.adapt)) {
  sel.res.adapt[[i]] <- readRDS(files.var.sel[i])
}



ci.alpha <- 2 * pnorm(-1)
ci.type <- "upper"
diff.pct <- 0
# ci.alpha <- 2 * pnorm(-1)
# ci.type <- "lower"
# diff.pct <- 0.5
# ci.alpha <- 2 * pnorm(-1)
# ci.type <- "upper"
# diff.pct <- 0.1
sel.res.sum.l <- list()
sel.res.size <- integer()
sel.res.vars <- integer()
for(i in seq_along(vars.adapt)) {
  nfeat_baseline <-
    projpred:::get_nfeat_baseline(sel.res.adapt[[i]],
                                  baseline = "best",
                                  stat = "elpd")
  compare.diff <-
    projpred:::.tabulate_stats(sel.res.adapt[[i]],
                               stats = "elpd",
                               alpha = ci.alpha,
                               nfeat_baseline = nfeat_baseline,
                               deltas = true) |>
    as.data.table()
  compare.elpd <-
    projpred:::.tabulate_stats(sel.res.adapt[[i]],
                               stats = "elpd",
                               alpha = ci.alpha) |>
    as.data.table()
  compare.dt <-
    merge(
          compare.elpd[, .(size,
                           elpd = value,
                           elpd.lq = lq,
                           elpd.uq = uq,
                           elpd.se = se)],
          compare.diff[, .(size,
                           diff = value,
                           diff.lq = lq,
                           diff.uq = uq,
                           diff.se = se)])
  compare.dt[order(size),
             `:=`(expl = c(NA, ranking(sel.res.adapt[[i]])$fulldata, NA),
                  cv_prop = c(NA, diag(cv_proportions(sel.res.adapt[[i]], cumulate = TRUE)), NA))]
  sel.res.sum.l[[i]] <- compare.dt
  ref.u <- diff.pct * (compare.dt[size == 0, elpd] - compare.dt[, max(elpd)])
  if(ci.type == "lower") {
    sel.res.size[i] <- compare.dt[diff.lq >= ref.u, min(size)]
  }
  if(ci.type == "upper") {
    sel.res.size[i] <- compare.dt[diff.uq >= ref.u, min(size)]
  }
}


sel.sizes <-
  data.table(resp = vars.adapt,
             size.sel = sel.res.size)

names(sel.res.sum.l) <- vars.adapt
sel.res.sum <-
  rbindlist(sel.res.sum.l, idcol = "resp") |>
  merge(sel.sizes)

sel.res.sum[,
            selected := fcase(size <= size.sel & size != 0, TRUE,
                              size > size.sel & size != Inf, FALSE,
                              default = NA)]

sel.res.sum <- 
  merge(sel.res.sum,
        sel.res.sum[, .(count.sel = sum(selected, na.rm = TRUE)), by = "expl"],
        by = "expl", all.x = TRUE)

sel.res.sum[is.na(selected), count.sel := NA]

setcolorder(sel.res.sum, c("resp", "size", "size.sel", "expl", "selected", "count.sel", "cv_prop"))

sel.res.sum[, resp := factor(resp, levels = vars.adapt)]
setorder(sel.res.sum, resp, size)

saveRDS(sel.res.sum, file.var.sel.res)


## PREPARE FOR PLOTTING ###############################################


vars.adapt <- variables[category.adaptation == TRUE, sort(code)]
vars.pred <- variables[code %notin% vars.adapt, code]


vars.pred.lik <-
  variables[code %in% vars.pred & cat.ord == TRUE &
            cat.scale %in% c("d", "i", "l", "n", "m"),
            code]
vars.pred.cont <- variables[code %in% vars.pred &
                            type == "continuous",
                            code]

poly.names <- c("L", "Q", "C")
var.codes.poly <-
  CJ(code = c(vars.pred.lik, vars.pred.cont),
     poly.id = 1:3)
var.codes.poly[, code.poly := paste0(code, "_p", poly.id)]
var.codes.poly[, poly.name := poly.names[poly.id]]
var.codes.poly[, code.name := paste0(code, " ", poly.name)]

var.sel <- 
  c(var.codes.poly[code.poly %in% sel.res.sum[!is.na(expl), unique(expl)],
                   unique(code)],
    vars.pred) |>
  unique()

var.cat <-
  variables[code %in% var.sel] |>
    melt(id.vars = "code",
         measure.vars =
           measure(category, pattern = "category.(.*)"),
        value.name = "cat") |>
    _[,
      .SD[cat == 1,
          .(cat.double = fifelse(.N > 1, TRUE, FALSE),
            cat1 = category[1], cat2 = category[2])],
      by = .(code)]

var.cat.poly <- merge(var.cat, var.codes.poly, all.x = TRUE)
var.cat.poly[is.na(code.poly), code.poly := code]
setnames(var.cat.poly, "code.poly", "expl")

sel.res.p <-
  merge(sel.res.sum, var.cat.poly, all.x = TRUE, by = "expl", sort = FALSE) |>
  _[size <= 25 | size == Inf]
sel.res.p[, resp := vars.adapt.p[as.character(resp)]]
sel.res.p[, cat.mult := ifelse(cat.double, "multiple", cat1)]

cat.lev <- c("personal_stakes", "threat_appraisal", "coping_appraisal", "control")
cat.lab <- c("Personal stakes", "Threat appraisal", "Coping appraisal", "Control")
cat.lab <- factor(cat.lab, levels = cat.lab)
names(cat.lab) <- cat.lev

cat.lev.mult <- c(cat.lev, "multiple")
cat.lab.mult <- c(as.character(cat.lab), "Multiple")
cat.lab.mult <- factor(cat.lab.mult, levels = cat.lab.mult)
names(cat.lab.mult) <- cat.lev.mult

sel.res.p[,
          `:=`(cat1.lab = cat.lab[cat1],
               cat2.lab = cat.lab[cat2],
               cat.lab.mult = cat.lab.mult[cat.mult])]

cat.labels <-
  sel.res.p[,
            .(cat1.lab,
              cat2.lab,
              cat.lab.mult,
              expl,
              code.name,
              # expl = ifelse(is.na(expl),
              #               NA,
              #               # paste0(expl, "   ")),
              #               paste0(expl, " ")),
              size,
              size.sel,
              count.sel,
              y = rep(min(diff.lq), .N)),
            by = "resp"
            ][!is.na(expl)] |>
  copy()

cat.labels[is.na(code.name), code.name := expl]
code.width <- max(stri_width(cat.labels$code.name)) + 1

cat.labels[,
           code.name := stri_pad_right(code.name,
                                       width = code.width)]

ref.lines <-
  rbind(
        sel.res.p[size == 0,
                  .(type = rep("Null model", .N), yint = diff, col = "black"),
                  by = "resp"],
        sel.res.p[size == Inf,
                  .(type = rep("Full model", .N), yint = diff, col = "black"),
                  by = "resp"],
        sel.res.p[,
                  .(type = rep("Best model", .N), yint = max(diff), col = "red"),
                  by = "resp"]
        # , sel.res.p[size == 0,
        #           .(type = rep("Acceptance threshold", .N), yint = diff.pct * diff, col = "red"),
        #           by = "resp"]
  )

ref.lines[, type := factor(type, levels = c("Best model",
                                            "Full model",
                                            "Null model"
                                            # , "Acceptance threshold"
                                            ))] 


sel.res.ex <- copy(sel.res.p)
sel.res.ex <-
  sel.res.ex[,
             .(resp, size, size.sel, selected, count.sel, expl,
               category = cat.lab.mult,
               diff, diff.lq, diff.uq)]

fwrite(sel.res.ex, file.var.sel.res.csv)

cairo_pdf(file.var.sel.plot, onefile = TRUE, width = 11, height = 5.65)


ggplot(sel.res.p[size < Inf]) +
  geom_rect(data = sel.res.p[,
                               .(xmin = size.sel[1] + 0.5,
                                 xmax = Inf,
                                 ymin = -Inf,
                                 ymax = Inf),
                               by = "resp"],
            mapping = aes(xmin = xmin,
                          xmax = xmax,
                          ymin = ymin,
                          ymax = ymax),
            fill = "grey90") +
  geom_hline(data = ref.lines[type != "Best model"],
             mapping = aes(yintercept = yint, linetype = type),
             linewidth = 0.2) +
  geom_hline(data = ref.lines[type == "Best model"],
             mapping = aes(yintercept = yint),
             linewidth = 0.2, color = 2) +
  # geom_hline(data = ref.lines[type != "Acceptance threshold"],
  #            mapping = aes(yintercept = yint, linetype = type),
  #            linewidth = 0.2) +
  # geom_hline(data = ref.lines[type == "Acceptance threshold"],
  #            mapping = aes(yintercept = yint),
  #            linewidth = 0.2, color = 2) +
  geom_line(aes(x = size,
                y = diff,
                group = resp),
            linewidth = 0.35) +
  geom_linerange(aes(x = size,
                     ymin = diff.lq,
                     ymax = diff.uq),
                 linewidth = 0.35) +
  geom_point(data = sel.res.p[size <= size.sel & !is.na(expl)],
             mapping = aes(x = size, y = diff,
                           # fill = cat1.lab
                           ), 
             shape = 21, fill = 1, size = 1.5) +
  geom_point(data = sel.res.p[size > size.sel & size != Inf],
             mapping = aes(x = size, y = diff), 
             shape = 21, fill = 1, size = 0.75) +
  geom_point(data = sel.res.p[size == 0],
             mapping = aes(x = size, y = diff), 
             shape = 21, fill = "white", size = 2) +
  geom_text(data = cat.labels[size <= size.sel],
            aes(x = size,
                y = y,
                label = code.name,
                color = cat.lab.mult),
            # family = base.family,
            family = "IBMPlexMono",
            fontface = "bold",
            size = base.size/4,
            angle = 90,
            hjust = 1,
            key_glyph = "point") +
  geom_text(data = cat.labels[size > size.sel & count.sel >= 1],
            aes(x = size,
                y = y,
                label = code.name),
            # family = base.family,
            family = "IBMPlexMono",
            fontface = "bold",
            size = base.size/4,
            angle = 90,
            hjust = 1) +
  geom_text(data = cat.labels[size > size.sel & count.sel == 0],
            aes(x = size,
                y = y,
                label = code.name),
            # family = base.family,
            family = "IBMPlexMono",
            size = base.size/4,
            angle = 90,
            hjust = 1) +
  scale_y_continuous(expand = expansion(c(0.275, 0.1), 0)) +
  scale_linetype_manual(values = c("Null model" = "dotted",
                                   "Full model" = "dashed",
                                   "Best model" = "solid"
                                   # ,"Acceptance threshold" = "solid"
                                   ),
                        drop = FALSE) +
  scale_fill_brewer(type = "qual", palette = "Set1",
                    aesthetics = c("fill", "colour"),
                    drop = FALSE) +
  guides(linetype = guide_legend(order = 1, override.aes = list(colour = c(2, 1, 1)))) +
  # guides(linetype = guide_legend(order = 1, override.aes = list(colour = c(1, 1, 1, 2)))) +
  facet_wrap(vars(resp), ncol = 3, scales = "free_y") +
  labs(x = "Model size (number of terms)",
       y = "Model performance (difference in ELPD vs. best model)",
       linetype = "Reference models",
       colour = "Category") +
  plot_theme +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "inch"))

dev.off()

sel.res.sum[size <= size.sel & !is.na(expl) & resp != "Count",
            sort(unique(expl))]


# sel.res.sum[size <= size.sel & !is.na(expl),
#             sort(unique(expl))]
