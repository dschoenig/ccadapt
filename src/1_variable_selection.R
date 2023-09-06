library(data.table)
library(stringi)
library(brms)
library(projpred)
library(ggplot2)
library(ggrepel)

source("paths.R")
source("utilities.R")

options(mc.cores = 4)



base.size <- 10
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

# vars.adapt <- variables[category.adaptation == TRUE, sort(code)]
vars.adapt <- c(variables[category.adaptation == TRUE, sort(code)], "Count")

files.var.sel <- paste0(file.var.sel.prefix, vars.adapt, ".rds")
var.sel.exists <- file.exists(files.var.sel)

vars.adapt <- vars.adapt[var.sel.exists]
files.var.sel <- files.var.sel[var.sel.exists]

sel.res.adapt <- list()
for(i in seq_along(vars.adapt)) {
  sel.res.adapt[[i]] <- readRDS(files.var.sel[i])
}


# ci.alpha <- 2 * pnorm(-1)
ci.alpha <- 2*pnorm(-1)
ci.type <- "lower"
diff.pct <- 0.5
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

sel.res.sum[size <= size.sel & size != 0, count.sel := .N, by = "expl"]
setcolorder(sel.res.sum, c("resp", "size", "expl", "cv_prop"))


sel.res.sum[, resp := factor(resp, levels = vars.adapt)]
setorder(sel.res.sum, resp)

fwrite(sel.res.sum, file.var.sel.res.csv)
saveRDS(sel.res.sum, file.var.sel.res)


## PREPARE FOR PLOTTING ###############################################


var.cat <-
  variables[code %in% sel.res.sum[!is.na(expl), unique(expl)]] |>
    melt(id.vars = "code",
         measure.vars =
           measure(category, pattern = "category.(.*)"),
        value.name = "cat") |>
    _[,
      .SD[cat == 1,
          .(cat.double = fifelse(.N > 1, TRUE, FALSE),
            cat1 = category[1], cat2 = category[2])],
      by = .(expl = code)]

sel.res.p <- merge(sel.res.sum, var.cat, all.x = TRUE, by = "expl", sort = FALSE)
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
              expl = ifelse(is.na(expl),
                            NA,
                            paste0(expl, "   ")),
              size,
              size.sel,
              y = rep(min(diff.lq), .N)),
            by = "resp"
            ][!is.na(expl)]

ref.lines <-
  rbind(
        sel.res.p[size == 0,
                  .(type = rep("Null model", .N), yint = diff, col = "black"),
                  by = "resp"],
        sel.res.p[size == Inf,
                  .(type = rep("Full model", .N), yint = diff, col = "black"),
                  by = "resp"],
        sel.res.p[,
                  .(type = rep("Best model", .N), yint = max(diff), col = "black"),
                  by = "resp"],
        sel.res.p[size == 0,
                  .(type = rep("Acceptance threshold", .N), yint = diff/2, col = "red"),
                  by = "resp"])

ref.lines[, type := factor(type, levels = c("Best model",
                                            "Full model",
                                            "Null model",
                                            "Acceptance threshold"))] 


ggplot(sel.res.p) +
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
  # geom_hline(data = sel.res.p[, .(int.line = min(diff)), by = "resp"],
  #            mapping = aes(yintercept = int.line),
  #            linetype = "dotted") +
  geom_hline(data = ref.lines[type != "Acceptance threshold"],
             mapping = aes(yintercept = yint, linetype = type),
             linewidth = 0.2) +
  geom_hline(data = ref.lines[type == "Acceptance threshold"],
             mapping = aes(yintercept = yint),
             linewidth = 0.2, color = 2) +
  geom_line(aes(x = size,
                y = diff,
                group = resp)) +
  geom_linerange(aes(x = size,
                     ymin = diff.lq,
                     ymax = diff.uq)) +
  geom_point(data = sel.res.p[size <= size.sel & !is.na(expl)],
             mapping = aes(x = size, y = diff,
                           # fill = cat1.lab
                           ), 
             shape = 21, fill = 1, size = 2) +
  geom_point(data = sel.res.p[size > size.sel],
             mapping = aes(x = size, y = diff), 
             shape = 21, fill = 1, size = 1) +
  geom_point(data = sel.res.p[size == 0],
             mapping = aes(x = size, y = diff), 
             shape = 21, fill = "white", size = 2) +
  geom_text(data = cat.labels[size <= size.sel],
            aes(x = size,
                y = y,
                label = expl,
                color = cat.lab.mult),
            family = base.family,
            # fontface = "bold",
            size = base.size/4,
            angle = 90,
            hjust = 1,
            key_glyph = "point") +
  geom_text(data = cat.labels[size > size.sel],
            aes(x = size,
                y = y,
                label = expl),
            family = base.family,
            size = base.size/4,
            angle = 90,
            hjust = 1) +
  # geom_point(data = sel.res.p[size <= size.sel],
  #            mapping = aes(x = size, y = diff), 
  #            shape = 21, fill = 1) +
  scale_y_continuous(expand = expansion(c(0.225, 0.1), 0)) +
  scale_linetype_manual(values = c("Null model" = "dotted",
                                   "Full model" = "dashed",
                                   "Best model" = "solid",
                                   "Acceptance threshold" = "solid"),
                        drop = FALSE) +
  scale_fill_brewer(type = "qual", palette = "Set1",
                    aesthetics = c("fill", "colour")) +
  # guides(colour = guide_legend(override.aes=list(shape = 16, size = 1,  label = ""))) +
  guides(linetype = guide_legend(order = 1, override.aes = list(colour = c(1, 1, 1, 2)))) +
  facet_wrap(vars(resp), ncol = 3, scales = "free_y") +
  labs(x = "Model size (number of terms)",
       y = "Difference in ELPD vs. best model",
       linetype = "Reference models",
       colour = "Category") +
  plot_theme


# sel.res.sum[size <= size.sel & !is.na(expl) & resp != "Count",
#             sort(unique(expl))]

# sel.res.sum[size <= size.sel & !is.na(expl),
#             sort(unique(expl))]
