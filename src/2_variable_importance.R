library(data.table)
library(stringi)
library(ggplot2)
library(ggdist)

source("paths.R")
source("utilities.R")

rf.imp.cat <- fread(file.rf.catimp)
rf.imp.var <- fread(file.rf.varimp)


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
        strip.text = element_text(size = rel(0.8),
                                  hjust = 0.5,
                                  color = "black",
                                  margin = margin(base.size/2,
                                                  base.size/2,
                                                  base.size/2,
                                                  base.size/2)),
        strip.background = element_rect(fill = "gray90", colour = NA))


resp.var <- c(paste0("D", 1:10), "Count")

cat.lev <- c("personal_stakes", "threat_appraisal", "coping_appraisal", "control")
cat.lab <- c("Personal stakes", "Threat appraisal", "Coping appraisal", "Control")
cat.n <- rf.imp.var[resp == "D1", .(n = .N), by = category][order(category), n]
cat.lab.n <- unname(mapply(\(x, y) paste0(x, " [", y, "]"), cat.lab, cat.n))

rf.imp.cat[, category := factor(category, levels = cat.lev)]
rf.imp.cat[, resp := factor(resp, levels = resp.var)]

rf.imp.var[, category := factor(category, levels = cat.lev)]
rf.imp.var[, resp := factor(resp, levels = resp.var)]



cairo_pdf(file.plot.catimp, width = 8.5, height = 11)

ggplot(rf.imp.var) +
# rf.imp.var[expl %in% rf.imp.var[selected == TRUE, unique(expl)]] |>
# ggplot() +
  stat_boxplot(aes(x = importance.full,
                   y = category,
                   color = category),
               coef = 1.5,
               outlier.size = 2,
               outlier.stroke = 0.8,
               outlier.shape = 21,
               outlier.fill = "#FFFFFF55",
               width = 0.5) +
  # geom_jitter(aes(x = importance.full,
  #                 y = category,
  #                 colour = category),
  #             alpha = 0.5, shape = 16) +
  scale_y_discrete(limits = rev, labels = rev(cat.lab.n)) +
  # scale_x_continuous(limits = c(0, NA), expand = expansion(0.1, 0)) +
  # scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
  scale_colour_brewer(type = "qual", palette = "Set1") +
  facet_wrap(vars(resp), ncol = 3, scales = "free_x") +
  # facet_wrap(vars(resp), ncol = 3) +
  guides(colour = "none") +
  labs(y = NULL, x = "Variable importance",
       # subtitle = resp[i]
       ) +
  plot_theme +
  theme(aspect.ratio = 0.65) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "inch"))

dev.off()


# cairo_pdf(file.plot.catimp, width = 8.5, height = 11)

# ggplot(rf.imp.cat,
#        aes(x = category)) +
#   geom_bar(aes(x = importance.max,
#                y = category,
#                fill = category),
#            stat = "identity",
#            alpha = 0.75) +
#   geom_bar(aes(x = importance.median,
#                y = category,
#                fill = category),
#            stat = "identity") +
#   geom_bar(aes(x = importance.median,
#                y = category),
#            fill = "black",
#            stat = "identity",
#            alpha = 0.2) +
#   scale_y_discrete(limits = rev, labels = rev(cat.lab)) +
#   # scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
#   scale_fill_brewer(type = "qual", palette = "Set1") +
#   facet_wrap(vars(resp), ncol = 3, scales = "free_x") +
#   guides(fill = "none") +
#   labs(y = NULL, x = "Variable importance",
#        # subtitle = resp[i]
#        ) +
#   plot_theme +
#   theme(aspect.ratio = 0.65) +
#   theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "inch"))

# dev.off()



# rf.imp.cat[, cat.n.lab := paste0("[", cat.n, "]   ")]

# cairo_pdf(file.plot.catimp, width = 8.5, height = 11)

# ggplot(rf.imp.cat) +
#   # geom_text(aes(x = importance.min,
#   #               y = category,
#   #               label = cat.n.lab),
#   #           hjust = 1,
#   #           family = "IBMPlexSansCondensed",
#   #           size = 2.5) +
#   geom_segment(aes(x = importance.min,
#                    xend = importance.max,
#                    y = category,
#                    yend = category,
#                    colour = category),
#                linewidth = 0.75) +
#   geom_segment(aes(x = importance.min,
#                    xend = importance.min,
#                    y = (5-as.integer(category))-0.25,
#                    yend = (5-as.integer(category))+0.25,
#                    colour = category),
#                linewidth = 0.7) +
#   geom_segment(aes(x = importance.max,
#                    xend = importance.max,
#                    y = (5-as.integer(category))-0.25,
#                    yend = (5-as.integer(category))+0.25,
#                    colour = category),
#                linewidth = 0.7) +
#   geom_point(aes(x = importance.median,
#                   y = category,
#                   colour = category),
#              shape = 21, stroke = 1, size = 2,
#              fill = "white"
#              ) +
#   # geom_point(aes(x = importance.median,
#   #                 y = category,
#   #                 colour = category,
#   #                 size = as.numeric(cat.n)),
#   #            shape = 21, stroke = 1, fill = "white") +
#   scale_y_discrete(limits = rev, labels = rev(cat.lab)) +
#   scale_x_continuous(limits = c(NA, NA), expand = expansion(0.3, 0)) +
#   # scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
#   scale_colour_brewer(type = "qual", palette = "Set1") +
#   facet_wrap(vars(resp), ncol = 3, scales = "free_x") +
#   # facet_wrap(vars(resp), ncol = 3) +
#   scale_size(range = c(1, 5)) +
#   guides(colour = "none") +
#   labs(y = NULL, x = "Variable importance",
#        # subtitle = resp[i]
#        ) +
#   plot_theme +
#   theme(aspect.ratio = 0.65) +
#   theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "inch"))

# dev.off()


cairo_pdf(file.plot.catimp.sel, width = 8.5, height = 11)

ggplot(rf.imp.var[selected == TRUE]) +
  geom_point(aes(x = importance.sel,
               y = category,
               colour = category),
             shape = 21, fill = "white", size = 2, stroke = 1) +
  scale_y_discrete(limits = rev, labels = rev(cat.lab)) +
  scale_x_continuous(limits = c(NA, NA), expand = expansion(0.1, 0)) +
  # scale_x_continuous(trans = "sqrt", breaks = scales::breaks_pretty(6)) +
  scale_colour_brewer(type = "qual", palette = "Set1") +
  # facet_wrap(vars(resp), ncol = 3, scales = "free_x") +
  facet_wrap(vars(resp), ncol = 3) +
  scale_size(range = c(1, 5)) +
  guides(colour = "none") +
  labs(y = NULL, x = "Variable importance",
       # subtitle = resp[i]
       ) +
  plot_theme +
  theme(aspect.ratio = 0.65) +
  theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "inch"))

dev.off()
