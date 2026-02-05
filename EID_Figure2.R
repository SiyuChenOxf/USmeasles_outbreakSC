# =============================================================================
# EID Figure 2 v1 — R translation from Python matplotlib
# Panels: A (stacked bars), B (scatter coverage × pool × rounds),
#         C (penetration vs pool size), D (coverage boxplot), E (susceptibles boxplot)
# Layout: Top row (A wider left, B wider right)
#         Bottom row (C left, D center, E right)
# =============================================================================

rm(list = ls())

# --- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(cowplot)
  library(scales)
  library(grid)
  library(ggrepel)
})

# --- File paths (adjust to your local paths) ---------------------------------
data_dir <- "."  # Change this to your working directory

exposure_df <- read_csv(file.path(data_dir, "exposure_sites_Re_analysis.csv"),
                        show_col_types = FALSE) %>%
  mutate(
    Coverage = Immunizations * 100,
    Proportion_Quarantined = Quarantined / Unvaccinated * 100,
    Proportion_Remaining   = 100 - Proportion_Quarantined,
    Rounds_f = factor(Rounds)
  )

# --- Colour palette -----------------------------------------------------------
col_round <- c("1" = "#3498DB", "2" = "#F39C12", "3" = "#E74C3C")

# =============================================================================
# A. Stacked Bar — Quarantine Removes Only a Fraction
# =============================================================================

df_bar <- exposure_df %>%
  arrange(Unvaccinated) %>%
  mutate(
    school_label = paste0(str_trunc(`Display Name`, 22, ellipsis = "…"),
                          " (", Unvaccinated, ")"),
    school_label = fct_inorder(school_label)
  ) %>%
  pivot_longer(cols = c(Proportion_Quarantined, Proportion_Remaining),
               names_to = "status", values_to = "pct") %>%
  mutate(status = factor(status,
                         levels = c("Proportion_Quarantined", "Proportion_Remaining"),
                         labels = c("Quarantined (protected temporarily)",
                                    "Remaining susceptibles (still at risk)")))

# Find Global Academy row for annotation
ga_bar <- exposure_df %>%
  arrange(Unvaccinated) %>%
  mutate(y_num = row_number()) %>%
  filter(str_detect(`Display Name`, "Global"))

p_A <- ggplot(df_bar, aes(x = pct, y = school_label, fill = status)) +
  geom_col(width = 0.75, alpha = 0.7, colour = NA) +
  scale_fill_manual(
    values = c("Quarantined (protected temporarily)" = "#3498DB",
               "Remaining susceptibles (still at risk)" = "#E74C3C"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(0, 100), expand = expansion(mult = 0)) +
  {
    if (nrow(ga_bar) > 0) {
      annotate("label",
               x = 50, y = max(1, ga_bar$y_num[1] - 4),
               label = "Global Academy:\nOnly 2.7% quarantined\n97.3% still susceptible",
               fontface = "bold", size = 3.5,
               fill = "lightyellow", colour = "red",
               label.size = 0.8)
    }
  } +
  labs(
    x = "Percentage of Susceptible Students",
    y = NULL,
    title = "A. Quarantine Removes Only a Fraction",
    subtitle = "Most susceptibles remain at risk"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(face = "bold", size = 12),
    axis.text.x = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = c(0.98, 0.02),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = alpha("white", 0.95), colour = NA),
    legend.text = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# B. Scatter — Coverage × Pool Size × Rounds
# =============================================================================

# Labels for multi-round schools
multi_labels <- exposure_df %>%
  filter(Rounds >= 2) %>%
  mutate(
    short_name = `Display Name` %>%
      str_replace(" Elementary", " Elem") %>%
      str_replace(" Academy - Boiling Springs", "") %>%
      str_replace(" School", "") %>%
      str_replace(" of SC", "") %>%
      str_trunc(20, ellipsis = ""),
    label_text = paste0(short_name, " [", Rounds, "×]")
  )

# Global Academy label
ga_scatter <- exposure_df %>% filter(str_detect(`Display Name`, "Global"))

p_B <- ggplot(exposure_df, aes(x = Coverage, y = Unvaccinated)) +
  # High risk zone shading

  annotate("rect", xmin = 15, xmax = 90, ymin = 100, ymax = 520,
           fill = "red", alpha = 0.08) +
  annotate("label", x = 52, y = 470,
           label = "HIGH RISK ZONE\n(Low coverage + Large susceptible pool)",
           fontface = "bold", size = 3.8, colour = "red",
           fill = "lightyellow", label.colour = "red", label.size = 0.5) +
  # Critical mass line
  geom_hline(yintercept = 100, linetype = "dotted", colour = "red",
             linewidth = 0.7, alpha = 0.6) +
  annotate("text", x = 18, y = 115,
           label = "Critical mass\n(~100 susceptibles)",
           fontface = "italic", size = 3.5, colour = "red", hjust = 0) +
  # Threshold lines
  geom_vline(xintercept = 90, linetype = "dashed", colour = "orange",
             linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = 95, linetype = "dashed", colour = "green",
             linewidth = 1, alpha = 0.8) +
  # Points
  geom_point(aes(colour = Rounds_f), size = 4, alpha = 0.7,
             shape = 21, fill = NA, stroke = 1) +
  geom_point(aes(fill = Rounds_f), size = 3.5, alpha = 0.7,
             shape = 21, colour = "black", stroke = 0.5) +
  # Multi-round labels
  geom_text_repel(
    data = multi_labels,
    aes(label = label_text),
    size = 3, fontface = "bold",
    nudge_x = 3, nudge_y = -5,
    segment.size = 0.3, segment.alpha = 0.5,
    max.overlaps = 20
  ) +
  # Global Academy special label
  {
    if (nrow(ga_scatter) > 0) {
      annotate("text",
               x = ga_scatter$Coverage[1] + 5,
               y = ga_scatter$Unvaccinated[1] - 15,
               label = "Global Academy [2×]",
               fontface = "bold", size = 3.8, colour = "red")
    }
  } +
  scale_colour_manual(values = col_round, guide = "none") +
  scale_fill_manual(
    values = col_round,
    labels = c("1" = "1 round", "2" = "2 rounds", "3" = "3 rounds"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(15, 100)) +
  scale_y_continuous(limits = c(0, 520)) +
  labs(
    x = "MMR Vaccination Coverage (%)",
    y = "Unvaccinated (Susceptible) Students",
    title = "B. Susceptible Pool Size Predicts Quarantine Recurrence",
    subtitle = "31 Exposure Sites, South Carolina Measles Outbreak"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.95), colour = NA),
    legend.text = element_text(size = 10),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# C. Penetration vs Pool Size (Bottom-left)
# =============================================================================

# Fit log decay curve
log_func <- function(x, a, b) a - b * log(x)
fit <- tryCatch({
  nls(Proportion_Quarantined ~ a - b * log(Unvaccinated),
      data = exposure_df, start = list(a = 100, b = 20))
}, error = function(e) NULL)

fit_line <- if (!is.null(fit)) {
  tibble(x = seq(15, 500, length.out = 100)) %>%
    mutate(y = pmin(100, pmax(0, predict(fit, newdata = list(Unvaccinated = x)))))
} else NULL

# Correlation
cor_test <- cor.test(exposure_df$Unvaccinated, exposure_df$Proportion_Quarantined)

# Global Academy for annotation
ga_pen <- exposure_df %>% filter(str_detect(`Display Name`, "Global"))

p_C <- ggplot(exposure_df, aes(x = Unvaccinated, y = Proportion_Quarantined)) +
  # Fit line
  {
    if (!is.null(fit_line)) {
      geom_line(data = fit_line, aes(x = x, y = y),
                colour = "red", linetype = "dashed", linewidth = 1, alpha = 0.7,
                inherit.aes = FALSE)
    }
  } +
  # Mean threshold line
  geom_hline(yintercept = 20, linetype = "dotted", colour = "gray50",
             linewidth = 0.7, alpha = 0.5) +
  annotate("text", x = 350, y = 23,
           label = "Mean observed", fontface = "italic",
           size = 3.3, colour = "gray50") +
  # Points (size ~ Unvaccinated)
  geom_point(aes(fill = Rounds_f, size = Unvaccinated),
             shape = 21, alpha = 0.7, colour = "black", stroke = 0.5) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  scale_fill_manual(
    values = col_round,
    labels = c("1" = "1 round", "2" = "2 rounds", "3" = "3 rounds"),
    name = NULL
  ) +
  # Correlation annotation
  annotate("label", x = 300, y = 87,
           label = paste0("r = ", round(cor_test$estimate, 2),
                          "\np = ", format.pval(cor_test$p.value, digits = 3)),
           fontface = "bold", size = 4,
           fill = "white", label.colour = "black", label.size = 0.5) +
  # Global Academy annotation
  {
    if (nrow(ga_pen) > 0) {
      list(
        annotate("label", x = 300, y = 20,
                 label = "Global Academy",
                 fontface = "bold", size = 3.5,
                 fill = "lightyellow", colour = "red", label.size = 0.5),
        annotate("segment",
                 x = 350, xend = ga_pen$Unvaccinated[1] + 10,
                 y = 17, yend = ga_pen$Proportion_Quarantined[1] + 2,
                 arrow = arrow(length = unit(0.15, "cm")),
                 colour = "red", linewidth = 0.8)
      )
    }
  } +
  scale_x_continuous(limits = c(0, 520)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = "Unvaccinated Students at School",
    y = "Quarantine Penetration (%)",
    title = "C. Contact Tracing Capacity Limit",
    subtitle = "Penetration drops as susceptible pool grows"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 11),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.95), colour = NA),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# D. Coverage Boxplot by Rounds (Bottom-center)
# =============================================================================

# Stats: single vs multi
single_cov <- exposure_df %>% filter(Rounds == 1) %>% pull(Coverage)
multi_cov  <- exposure_df %>% filter(Rounds >= 2) %>% pull(Coverage)
wt_cov <- wilcox.test(single_cov, multi_cov)

round_n <- exposure_df %>%
  count(Rounds) %>%
  mutate(x_label = paste0(Rounds, " Round", ifelse(Rounds > 1, "s", ""),
                          "\n(n=", n, ")"))

set.seed(42)

p_D <- ggplot(exposure_df, aes(x = Rounds_f, y = Coverage)) +
  # Boxplots
  geom_boxplot(aes(fill = Rounds_f), width = 0.6, alpha = 0.4,
               colour = "black", outlier.shape = NA) +
  # Jittered points
  geom_jitter(aes(colour = Rounds_f), width = 0.08, size = 2.5,
              alpha = 0.7, shape = 16) +
  # Threshold lines
  geom_hline(yintercept = 95, linetype = "dashed", colour = "green",
             linewidth = 1, alpha = 0.8) +
  geom_hline(yintercept = 90, linetype = "dashed", colour = "orange",
             linewidth = 1, alpha = 0.8) +
  # Stats annotation
  annotate("label",
           x = 2.2, y = 40,
           label = paste0("Single vs Multiple:\n",
                          round(mean(single_cov) - mean(multi_cov), 1),
                          " pp difference\np = ",
                          format.pval(wt_cov$p.value, digits = 3)),
           fontface = "bold", size = 3.8,
           fill = "white", label.colour = "black", label.size = 0.5) +
  scale_fill_manual(values = col_round, guide = "none") +
  scale_colour_manual(values = col_round, guide = "none") +
  scale_x_discrete(labels = setNames(round_n$x_label, round_n$Rounds)) +
  scale_y_continuous(limits = c(15, 100)) +
  labs(
    x = NULL,
    y = "MMR Vaccination Coverage (%)",
    title = "D. Lower Coverage →",
    subtitle = "More Repeated Quarantines"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# E. Susceptibles Boxplot by Rounds (Bottom-right)
# =============================================================================

single_sus <- exposure_df %>% filter(Rounds == 1) %>% pull(Unvaccinated)
multi_sus  <- exposure_df %>% filter(Rounds >= 2) %>% pull(Unvaccinated)
wt_sus <- wilcox.test(single_sus, multi_sus)

ga_box <- exposure_df %>% filter(str_detect(`Display Name`, "Global"))

set.seed(42)

p_E <- ggplot(exposure_df, aes(x = Rounds_f, y = Unvaccinated)) +
  # Boxplots
  geom_boxplot(aes(fill = Rounds_f), width = 0.6, alpha = 0.4,
               colour = "black", outlier.shape = NA) +
  # Jittered points
  geom_jitter(aes(colour = Rounds_f), width = 0.08, size = 2.5,
              alpha = 0.7, shape = 16) +
  # Stats annotation
  annotate("label",
           x = 1.8, y = 440,
           label = paste0("Single vs Multiple:\n",
                          round(mean(multi_sus) - mean(single_sus), 0),
                          " more susceptibles\np = ",
                          format.pval(wt_sus$p.value, digits = 3)),
           fontface = "bold", size = 3.8,
           fill = "white", label.colour = "black", label.size = 0.5) +
  # Global Academy annotation
  {
    if (nrow(ga_box) > 0) {
      annotate("text", x = 2.55, y = 460,
               label = "Global\nAcademy",
               fontface = "bold", size = 3.5, colour = "red")
    }
  } +
  scale_fill_manual(values = col_round, guide = "none") +
  scale_colour_manual(values = col_round, guide = "none") +
  scale_x_discrete(labels = setNames(round_n$x_label, round_n$Rounds)) +
  labs(
    x = NULL,
    y = "Number of Susceptible Students",
    title = "E. Larger Susceptible Pool →",
    subtitle = "More Repeated Quarantines"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    plot.title    = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# =============================================================================
# ASSEMBLY — Matching Python gridspec layout
# Top row:    A (5/12 width) | B (6/12 width)
# Bottom row: C (4/12)       | D (4/12) | E (4/12)
# =============================================================================

top_row <- p_A + p_B +
  plot_layout(widths = c(5, 6))

bottom_row <- p_C + p_D + p_E +
  plot_layout(widths = c(4, 4, 4))

final <- top_row / bottom_row +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Figure 2. Why Reactive Quarantine Fails to Reduce Transmission",
    subtitle = paste0(
      "Quarantine temporarily protects susceptibles without conferring immunity; ",
      "upon release, they return to a larger outbreak still susceptible"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
    )
  )

# =============================================================================
# SAVE
# =============================================================================

ggsave("EID_Figure2_OrigLayout.png", final,
       width = 22, height = 14, dpi = 300, bg = "white", units = "in")
ggsave("EID_Figure2_OrigLayout.pdf", final,
       width = 22, height = 14, bg = "white", units = "in")

cat("EID Figure 2 (R version) saved!\n")
