# =============================================================================
# EID Figure 1 v18 — R translation from Python matplotlib
# Panels: A (coverage bars), B (quarantine timeline + A* inset),
#         C (histogram + B* inset), D (cumulative outbreak curve)
# =============================================================================

rm(list = ls())

# --- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
  library(cowplot)
  library(scales)
  library(grid)
  library(gridExtra)
  library(lubridate)
})

# --- File paths (adjust to your local paths) ---------------------------------
data_dir <- "."  # Change this to your working directory

all_schools       <- read_csv(file.path(data_dir, "spartanburg_all_schools_exposure_status.csv"),
                              show_col_types = FALSE)
quarantine_31     <- read_csv(file.path(data_dir, "all_31_schools_quarantine_data.csv"),
                              show_col_types = FALSE)
outbreak_timeline <- read_csv(file.path(data_dir, "outbreak_timeline_data.csv"),
                              show_col_types = FALSE)
quarantine_by_round <- read_csv(file.path(data_dir, "school_quarantine_by_round.csv"),
                                show_col_types = FALSE)
quarantine_temporal <- read_csv(file.path(data_dir, "quarantine_temporal.csv"),
                                show_col_types = FALSE)

# --- Colour palettes ---------------------------------------------------------
col_round <- c("1" = "#3498DB", "2" = "#F39C12", "3" = "#E74C3C")
col_incr  <- "#8E44AD"   # purple  — increasing trajectory
col_reinf <- "#16A085"   # teal    — cleared then reinfected
col_decr  <- "#D4AC0D"   # dark gold — decreasing trajectory

date_min <- as.Date("2025-10-01")
date_max <- as.Date("2026-02-22")
break_start <- as.Date("2025-12-20")
break_end   <- as.Date("2026-01-05")

# =============================================================================
# PANEL A — Horizontal bar chart of 31 schools' coverage (inverted x)
# =============================================================================

q31 <- quarantine_31 %>%
  arrange(desc(Coverage_Percent)) %>%
  mutate(
    y_pos  = row_number(),
    Rounds = factor(Rounds)
  )

p_A <- ggplot(q31, aes(x = Coverage_Percent, y = reorder(School, Coverage_Percent))) +
  geom_col(aes(fill = Rounds), width = 0.72, alpha = 0.85,
           colour = "black", linewidth = 0.3) +
  geom_text(aes(x = Coverage_Percent - 5, label = paste0(round(Coverage_Percent), "%")),
            colour = "white", fontface = "bold", size = 3, hjust = 0.5) +
  geom_vline(xintercept = 90, linetype = "dashed", colour = "orange",
             linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = 95, linetype = "dashed", colour = "green",
             linewidth = 1, alpha = 0.8) +
  scale_x_reverse(limits = c(102, 15), breaks = seq(20, 100, 10)) +
  scale_fill_manual(
    values = col_round,
    labels = c("1" = "1 quarantine round",
               "2" = "2 quarantine rounds",
               "3" = "3 quarantine rounds"),
    guide = guide_legend(order = 1)
  ) +
  labs(x = "MMR Vaccination Coverage (%)", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.y      = element_blank(),
    axis.ticks.y     = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position  = c(0.02, 0.02),
    legend.justification = c(0, 0),
    legend.background = element_rect(fill = alpha("white", 0.95), colour = NA),
    legend.text       = element_text(size = 9),
    plot.title        = element_text(face = "bold", size = 14),
    axis.title.x      = element_text(face = "bold", size = 13)
  )

# =============================================================================
# PANEL B — Quarantine timeline (Gantt-style) with school labels
# =============================================================================

# Prepare school ordering: sort by first quarantine date (desc), then rounds
q31_t <- quarantine_31 %>%
  mutate(First_Date = as.Date(First_Quarantine_Date)) %>%
  arrange(desc(First_Date), Rounds) %>%
  mutate(y_pos = row_number())

# School name matching map (Python `em` dict)
name_map <- c(
  "Boiling Springs Elem"   = "Boiling Springs Elementary",
  "Boiling Springs High"   = "Boiling Springs High",
  "Boiling Springs Middle"  = "Boiling Springs Middle",
  "Fairforest Elementary"  = "Fairforest Elementary",
  "Global Academy"         = "Global Academy of SC",
  "Holly Springs-Motlow"   = "Holly Springs-Motlow Elem",
  "Cooley Springs-Finger"  = "Cooley Springs-Fingerville Elem",
  "Sugar Ridge Elem"       = "Sugar Ridge Elementary",
  "Berry Shoals Elem"      = "Berry Shoals Elementary",
  "New Prospect Elem"      = "New Prospect Elementary",
  "Campobello-Gramling"    = "Campobello-Gramling School",
  "T.E. Mabry Middle"      = "T. E. Mabry Middle"
)

# Build a lookup from school name -> y position
school_ypos <- setNames(q31_t$y_pos, q31_t$School)

# Fuzzy match function
match_school_name <- function(name, targets) {
  # Try exact name map first
  if (name %in% names(name_map)) {
    mapped <- name_map[name]
    if (mapped %in% targets) return(mapped)
  }
  if (name %in% targets) return(name)
  # Fuzzy prefix match (remove spaces, hyphens, dots)
  clean <- function(s) tolower(gsub("[\\s\\-\\.]", "", s))
  nc <- clean(name)
  best <- NA_character_
  best_len <- 0
  for (t in targets) {
    tc <- clean(t)
    match_len <- 0
    for (i in seq_len(min(nchar(nc), nchar(tc)))) {
      if (substr(nc, i, i) == substr(tc, i, i)) match_len <- match_len + 1
      else break
    }
    if (match_len > best_len && match_len >= 10) {
      best_len <- match_len
      best <- t
    }
  }
  return(best)
}

# Prepare quarantine round data
qr <- quarantine_by_round %>%
  mutate(Date = as.Date(Date))

# Match each quarantine round school to the y-position list
qr$matched_school <- sapply(qr$School, function(s) {
  match_school_name(s, names(school_ypos))
})
qr <- qr %>% filter(!is.na(matched_school))
qr$y_pos <- school_ypos[qr$matched_school]

# Each quarantine bar spans 21 days
q_duration <- 21

# Y-axis labels: "School (n susceptibles)"
y_labels <- q31_t %>%
  mutate(label = paste0(School, " (", Susceptibles, ")")) %>%
  select(y_pos, label, Rounds)

p_B <- ggplot() +
  # Winter break shading
  annotate("rect",
           xmin = break_start, xmax = break_end,
           ymin = 0.3, ymax = nrow(q31_t) + 0.7,
           fill = "gray", alpha = 0.15) +
  # Quarantine bars
  geom_rect(data = qr,
            aes(xmin = Date, xmax = Date + q_duration,
                ymin = y_pos - 0.3, ymax = y_pos + 0.3,
                fill = factor(Total_Rounds)),
            alpha = 0.7, colour = "black", linewidth = 0.2) +
  # Student counts inside bars

  geom_text(data = qr,
            aes(x = Date + q_duration / 2, y = y_pos,
                label = Students_Quarantined),
            colour = "white", fontface = "bold", size = 2.5) +
  scale_fill_manual(
    values = col_round,
    labels = c("1" = "1 round", "2" = "2 rounds", "3" = "3 rounds"),
    name = NULL
  ) +
  scale_x_date(limits = c(date_min, date_max),
               date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(
    breaks = y_labels$y_pos,
    labels = y_labels$label,
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  # Term labels at top
  annotate("text", x = as.Date("2025-10-25"), y = nrow(q31_t) + 1.5,
           label = "Fall Term", fontface = "italic", size = 3.8) +
  annotate("text", x = as.Date("2025-12-28"), y = nrow(q31_t) + 1.5,
           label = "Break", fontface = "italic", size = 3.8, colour = "gray50") +
  annotate("text", x = as.Date("2026-02-05"), y = nrow(q31_t) + 1.5,
           label = "Spring Term", fontface = "italic", size = 3.8) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 9),
    panel.grid.minor = element_blank(),
    legend.position  = c(0.05, 0.82),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = alpha("white", 0.95), colour = NA),
    legend.text = element_text(size = 8),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )

# Bold y-axis labels for multi-round schools
bold_idx <- y_labels %>% filter(Rounds >= 2) %>% pull(y_pos)
# (Applied via theme element after combining — see final assembly)

# =============================================================================
# PANEL B INSET (A*) — Quarantine trajectories for multi-round schools
# =============================================================================

# Identify multi-round schools
ms2 <- quarantine_31 %>% filter(Rounds >= 2) %>% pull(School)

# Match temporal data schools to multi-round schools
temporal <- quarantine_temporal %>%
  mutate(Date = as.Date(Date))

match_temporal_to_ms2 <- function(ts, ms2_list) {
  clean <- function(s) tolower(gsub("[\\s]", "", s))
  tsc <- clean(ts)
  for (ms in ms2_list) {
    msc <- clean(ms)
    if (grepl(substr(tsc, 1, 12), msc, fixed = TRUE) ||
        grepl(substr(msc, 1, 12), tsc, fixed = TRUE)) {
      return(ms)
    }
  }
  return(NA_character_)
}

temporal$matched <- sapply(temporal$School, match_temporal_to_ms2, ms2_list = ms2)
temporal_multi <- temporal %>% filter(!is.na(matched))

# Classify each school trajectory
classify_trajectory <- function(df) {
  vals <- df$Quarantined
  has_zero <- 0 %in% vals
  last_val <- tail(vals, 1)
  first_nonzero <- vals[vals > 0][1]
  if (is.na(first_nonzero)) first_nonzero <- 0

  if (has_zero & last_val > 0) return("Cleared→Reinf. (31%)")
  if (last_val > first_nonzero) return("Increasing (54%)")
  return("Decreasing (15%)")
}

trajectory_class <- temporal_multi %>%
  group_by(matched) %>%
  arrange(Date) %>%
  summarise(category = classify_trajectory(cur_data()), .groups = "drop")

temporal_multi <- temporal_multi %>%
  left_join(trajectory_class, by = "matched")

traj_colors <- c(
  "Increasing (54%)"      = col_incr,
  "Cleared→Reinf. (31%)"  = col_reinf,
  "Decreasing (15%)"      = col_decr
)

p_Astar <- ggplot(temporal_multi,
                  aes(x = Date, y = Quarantined,
                      group = matched, colour = category)) +
  geom_line(linewidth = 0.9, alpha = 0.7) +
  geom_point(size = 1.8, alpha = 0.7) +
  scale_colour_manual(values = traj_colors, name = NULL) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  labs(x = "Date", y = "Students in Quarantine") +
  annotate("text", x = -Inf, y = Inf, label = "(A*)", hjust = -0.1, vjust = 1.2,
           fontface = "bold", size = 3.5) +
  theme_minimal(base_size = 7) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 1.2),
    plot.background  = element_rect(fill = "white", colour = NA),
    legend.position  = c(0.95, 0.95),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = alpha("white", 0.9), colour = NA),
    legend.key.size  = unit(0.4, "cm"),
    legend.text      = element_text(size = 6),
    axis.title       = element_text(size = 7),
    axis.text        = element_text(size = 5.5),
    plot.margin      = margin(3, 3, 3, 3)
  )

# =============================================================================
# PANEL C — Histogram of coverage: all schools vs exposure sites
# =============================================================================

hist_all <- all_schools %>%
  filter(!is.na(Immunizations)) %>%
  mutate(Coverage = Immunizations * 100)

hist_exp <- all_schools %>%
  filter(!is.na(Immunizations), Is_Exposure_Site == TRUE) %>%
  mutate(Coverage = Immunizations * 100)

bins <- seq(20, 100, by = 5)

p_C <- ggplot() +
  geom_histogram(data = hist_all,
                 aes(x = Coverage), breaks = bins,
                 fill = "#95A5A6", alpha = 0.6,
                 colour = "black", linewidth = 0.3) +
  geom_histogram(data = hist_exp,
                 aes(x = Coverage), breaks = bins,
                 fill = "#E67E22", alpha = 0.8,
                 colour = "black", linewidth = 0.3) +
  geom_vline(xintercept = 90, linetype = "dashed", colour = "orange",
             linewidth = 1, alpha = 0.8) +
  geom_vline(xintercept = 95, linetype = "dashed", colour = "green",
             linewidth = 1, alpha = 0.8) +
  scale_x_reverse(limits = c(102, 15)) +
  # Legend via manual dummy layers
  annotate("rect", xmin = 99, xmax = 101, ymin = -Inf, ymax = -Inf,
           fill = "#95A5A6", alpha = 0.6) +
  labs(x = "MMR Vaccination Coverage (%)",
       y = "Number of Schools") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 13),
    axis.text  = element_text(size = 11),
    plot.margin = margin(5, 5, 5, 5)
  )

# Add manual legend text + gap box as annotation (positioned at plot level)
# We'll add these with annotation_custom in the final assembly

# =============================================================================
# PANEL C INSET (B*) — % schools becoming exposure sites by coverage band
# =============================================================================

bands <- tibble(
  label = c("<80%", "80-85%", "85-90%", "90-95%", "≥95%"),
  lo    = c(0, 80, 85, 90, 95),
  hi    = c(80, 85, 90, 95, 101)
)

band_data <- bands %>%
  rowwise() %>%
  mutate(
    total = if (hi == 101) {
      sum(all_schools$Immunizations * 100 >= lo, na.rm = TRUE)
    } else {
      sum(all_schools$Immunizations * 100 >= lo &
          all_schools$Immunizations * 100 < hi, na.rm = TRUE)
    },
    exposed = if (hi == 101) {
      sum(all_schools$Immunizations * 100 >= lo &
          all_schools$Is_Exposure_Site == TRUE, na.rm = TRUE)
    } else {
      sum(all_schools$Immunizations * 100 >= lo &
          all_schools$Immunizations * 100 < hi &
          all_schools$Is_Exposure_Site == TRUE, na.rm = TRUE)
    },
    pct = ifelse(total > 0, exposed / total * 100, 0)
  ) %>%
  ungroup() %>%
  mutate(label = factor(label, levels = label))

band_colors <- c("<80%" = "#E74C3C", "80-85%" = "#F39C12",
                 "85-90%" = "#F1C40F", "90-95%" = "#2ECC71",
                 "≥95%" = "#27AE60")

p_Bstar <- ggplot(band_data, aes(x = label, y = pct, fill = label)) +
  geom_col(alpha = 0.8, colour = "black", linewidth = 0.3, width = 0.7) +
  geom_text(aes(label = paste0(round(pct), "%\n(n=", total, ")")),
            vjust = -0.3, size = 2.5, fontface = "bold") +
  scale_fill_manual(values = band_colors, guide = "none") +
  scale_y_continuous(limits = c(0, 90), expand = expansion(mult = c(0, 0.1))) +
  labs(x = NULL, y = "% Schools Became\nExposure Sites") +
  annotate("text", x = -Inf, y = Inf, label = "(B*)", hjust = -0.1, vjust = 1.2,
           fontface = "bold", size = 3.5) +
  theme_minimal(base_size = 7) +
  theme(
    panel.background = element_rect(fill = "white", colour = "black", linewidth = 1.2),
    plot.background  = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(size = 6, angle = 20, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title.y = element_text(size = 7),
    plot.margin = margin(3, 3, 3, 3)
  )

# =============================================================================
# PANEL D — Cumulative outbreak cases
# =============================================================================

ob <- outbreak_timeline %>%
  mutate(Date = as.Date(Date))

last_row <- ob %>% slice_tail(n = 1)

# Build gradient fill beneath the line: approximate with many narrow rectangles
# We use geom_ribbon with varying alpha via multiple layers
n_grad <- 50
y_max <- max(ob$Total_Cases, na.rm = TRUE)

gradient_layers <- list()
for (i in seq_len(n_grad)) {
  y_lo <- y_max * (i - 1) / n_grad
  y_hi <- y_max * i / n_grad
  a <- 0.02 + 0.25 * ((i - 1) / n_grad)
  gradient_layers[[i]] <- geom_ribbon(
    data = ob,
    aes(x = Date,
        ymin = pmax(y_lo, 0),
        ymax = pmin(Total_Cases, y_hi)),
    fill = "#E74C3C", alpha = a
  )
}

p_D <- ggplot(ob, aes(x = Date, y = Total_Cases)) +
  # Winter break shading
  annotate("rect",
           xmin = break_start, xmax = break_end,
           ymin = -Inf, ymax = Inf,
           fill = "gray", alpha = 0.15) +
  # Gradient fill
  gradient_layers +
  # Line + points
  geom_line(colour = "#C0392B", linewidth = 1.2) +
  geom_point(colour = "#E74C3C", fill = "#E74C3C", shape = 21,
             size = 3, stroke = 0.5) +
  # Last data point annotation
  annotate("text",
           x = last_row$Date - 25,
           y = last_row$Total_Cases - 80,
           label = paste0("Feb 3:\n", last_row$Total_Cases, " cases"),
           fontface = "bold.italic", size = 3.8, colour = "#C0392B") +
  annotate("segment",
           x = last_row$Date - 15,
           xend = last_row$Date - 2,
           y = last_row$Total_Cases - 50,
           yend = last_row$Total_Cases - 5,
           arrow = arrow(length = unit(0.15, "cm")),
           colour = "#C0392B", linewidth = 0.7) +
  # Key event annotations
  annotate("text", x = as.Date("2025-10-22"), y = 250,
           label = "Oct 10:\nFirst quarantine",
           fontface = "italic", size = 3.3, colour = "gray30") +
  annotate("segment",
           x = as.Date("2025-10-18"), xend = as.Date("2025-10-11"),
           y = 230, yend = 15,
           arrow = arrow(length = unit(0.12, "cm")),
           colour = "gray50", linewidth = 0.7) +
  annotate("text", x = as.Date("2025-11-12"), y = 350,
           label = "Oct 31:\nFirst release",
           fontface = "italic", size = 3.3, colour = "gray30") +
  annotate("segment",
           x = as.Date("2025-11-08"), xend = as.Date("2025-11-01"),
           y = 330, yend = 25,
           arrow = arrow(length = unit(0.12, "cm")),
           colour = "gray50", linewidth = 0.7) +
  annotate("text", x = as.Date("2026-01-05"), y = 550,
           label = "Jan 16-30:\n31 schools quarantined",
           fontface = "italic", size = 3.3, colour = "gray30") +
  annotate("segment",
           x = as.Date("2026-01-12"), xend = as.Date("2026-01-22"),
           y = 570, yend = 690,
           arrow = arrow(length = unit(0.12, "cm")),
           colour = "gray50", linewidth = 0.7) +
  # Term labels above plot
  annotate("text", x = as.Date("2025-10-25"), y = 950,
           label = "Fall Term", fontface = "italic", size = 4.2) +
  annotate("text", x = as.Date("2025-12-28"), y = 950,
           label = "Break", fontface = "italic", size = 4.2, colour = "gray50") +
  annotate("text", x = as.Date("2026-02-05"), y = 950,
           label = "Spring Term", fontface = "italic", size = 4.2, colour = "#C0392B") +
  scale_x_date(limits = c(date_min, date_max),
               date_breaks = "1 month", date_labels = "%b %d") +
  scale_y_continuous(limits = c(0, 960), expand = expansion(mult = 0)) +
  coord_cartesian(clip = "off") +
  labs(x = "Date", y = "Cumulative Outbreak Cases") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold", size = 13),
    axis.text  = element_text(size = 11),
    plot.margin = margin(t = 20, r = 5, b = 5, l = 5)
  )

# =============================================================================
# FINAL ASSEMBLY — 4 panels + 2 insets + panel labels
# =============================================================================

# Create the base 4-panel layout using patchwork
# Top row: A + B (taller); Bottom row: C + D
# B and D share x-axis range (both date axes, Oct 1 – Feb 22)

layout <- (p_A | p_B) / (p_C | p_D) +
  plot_layout(heights = c(1.3, 1), widths = c(1, 1.2))

# --- Use cowplot::ggdraw to overlay insets and panel labels -------------------

final <- ggdraw() +
  draw_plot(layout, x = 0, y = 0, width = 1, height = 1) +

  # Panel labels (top-left, outside each panel box)
  draw_label("A", x = 0.005, y = 0.99, fontface = "bold", size = 20,
             hjust = 0, vjust = 1) +
  draw_label("B", x = 0.44, y = 0.99, fontface = "bold", size = 20,
             hjust = 0, vjust = 1) +
  draw_label("C", x = 0.005, y = 0.435, fontface = "bold", size = 20,
             hjust = 0, vjust = 1) +
  draw_label("D", x = 0.44, y = 0.435, fontface = "bold", size = 20,
             hjust = 0, vjust = 1) +

  # Inset A* in Panel B (lower-left of B)
  draw_plot(p_Astar,
            x = 0.47, y = 0.44,    # position within the full canvas
            width = 0.22, height = 0.18) +

  # Inset B* in Panel C (right side of C)
  draw_plot(p_Bstar,
            x = 0.24, y = 0.12,
            width = 0.17, height = 0.17) +

  # C legend labels (manual, since overlapping histograms don't auto-legend well)
  draw_label("■ All Spartanburg schools",
             x = 0.25, y = 0.14, size = 9, hjust = 0, colour = "#95A5A6") +
  draw_label("■ Outbreak exposure sites",
             x = 0.25, y = 0.12, size = 9, hjust = 0, colour = "#E67E22") +

  # Gap box annotation in C
  draw_label("Spartanburg gap: 6.4 pp\nOutbreak mean: 82.5%",
             x = 0.26, y = 0.08, size = 9, fontface = "bold", hjust = 0)

# =============================================================================
# SAVE
# =============================================================================

ggsave("EID_Figure1_v18.png", final, width = 26, height = 18,
       dpi = 300, bg = "white", units = "in")
ggsave("EID_Figure1_v18.pdf", final, width = 26, height = 18,
       bg = "white", units = "in")

cat("EID Figure 1 v18 (R version) saved!\n")
