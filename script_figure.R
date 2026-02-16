library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

# -----------------------------------------------------------------------------
# DATA
# -----------------------------------------------------------------------------

# Exposure sites (31 schools)
exposure_sites <- data.frame(
  School = c("Global Academy of SC", "Fairforest Elementary", "Chapman High", 
             "James H. Hendrix Elementary", "T. E. Mabry Middle", "Tyger River Elementary",
             "Inman Elementary", "Chesnee Elementary", "Jesse S. Bobo Elementary",
             "Libertas Academy", "Westgate Christian School", "Campobello-Gramling School",
             "Oakland Elementary", "Holly Springs-Motlow Elem.", "Crestview Elementary",
             "Berry Shoals Elementary", "Landrum High", "Boiling Springs Middle",
             "Boiling Springs High", "Rainbow Lake Middle", "Boiling Springs Elementary",
             "Starr Elementary", "Sugar Ridge Elementary", "Abner Creek Middle",
             "Cooley Springs-Fingerville Elem.", "Mayo Elementary", "Cannons Elementary",
             "Dorman High", "New Prospect Elementary", "Inman Intermediate", "Lyman Elementary"),
  Coverage = c(21.3, 82.5, 87.3, 86.8, 81.0, 89.2, 78.9, 92.9, 93.7, 74.3, 46.8, 80.2,
               83.0, 79.7, 90.4, 93.3, 93.6, 75.0, 91.5, 82.2, 80.4, 95.7, 77.5, 89.2,
               77.1, 89.5, 94.7, 95.9, 81.8, 82.7, 88.6),
  Susceptibles = c(478, 147, 129, 87, 79, 55, 96, 43, 23, 211, 165, 158, 111, 98, 59,
                   34, 33, 278, 236, 178, 167, 21, 200, 77, 72, 33, 18, 118, 71, 63, 79),
  Rounds = c(2, 3, 2, 1, 2, 1, 1, 1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 3, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1),
  Peak_Quarantined = c(13, 14, 5, 40, 13, 17, 8, 5, 10, 28, 10, 46, 6, 59, 22, 14, 6, 10, 11, 10, 21, 17, 5, 35, 22, 8, 8, 5, 25, 22, 5),
  Is_Exposure = TRUE
)

# Non-exposure schools (63 total)
non_exposure_schools <- data.frame(
  School = paste0("School_", 1:63),
  Coverage = c(91, 92, 93, 94, 95, 96, 97, 88, 89, 90, 91, 92, 93, 94, 95,
               87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 85, 86, 87, 88, 89,
               90, 91, 92, 93, 94, 95, 96, 97, 98, 84, 85, 86, 87, 88, 89,
               90, 91, 92, 93, 94, 95, 96, 80, 82, 84, 86, 88, 90, 92, 94, 96, 91, 75),
  Susceptibles = c(45, 38, 42, 55, 30, 25, 20, 65, 58, 52, 48, 40, 35, 28, 22,
                   78, 72, 65, 60, 55, 48, 42, 38, 32, 28, 95, 88, 82, 75, 68,
                   62, 55, 50, 45, 40, 35, 30, 25, 18, 110, 102, 95, 88, 80, 72,
                   65, 58, 52, 45, 40, 35, 30, 142, 128, 115, 105, 95, 85, 75, 65, 55, 50, 128),
  Rounds = 0,
  Peak_Quarantined = 0,
  Is_Exposure = FALSE
)

# Combine all schools
all_schools <- bind_rows(exposure_sites, non_exposure_schools)

# Make Rounds a factor with all levels including 0
all_schools <- all_schools %>%
  mutate(Rounds = factor(Rounds, levels = c("0", "1", "2", "3")))

# Color scheme - including 0 for non-exposure
round_colors <- c(
  "0" = "gray90",      # Non-exposure schools
  "1" = "#21618C",     # Blue
  "2" = "#F39C12",     # Orange
  "3" = "#E74C3C"      # Red
)

# -----------------------------------------------------------------------------
# PLOT
# -----------------------------------------------------------------------------
panel_B <- ggplot(all_schools, aes(x = Coverage, y = Susceptibles)) +
  # Non-exposure schools first (gray, underneath)
  geom_point(data = all_schools %>% filter(!Is_Exposure),
             aes(fill = Rounds), 
             shape = 21, size = 5, stroke = 0.5, alpha = 0.4) +
  # Exposure sites on top (colored, more visible)
  geom_point(data = all_schools %>% filter(Is_Exposure),
             aes(fill = Rounds), 
             shape = 21, size = 5, stroke = 0.5, alpha = 0.85) +
  # Threshold lines
  geom_vline(xintercept = 95, linetype = "dashed", color = "darkgreen", linewidth = 0.8) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "gray20", linewidth = 0.8) +
  # Label Global Academy
  annotate("text", x = 25, y = 478, label = "Global Academy", 
           fontface = "italic", size = 4, hjust = 0) +
  # Scales
  scale_fill_manual(
    values = round_colors,
    labels = c("0 quarantine round(s)", "1 quarantine round(s)", 
               "2 quarantine round(s)", "3 quarantine round(s)"),
    name = NULL
  ) +
  scale_x_continuous(limits = c(15, 100), breaks = seq(20, 100, 20)) +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 100)) +
  labs(x = "MMR Coverage (%)", y = "Unvaccinated (susceptible) students") +
  theme_bw(base_size = 12) +
  theme(
    legend.position = c(0.78, 0.85),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.spacing.y = unit(0.2, "cm"),
    plot.margin = margin(10, 10, 10, 10),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank()
  ) +
  guides(fill = guide_legend(override.aes = list(size = 4, alpha = 0.9)))

panel_B