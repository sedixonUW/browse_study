#port blakely analysis

library(faraway)
library(dplyr)
library(arm)
library(generalhoslem)
library(tidyverse)
library(readxl)
#install.packages("janitor")
library(janitor)
library(ggplot2)
library(readxl)
library(patchwork)
library(ggpattern)


PB_2011_survey <- read_excel("./analysis_ready_data/port_blakely/Port Blakely Cedar Info.xlsx",
                       sheet = "raw_data")

#For tubed unit, planting date: 2/2/2021
#we took measurements on 10_20_2025 (4 years old)
PB_2025_tubed <- read_excel("./analysis_ready_data/port_blakely/10_20_2025_Tubed_Unit_data.xlsx",
                             sheet = "reformatted")

table(PB_2011_survey$Survival)

#############
#1.1 2011 survival
#############

PB_2011_survey <- PB_2011_survey %>%
  filter(Species == "WRC")
table(PB_2011_survey)

cont_survival_2011 <- table(PB_2011_survey$Treatment, PB_2011_survey$Survival)
mosaicplot(cont_survival_2011)

chi_survival_2011 <- chisq.test(cont_survival_2011) #very sig
p_survival_2011 <- chi_survival_2011$p.value
fisher.test(cont_survival_2011) #very sig

#############
#1.2 2025 Averages at 4 year mark
#############
PB_2025_tubed <- PB_2025_tubed %>%
  filter(!height_ft == "dead")

PB_2025_tubed$height_ft <- as.numeric(PB_2025_tubed$height_ft)
mean(PB_2025_tubed$height_ft)

#############
#2 plotting 2011 survival
#############

format_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 3)))
}
PB_2011_survey$Treatment
# Colors for Pair Plant and Vexar
PB_2011_colors <- c("SS Co Planting" = "#E69F00", "Fence" = "#009E73")

# 1.1 Deformity
PB_survival <- PB_2011_survey |>
  group_by(Treatment) |>
  summarise(pct_survival = mean(Survival == "Alive", na.rm = TRUE) * 100) |>
  ggplot(aes(x = Treatment, y = pct_survival, fill = Treatment)) +
  geom_bar_pattern(stat = "identity", show.legend = FALSE,
                   pattern = "stripe",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black") +
  scale_fill_manual(values = PB_2011_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1.34, y = 97,
           label = format_p(p_survival_2011),
           color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "", y = "Survival (%)") +
  theme_bw() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.text    = element_text(size = 12)
  )

PB_survival

# Save 
ggsave(
  filename = "figures/PB_survival.png",
  plot = PB_survival, dpi = 800, width = 8, height = 8, units = "in"
)
