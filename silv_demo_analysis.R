#Silv demo small fenced units

#two major datasets: 2019 and 2024

library(faraway)
library(dplyr)
library(arm)
library(generalhoslem)
library(tidyverse)
library(readxl)
#install.packages("janitor")
library(janitor)
library(ggplot2)
#install.packages("ggpattern")
library(ggpattern)
source("./functions/check_resids_func.R")

library(DHARMa)
silv_d_2019 <- read_excel("./analysis_ready_data/silv_demo_small_fence/Cline_class_20219.xlsx",
                          sheet = "Data")

silv_d_2024 <- read_excel("./analysis_ready_data/silv_demo_small_fence/Silviculture Demo W Data 2024.xlsx",
                          sheet = "All Data")

#########
#1.1 Cleaning up 2019 data 
###########
#using Janitor cleaning up names
silv_d_2019 <- clean_names(silv_d_2019)

silv_d_2019 <- silv_d_2019 %>%
  dplyr::rename(
    plot       = plot_name,
    tree_number    = seedling_no,
    species    = seedling_sp,
    #tree_number_2  = seedling_no_2, not sure what this one is, but in looking at the pivot tables in excel, doesn't actually mean count (they just use the species column for counting)
    height  = seedling_h_tcm,
    diameter = seedling_d_bmm,
    volume = seedling_volumecm_3,
  )

#take out WWP, SS and GF since those were not the focus of the study
silv_d_2019 <- silv_d_2019 %>%
  filter(species %in% c("Western Red Cedar", "Douglas-Fir"))

#fix species names
silv_d_2019 <- silv_d_2019 %>%
  mutate(species = case_when(
    species == "Western Red Cedar" ~ "WRC",
    species == "Douglas-Fir" ~ "DF"
  ))
unique(silv_d_2019$species)

# need to code in treatment column. there is "weededis1" col and "fencedis1" col.
# if both have 1's then F/W, if just F has one, F/NW, etc
silv_d_2019 <- silv_d_2019 %>%
  mutate(treatment = case_when(
    fencedis1 == 1 & weededis1 == 1 ~ "F/W",
    fencedis1 == 1 & weededis1 == 0 ~ "F/NW",
    fencedis1 == 0 & weededis1 == 1 ~ "W",
    fencedis1 == 0 & weededis1 == 0 ~ "NW"
  ))

###################################
#1.2 WRC Survival
###################################
#For WRC survival, they assumed 37 WRC were planted within each treatment within each plot

#maybe make new column or new just WRC dataframe? Leave for now....
#silv_d_2019$survival <- 

###################################
#1.3 Browse 2019
###################################
silv_d_2019$graze1is_yes <- as.factor(silv_d_2019$graze1is_yes)
levels(silv_d_2019$graze1is_yes)

# making NW the control
silv_d_2019$treatment <- relevel(factor(silv_d_2019$treatment), ref = "NW")

silv_d_2019_mod_final <- glmer(graze1is_yes ~ treatment*species + (1|plot), silv_d_2019, family = "binomial")

summary(silv_d_2019_mod_final) #no significance

pval_silv_d_2019_mod_final <- summary(silv_d_2019_mod_final)$coefficients[, "Pr(>|z|)"]

check_resids(silv_d_2019_mod_final)
simulationOutput <- simulateResiduals(silv_d_2019_mod_final)
plot(simulationOutput)
#looks good 

##################
#1.4 Height 2019
####################
#making sure NW is the control
levels(silv_d_2019$treatment)

hist(silv_d_2019$height) #looks normal 
silv_d_2019_ht_mod_final <- lmerTest::lmer((height) ~ treatment*species + (1|plot), silv_d_2019)

summary(silv_d_2019_ht_mod_final)
#weeding decreases height---why?! #maybe look back at plot notes...
#other fenced treatments sig increase height 

check_resids(silv_d_2019_ht_mod_final)
#looks good

pval_silv_d_2019_ht_mod <- summary(silv_d_2019_ht_mod_final)$coefficients[, "Pr(>|t|)"]

##################
#1.5 Diameter 2019
####################
hist(silv_d_2019$diameter)

silv_d_2019_diam_mod_final <- lmerTest::lmer((diameter) ~ treatment*species + (1|plot), silv_d_2019)

summary(silv_d_2019_diam_mod_final)
#F/W increases diameter
#just weeding decreases diameter? Not sure what to make of that...

check_resids(silv_d_2019_diam_mod_final)
#looks good

p_val_silv_d_2019_diam_mod_final <- summary(silv_d_2019_diam_mod_final)$coefficients[, "Pr(>|t|)"]
  
##########################################################
#########
#2.1 Cleaning up 2024 data 
###########
##########################################################
#using Janitor cleaning up names
silv_d_2024 <- clean_names(silv_d_2024)

silv_d_2024 <- silv_d_2024 %>%
  filter(!is.na(treatment))

unique(paste(silv_d_2024$plot, silv_d_2024$treatment, silv_d_2024$tree_number, sep = "-"))
#looking for around 260 

#this drops extra rows for deformities; should leave one row for each tree
silv_d_2024 <- silv_d_2024 %>%
  filter(!is.na(species)) #that worked

##########
#2.2 Deformity models
###########

silv_d_2024$deformity_y_n <- as.factor(silv_d_2024$deformity_y_n)
levels(silv_d_2024$deformity_y_n)

#for this measurement only, dropping 3 NA deform values because those trees were not measured 
#bees nest was in the way 
silv_d_2024_deform <- silv_d_2024 %>%
  filter(!is.na(deformity_y_n)) #that worked

head(silv_d_2024_deform)

#making NW the control
silv_d_2024_deform$treatment <- relevel(factor(silv_d_2024_deform$treatment), ref = "NW")

silv_d_2024_deform_mod_final <- glmer(deformity_y_n ~ treatment*species + (1|plot), silv_d_2024_deform, family = "binomial")

summary(silv_d_2024_deform_mod_final)

#Positive coefficient → that treatment has higher odds of deformity compared to the reference
#treatment F/NW sig reduced deformity
#treatment F/W sig reduced deformity 

check_resids(silv_d_2024_deform_mod_final)
simulationOutput <- simulateResiduals(silv_d_2024_deform_mod_final)
plot(simulationOutput)

p_val_silv_d_2024_deform_mod_final <- summary(silv_d_2024_deform_mod_final)$coefficients[, "Pr(>|z|)"]

##########
#2.3 Rotation Risk
###########

#the same three trees have Na values for rotation risk, so using the same smaller
#dataset here for rotation risk models 
silv_d_2024_deform$rotation_risk_y_n <- as.factor(silv_d_2024_deform$rotation_risk_y_n)
levels(silv_d_2024_deform$rotation_risk_y_n)
colnames(silv_d_2024_deform)

#Need to add weights to this one so that it isn't overdispersed
#redo dataset to have single line for each plot
#plot number
#column for proportion of rotation risk
#column for total number of seedlings in that plot
#coding for treatment 
# silv_d_2024_rotate <- silv_d_2024_deform %>%
#   group_by(plot, treatment, species) %>%
#   summarise(
#     total_seedlings     = n(),
#     prop_rotation_risk  = mean(rotation_risk_y_n == "Y", na.rm = TRUE),
#     .groups = "drop"
#   )

tapply(silv_d_2024_rotate$total_seedlings, silv_d_2024_rotate$treatment, FUN = "mean")
# NW      F/NW       F/W         W 
# 7.666667 13.833333 16.666667  6.600000 
#more you have in the denominator, more precision you have in proportion

#which option is better? Using this one
silv_d_2024_rotate_mod_final <- glmer(rotation_risk_y_n ~ treatment*species + (1|plot), family = "binomial", 
                                      data = silv_d_2024_deform)
#with weights
# silv_d_2024_rotate_mod_final <- glm(prop_rotation_risk ~ treatment*species, family = "quasibinomial",
#                                       data = silv_d_2024_rotate, weights = total_seedlings)
# silv_d_2024_rotate_mod_final <- glmer(prop_rotation_risk ~ treatment+species + (1|plot), family = "binomial", 
#                                       data = silv_d_2024_rotate, weights = total_seedlings)

summary(silv_d_2024_rotate_mod_final) #but take with a grain of salt considering the other ones came up not significant
#F/NW sig reduced rotation risk
#treatment F/W sig reduced rotation risk
#weeding did not

anova(silv_d_2024_rotate_mod_final) #shows that treatment F value is large, could be significant
check_resids(silv_d_2024_rotate_mod_final)
#dharma check resids
simulationOutput <- simulateResiduals(silv_d_2024_rotate_mod_final)
plot(simulationOutput)
#DEVIATION IS SIG HERE

table(silv_d_2024_deform$treatment, silv_d_2024_deform$rotation_risk_y_n)

p_val_silv_d_2024_rotate_mod_final <- summary(silv_d_2024_rotate_mod_final)$coefficients[, "Pr(>|z|)"]

##################
#2.4 Height 2024
####################

#making NW the control for this larger dataset
silv_d_2024$treatment <- relevel(factor(silv_d_2024$treatment), ref = "NW")

hist(silv_d_2024$height_m)
silv_d_2024_ht_mod_final <- lmerTest::lmer(height_m ~ treatment*species + (1|plot), silv_d_2024)

summary(silv_d_2024_ht_mod_final) 
#both F/NW and F/W sig improved height, weeding had an even greater effect on height

check_resids(silv_d_2024_ht_mod_final)
#looks good to me

p_val_silv_d_2024_ht_mod_final <- summary(silv_d_2024_ht_mod_final)$coefficients[, "Pr(>|t|)"]

##################
#2.5 Diameter 2024
####################
hist(silv_d_2024$diameter_mm) #maybe more gamma than normal?

silv_d_2024diam_df <- silv_d_2024 %>%
  filter(!grepl("Diameter|measured at|diam @ |m @ 0\\.5|m @ 0\\.25|m @ 0\\.9|measurements at 0\\.5|measurements at 0\\.25|measurements at 0\\.9|D at 0\\.|D taken at|D, DfV|Diameter taken at", 
                comments, ignore.case = TRUE))

silv_d_2024_diam_mod_final <- lmerTest::lmer(diameter_mm ~ treatment*species + (1|plot), silv_d_2024diam_df)
#AIC(silv_d_2024_diam_mod) #with random effects better

summary(silv_d_2024_diam_mod_final) #same pattern as above with diameter: F and F/Nw better
anova(silv_d_2024_diam_mod_final) #treatments differ from each other regardless, and species effect differs by treatment
check_resids(silv_d_2024_diam_mod_final)#also looks okay to assume normal here?

#interaction.plot(silv_d_2024$treatment, silv_d_2024$species, silv_d_2024$diameter_mm)

p_val_silv_d_2024_diam_mod_final <- summary(silv_d_2024_diam_mod_final)$coefficients[, "Pr(>|t|)"]


##################
#2.6 Number of Leaders 2024
####################
silv_d_2024
n_leaders_mod_2024 <- glmer(number_of_leaders ~ treatment*species + (1|plot), silv_d_2024, family = poisson) #log transformed is sig!

summary(n_leaders_mod_2024) #not sig
anova(n_leaders_mod_2024) #
check_resids(n_leaders_mod_2024)#pretty funky 

sum(residuals(n_leaders_mod_2024, type= "pearson")^2) / df.residual(n_leaders_mod_2024)
# Ratio >> 1 suggests overdispersion, but it is less than 1, we're okay

############################################################################
##################
# 5) Plotting 
##################
#############################################################################
# ─────────────────────────────────────────────
# Shared helpers
# ─────────────────────────────────────────────

my_theme <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 13),
    axis.title.y = element_text(size = 13),
    axis.text    = element_text(size = 11),
    axis.text.x  = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text  = element_text(size = 11),
    legend.key    = element_rect(fill = "white", color = NA)  # add this line
  )

treatment_colors <- c(
  "NW"   = "#0072B2",
  "W"    = "#E69F00",
  "F/NW" = "#009E73",
  "F/W"  = "#CC79A7"
)

treatment_labels <- c(
  "NW"   = "No Fence /\nNo Weed",
  "W"    = "No Fence /\nWeed",
  "F/NW" = "Fence /\nNo Weed",
  "F/W"  = "Fence /\nWeed"
)

species_pattern <- c("DF" = "none", "WRC" = "stripe")
species_labels  <- c("DF" = "Douglas-fir", "WRC" = "western redcedar")

# Ensure species is a factor with DF first (solid) then WRC (hatched)
silv_d_2019 <- silv_d_2019 |>
  mutate(species = factor(species, levels = c("DF", "WRC")))

# 5.1) Browse 2019 (1 year and 9 months)
browse_summary_2019 <- silv_d_2019 |>
  group_by(treatment, species) |>
  summarise(pct_browse = mean(graze1is_yes == 1, na.rm = TRUE) * 100,
            .groups = "drop")

p_browse_2019 <- ggplot(browse_summary_2019,
                        aes(x = treatment, y = pct_browse,
                            fill = treatment, pattern = species)) +
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.7,
                   pattern_fill  = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black",
                   show.legend = TRUE) +
  annotate("text", x = 2.5, y = 90,
           label = "All effects: ns",
           size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "", y = "Browse (%) in Year 2", pattern = "") +
  my_theme
  # guides(pattern = guide_legend(override.aes = list(fill = "white")))


# 5.2) Height 2019 by species
summary(silv_d_2019_ht_mod_final)
p_ht_2019 <- ggplot(silv_d_2019,
                    aes(x = treatment, y = height,
                        fill = treatment, pattern = species)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       pattern_fill  = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black",
                       show.legend = TRUE) +
  annotate("text", x = 2.5, y = 202,
           label = "All treatment & species effects p < 0.001",
           size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  labs(x = "", y = "Height (cm) in Year 2", title = "", pattern = "") +
  my_theme + theme(legend.position = "none")


summary(silv_d_2019_diam_mod_final)

# 5.3) Diameter 2019 by species
p_diam_2019 <- ggplot(silv_d_2019,
                      aes(x = treatment, y = diameter,
                          fill = treatment, pattern = species)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       pattern_fill  = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black",
                       show.legend = TRUE) +
  annotate("text", x = 2.5, y = max(silv_d_2019$diameter, na.rm = TRUE) * 1.12,
           label = "Treatment W x WRC interaction: ns",
           size = 4, hjust = 0.5) +
  annotate("text", x = 2.5, y = max(silv_d_2019$diameter, na.rm = TRUE) * 1.06,
           label = "All other effects p < 0.05",
           size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  labs(x = "", y = "Diameter (mm) in Year 2", title = "", pattern = "") +
  my_theme+ theme(legend.position = "none")


# ─────────────────────────────────────────────
# 2024 Plots
# ─────────────────────────────────────────────

# Ensure species is a factor with DF first (solid) then WRC (hatched)
silv_d_2024_deform <- silv_d_2024_deform |>
  mutate(species = factor(species, levels = c("DF", "WRC")))

# 5.4) Deformity 2024 by species
deform_summary_2024 <- silv_d_2024_deform |>
  group_by(treatment, species) |>
  summarise(pct_deform = mean(deformity_y_n == "Y", na.rm = TRUE) * 100, #creating percent deform column 
            .groups = "drop") 

summary(silv_d_2024_deform_mod_final)

p_deform_2024 <- ggplot(deform_summary_2024,
                        aes(x = treatment, y = pct_deform,
                            fill = treatment, pattern = species)) +
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.7,
                   pattern_fill  = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black",
                   show.legend = TRUE) +
  annotate("text", x = 2.5, y = 90,
           label = "All effects: ns",
           size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "", y = "Deformity (%) in Year 7", pattern = "") +
  my_theme

summary(silv_d_2024_rotate_mod_final) #not sig
#NEED TO ADD 

# 5.5) Rotation Risk 2024 by species
rotate_summary_2024 <- silv_d_2024_deform |>
  group_by(treatment, species) |>
  summarise(pct_rotate = mean(rotation_risk_y_n == "Y", na.rm = TRUE) * 100, #making percent rotate risk column
            .groups = "drop")

p_rotate_2024 <- ggplot(rotate_summary_2024,
                        aes(x = treatment, y = pct_rotate,
                            fill = treatment, pattern = species)) +
  geom_bar_pattern(stat = "identity",
                   position = position_dodge(width = 0.8),
                   width = 0.7,
                   pattern_fill  = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black",
                   show.legend = TRUE) +
  # annotate("text", x = 2.5, y = 90,
  #          label = "NEED TO ADD",
  #          size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "", y = "Rotation Risk (%) in Year 7", title = "", pattern = "") +
  my_theme + theme(legend.position = "none")


# 5.6) Height 2024 by species
summary(silv_d_2024_ht_mod_final) 

# Ensure species is a factor with DF first (solid) then WRC (hatched)
silv_d_2024 <- silv_d_2024 |>
  mutate(species = factor(species, levels = c("DF", "WRC")))

p_ht_2024 <- ggplot(silv_d_2024,
                    aes(x = treatment, y = height_m,
                        fill = treatment, pattern = species)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       pattern_fill  = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black",
                       show.legend = TRUE) +
  annotate("text", x = 2.5, y = max(silv_d_2024$height_m, na.rm = TRUE) * 1.08,
           label = "Species, F/NW × WRC, F/W × WRC: p < 0.001",
           size = 4, hjust = 0.5) +
  annotate("text", x = 2.5, y = max(silv_d_2024$height_m, na.rm = TRUE) * 1.02,
           label = "All other effects: ns",
           size = 4, hjust = 0.5) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  labs(x = "", y = "Height (m) in Year 7", title = "", pattern = "") +
  my_theme + theme(legend.position = "none")


# 5.7) Diameter 2024 by species
summary(silv_d_2024_diam_mod_final)
p_diam_2024 <- ggplot(silv_d_2024diam_df,
                      aes(x = treatment, y = diameter_mm,
                          fill = treatment, pattern = species)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       pattern_fill  = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black",
                       show.legend = TRUE) +
  annotate("text", x = 2.5, y = max(silv_d_2024diam_df$diameter_mm, na.rm = TRUE) * 1.12,
           label = "Species, F/NW × WRC, F/W × WRC: p < 0.001",
           size = 4, hjust = 0.5) +
  annotate("text", x = 2.5, y = max(silv_d_2024diam_df$diameter_mm, na.rm = TRUE) * 1.05,
           label = "F/NW treatment: p < 0.05 | All other effects: ns",
           size = 4, hjust = 0.5) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) + #add space above text
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  labs(x = "", y = "Diameter (mm) in Year 7", title = "", pattern = "") +
  my_theme + theme(legend.position = "none")

# 5.8) Number of leaders 2024
p_leaders_2024 <- ggplot(silv_d_2024,
                         aes(x = treatment, y = number_of_leaders,
                             fill = treatment, pattern = species)) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8),
                       pattern_fill  = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black",
                       show.legend = TRUE) +
  scale_fill_manual(values = treatment_colors, labels = treatment_labels, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels) +
  scale_x_discrete(labels = treatment_labels) +
  labs(x = "", y = "Number of Leaders in Year 7", title = "", pattern = "") +
  my_theme + theme(legend.position = "none")

# ─────────────────────────────────────────────
# Combined figure
# Row 1: Browse | Height | Diameter        (2019 — 3 panels)
# Row 2: Deformity | Rot.Risk | Ht | Diam  (2024 — 4 panels)
# Use plot_spacer() to centre the 2019 row
# ─────────────────────────────────────────────
library(patchwork)

fig_combined <-
  (p_browse_2019 | p_ht_2019 | p_diam_2019 | plot_spacer()) /
  (p_deform_2024 | p_rotate_2024 | p_ht_2024 | p_diam_2024) +
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(legend.position = "none"))

fig_combined

fig_combined_save <- patchwork::wrap_elements(fig_combined)


ggsave("figures/silv_demo_combined.png",
       plot = fig_combined_save, dpi = 800, width = 20, height = 12, units = "in")


##################
#tables:
library(broom.mixed)

silv_model_table <- bind_rows(
  tidy(silv_d_2019_mod_final)       |> mutate(response = "Survival 2019"),
  tidy(silv_d_2019_ht_mod_final)    |> mutate(response = "Height 2019"),
  tidy(silv_d_2019_diam_mod_final)  |> mutate(response = "Diameter 2019"),
  tidy(silv_d_2024_deform_mod_final)|> mutate(response = "Deformity 2024"),
  tidy(silv_d_2024_rotate_mod_final)|> mutate(response = "Rotation Risk 2024"),
  tidy(silv_d_2024_ht_mod_final)    |> mutate(response = "Height 2024"),
  tidy(silv_d_2024_diam_mod_final)  |> mutate(response = "Diameter 2024")
) |>
  mutate(term = recode(term,
                       "(Intercept)"                = "Intercept",
                       "treatmentF/NW"              = "Treatment F/NW",
                       "treatmentF/W"               = "Treatment F/W",
                       "treatmentW"                 = "Treatment W",
                       "speciesWRC"                 = "Species Effect",
                       "treatmentF/NW:speciesWRC"   = "F/NW x Species",
                       "treatmentF/W:speciesWRC"    = "F/W x Species",
                       "treatmentW:speciesWRC"      = "W x Species"
  )) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  select(response, everything())

#View(silv_model_table)

write.csv(silv_model_table, "tables/model_results_silv.csv", row.names = FALSE)

