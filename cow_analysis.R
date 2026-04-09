#10/1/2024
#Example analysis of cow pre generals
# install.packages("faraway")
# install.packages("arm")
# install.packages("generalhoslem")

library(faraway)
library(dplyr)
library(arm)
library(generalhoslem)
library(tidyverse)
library(readxl)
#install.packages("janitor")
library(janitor)
library(ggplot2)
library(ggpattern)
source("./functions/check_resids_func.R")

############
#1. Cow 2024
############

cow_2024 <- read_excel("./analysis_ready_data/cow_plots/Cow Data 2024.xlsx",
                     sheet = "Data")
colnames(cow_2024)
cow_2024 <- clean_names(cow_2024)
cow_2024 <- cow_2024 |>
  dplyr::rename(east_west = e_w)

#Trim any potential leading/trailing spaces in the deformity_y_n column
cow_2024$deformity_y_n <- trimws(cow_2024$deformity_y_n)
cow_2024$deformity_y_n <- as.factor(cow_2024$deformity_y_n)

unique(cow_2024$deformity_y_n)

#making all empty cells NA
cow_2024[cow_2024 == ""] <- NA

#there are multiple rows for one tree, if the tree had multiple deformities or multiple forks, 
#for now, going to drop replicates but important to remember that there is more data 
#available on deformities 
cow_2024 <- cow_2024 %>%
  filter(!is.na(species))

#############
#1.1 DEFORMITY
#############
#making out DF the reference
cow_2024 <- cow_2024 |>
  mutate(in_out = relevel(factor(in_out), ref = "Out"),
         species = relevel(factor(species), ref = "DF"))

#with species effect
cow_2024_deform_mod_final <- glm(deformity_y_n ~ in_out*species, cow_2024, family = "binomial")
summary(cow_2024_deform_mod_final) #DF is control 
p_deform_2024 <- summary(cow_2024_deform_mod_final)$coefficients[, "Pr(>|z|)"]

#dharma check resids
simulationOutput <- simulateResiduals(cow_2024_deform_mod_final)
plot(simulationOutput) #looks good 
########
#1.2 Diameter 
##########
#filter so that we don't have any without diameter values
# cow_2024 <- cow_2024 %>%
#   filter(!is.na(diameter_mm))

#need to drop weird diameter measurements here
cow_24diam_df <- cow_2024 %>%
  filter(!grepl("Diameter|measured at|diam @ |m @ 0\\.5|m @ 0\\.25|m @ 0\\.9|measurements at 0\\.5|measurements at 0\\.25|measurements at 0\\.9", 
                comments, ignore.case = TRUE))

print(cow_24diam_df$comments)
hist(log(cow_24diam_df$diameter_mm))
hist(cow_24diam_df$diameter_mm)

#adding species effect
cow_diam_2024 <- lm(diameter_mm ~ in_out*species, cow_24diam_df)
summary(cow_diam_2024)
p_diam_mod_2024 <- summary(cow_diam_2024)$coefficients[, "Pr(>|t|)"]

check_resids(cow_diam_2024)
#Species effect (speciesWRC, p < 0.001) — WRC is significantly smaller in diameter than DF (-21.6mm), regardless of fence

##################
# 1.3 Ht 2024 data
#################
#for continuous ones, if you have large enough sample, t test 

hist(log(cow_2024$height_m))
hist(cow_2024$height_m) #looks pretty normal -- maybe just reg lm here

#adding species effect
cow_height_2024 <- lm(height_m ~ in_out*species, cow_2024)
summary(cow_height_2024)
p_ht_mod_2024 <- summary(cow_height_2024)$coefficients[, "Pr(>|t|)"]

qqnorm(residuals(cow_height_2024), main = "QQ plot (residuals)", pch = 16)
qqline(residuals(cow_height_2024))

check_resids(cow_height_2024)

tapply(cow_2024$height_m, list(cow_2024$species, cow_2024$in_out), FUN = mean, na.rm = TRUE)

##################
# 1.4 Rotation Risk 2024 data
#################
#there was one little n, fixing that here 
cow_2024 <- cow_2024 %>%
  mutate(rotation_risk_y_n = toupper(rotation_risk_y_n))

#switching how rotation risk is coded
cow_2024$rotation_risk_y_n <- ifelse(cow_2024$rotation_risk_y_n == "Y", 1, 0)

#adding species effect
rotate_2024 <- glm(rotation_risk_y_n ~ in_out*species, cow_2024, family = "binomial")
summary(rotate_2024)
p_rotate_2024 <- summary(rotate_2024)$coefficients[, "Pr(>|z|)"]

check_resids(rotate_2024)
simulationOutput <- simulateResiduals(rotate_2024)
plot(simulationOutput) #looks good 

##################
# 1.5 Number of leaders: 2024 data
#################
hist(cow_2024$number_of_leaders)

#poisson glm for count data -- USE THIS
n_leaders_mod_2024 <- glm(number_of_leaders ~ in_out*species, cow_2024, family = poisson) #log transformed is sig!
summary(n_leaders_mod_2024)
p_leaders_2024 <- summary(n_leaders_mod_2024)$coefficients[, "Pr(>|z|)"]

simulationOutput <- simulateResiduals(n_leaders_mod_2024)
plot(simulationOutput)
testOutliers(simulationOutput, type = "bootstrap") #but this looks okay

####################
################################
##### 2. Bringing in 2016 Data #######
################################
####################

cow_2016 <- read.csv("./analysis_ready_data/cow_plots/cow_data_4_7_2016.csv")
#Trim any potential leading/trailing spaces in the deformity_y_n column
cow_2016$browse_y_n <- trimws(cow_2016$browse_y_n)
cow_2016$browse_y_n <- as.factor(cow_2016$browse_y_n)
#fixing sp name
cow_2016 <- cow_2016 %>%
  mutate(species = ifelse(species == "RC", "WRC", species))

unique(cow_2016$browse_y_n)

#making all empty cells NA
cow_2016[cow_2016 == ""] <- NA

#fixing in and out so it matches 2024 dataset
cow_2016 <- cow_2016 |>
  mutate(in_out = case_when(
    tolower(trimws(in_out)) == "in"  ~ "In",
    tolower(trimws(in_out)) == "out" ~ "Out",
    TRUE ~ in_out
  ))

cont_browse_2016 <- table(cow_2016$in_out, cow_2016$browse_y_n)
mosaicplot(cont_browse_2016)

chi_browse_2016 <- chisq.test(cont_browse_2016) #very sig

#doing fisher test because of low numbers
#chi_browse_2016 <- fisher.test(cont_browse_2016) #very sig

p_browse_2016 <- chi_browse_2016$p.value

#### 2.2 Height for 2016 ####

cow_2016 <- cow_2016 |>
  mutate(in_out = relevel(factor(in_out), ref = "Out"),
         species = relevel(factor(species), ref = "DF"))

hist(log(cow_2016$height_cm))
hist(cow_2016$height_cm) 

#adding species effect
cow_height_2016 <- lm(height_cm ~ in_out*species, cow_2016)
summary(cow_height_2016)
#p_ht_mod_2024 <- summary(cow_height_2024)$coefficients[, "Pr(>|t|)"]

qqnorm(residuals(cow_height_2016), main = "QQ plot (residuals)", pch = 16)
qqline(residuals(cow_height_2016))

check_resids(cow_height_2016)

###### 2.3 Diameter for 2016 #####
hist(log(cow_2016$diameter_mm))
hist(cow_2016$diameter_mm)

#adding species effect
cow_diam_2016 <- lm(diameter_mm ~ in_out*species, cow_2016)
summary(cow_diam_2016)
#p_diam_mod_2024 <- summary(cow_diam_2024)$coefficients[, "Pr(>|t|)"]

check_resids(cow_diam_2016)


################################
#####3. Bringing in 2018 Data#######
################################

cow_2018 <- read.csv("analysis_ready_data/cow_plots/cow_data_8_2_2018.csv")
#fixing sp name
cow_2018 <- cow_2018 %>%
  mutate(species = ifelse(species == "RC", "WRC", species))

#Trim any potential leading/trailing spaces in the deformity_y_n column
cow_2018$browse_y_n <- trimws(cow_2018$browse_y_n)
cow_2018$browse_y_n <- as.factor(cow_2018$browse_y_n)

unique(cow_2018$browse_y_n)

#making all empty cells NA
cow_2018[cow_2018 == ""] <- NA

#fixing in and out so it matches 2024 dataset
cow_2018 <- cow_2018 |>
  mutate(in_out = case_when(
    tolower(trimws(in_out)) == "in"  ~ "In",
    tolower(trimws(in_out)) == "out" ~ "Out",
    TRUE ~ in_out
  ))

#dead/missing trees were left in the dataset. Drop those for now
cow_2018 <- cow_2018 %>%
  filter(!is.na(browse_y_n)) %>%
  filter(!is.na(species)) %>% #one na species
  filter(!tolower(Notes) %in% c("dead", "missing") | is.na(Notes))

#removing empty strings (making them NA's first)
cow_2018 <- cow_2018 %>%
  mutate(browse_y_n = na_if(browse_y_n, ""),
         browse_y_n = as.character(browse_y_n))

#### 3.1 Browse for 2018 ###
cont_browse_2018 <- table(cow_2018$in_out, cow_2018$browse_y_n)
print(cow_2018$browse_y_n)
unique(cow_2018$browse_y_n)


mosaicplot(cont_browse_2018)
chi_browse_2018 <- chisq.test(cont_browse_2018)
#doing fisher test because of low numbers
#chi_browse_2018 <- fisher.test(cont_browse_2018)
p_browse_2018 <- chi_browse_2018$p.value

#### 3.2 Height for 2018 ####

cow_2018 <- cow_2018 |>
  mutate(in_out = relevel(factor(in_out), ref = "Out"),
         species = relevel(factor(species), ref = "DF"))

hist(log(cow_2018$height_cm))
hist(cow_2018$height_cm) 

#adding species effect
cow_height_2018 <- lm(height_cm ~ in_out*species, cow_2018)
summary(cow_height_2018)
p_ht_mod_2018 <- summary(cow_height_2018)$coefficients[, "Pr(>|t|)"]

qqnorm(residuals(cow_height_2018), main = "QQ plot (residuals)", pch = 16)
qqline(residuals(cow_height_2018))

check_resids(cow_height_2018)

tapply(cow_2018$height_cm, list(cow_2018$species, cow_2018$in_out), FUN = mean, na.rm = TRUE)

###### 3.3 Diameter for 2018 #####
hist(log(cow_2018$diameter_mm))
hist(cow_2018$diameter_mm)

#adding species effect
cow_diam_2018 <- lm(diameter_mm ~ in_out*species, cow_2018)
summary(cow_diam_2018)
p_diam_mod_2018 <- summary(cow_diam_2018)$coefficients[, "Pr(>|t|)"]

check_resids(cow_diam_2018)

library(emmeans)

emm <- emmeans(cow_diam_2018, ~ in_out * species)
emm
pairs(emm, by = "species")
################

#1) Plotting ht, diameter, browse and deformity in multi panel plot
format_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 2)))
}

################################
#version 2
#################################
library(ggpattern)

species_pattern <- c("DF" = "none", "WRC" = "stripe")
species_labels  <- c("DF" = "Douglas-fir", "WRC" = "Western Red Cedar")

cow_colors <- c("In" = "#009E73", "Out" = "#0072B2")
library(ggsignif)

# 1.1 Deformity
deform_2024 <- cow_2024 |>
  mutate(species = factor(species, levels = c("DF", "WRC"))) |>
  group_by(in_out, species) |>
  summarise(pct_deformity = mean(deformity_y_n == "Y") * 100, .groups = "drop") |>
  ggplot(aes(x = in_out, y = pct_deformity, fill = in_out, pattern = species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.8),
                   width = 0.7, color = "black",
                   pattern_fill = "black", pattern_angle = 45,
                   pattern_density = 0.15, pattern_spacing = 0.03) +
  annotate("text", x = 1.5, y = 100,
           label = paste("Fence effect x WRC:", format_p(p_deform_2024["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = 95,
           label = paste("Fence effect x DF:", format_p(p_deform_2024["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = 90,
           label = paste("Species effect:", format_p(p_deform_2024["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  # annotate("text", x = 1.15, y = 97, label = format_p(p_deform_2024),
  #          color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "", y = "Deformity (%) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "center")  

# 1.2 Browse 2016
browse_2016 <- cow_2016 |>
  mutate(species = factor(species, levels = c("DF", "WRC"))) |>
  group_by(in_out, species) |>
  summarise(pct_browse = mean(browse_y_n == "Y") * 100, .groups = "drop") |>
  ggplot(aes(x = in_out, y = pct_browse, fill = in_out, pattern = species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.8),
                   width = 0.7, color = "black",
                   pattern_fill = "black", pattern_angle = 45,
                   pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1, y = 97, label = paste("Fence effect:", format_p(p_browse_2016)),
           color = "black", size = 6, hjust = 0) +
  labs(x = "", y = "Browse (%) in Year 1") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
      legend.position = "none")

# 1.3 Browse 2018
browse_2018 <- cow_2018 |>
  mutate(species = factor(species, levels = c("DF", "WRC"))) |>
  group_by(in_out, species) |>
  summarise(pct_browse = mean(browse_y_n == "Y") * 100, .groups = "drop") |>
  ggplot(aes(x = in_out, y = pct_browse, fill = in_out, pattern = species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.8),
                   width = 0.7, color = "black",
                   pattern_fill = "black", pattern_angle = 45,
                   pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1, y = 97, label = paste("Fence effect:", format_p(p_browse_2018)),
           color = "black", size = 6, hjust = 0) +
  labs(x = "", y = "Browse (%) in Year 3") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")


# 1.4 Diameter

diam_2024 <- ggplot(cow_24diam_df |> mutate(species = factor(species, levels = c("DF", "WRC"))),
                    aes(x = in_out, y = diameter_mm, fill = in_out, pattern = species,
                        group = interaction(in_out, species))) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8), color = "black",
                       pattern_fill = "black", pattern_angle = 45,
                       pattern_density = 0.15, pattern_spacing = 0.03) +
  annotate("text", x = 1.5, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) * 1.17,
           label = paste("Fence effect x WRC:", format_p(p_diam_mod_2024["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) * 1.11,
           label = paste("Fence effect x DF:", format_p(p_diam_mod_2024["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) * 1.05,
           label = paste("Species effect:", format_p(p_diam_mod_2024["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.13))) + #getting more space for sig
  # annotate("text", x = 1.175, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) - 9,
  #          label = format_p(p_diam_mod_2024), color = "black", size = 5, hjust = 0) +
  labs(x = "", y = "Diameter (mm) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")

# 1.5 Height
ht_2024 <- ggplot(cow_2024 |> mutate(species = factor(species, levels = c("DF", "WRC"))),
                  aes(x = in_out, y = height_m, fill = in_out, pattern = species,
                      group = interaction(in_out, species))) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8), color = "black",
                       pattern_fill = "black", pattern_angle = 45,
                       pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.13))) + #getting more space for sig
  annotate("text", x = 1.5, y = max(cow_2024$height_m, na.rm = TRUE) * 1.17,
           label = paste("Fence effect x WRC:", format_p(p_ht_mod_2024["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2024$height_m, na.rm = TRUE) * 1.11,
           label = paste("Fence effect x DF:", format_p(p_ht_mod_2024["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2024$height_m, na.rm = TRUE) * 1.05,
           label = paste("Species effect:", format_p(p_ht_mod_2024["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  labs(x = "", y = "Height (m) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")

# 1.6 Rotation Risk
rotate_plot_2024 <- cow_2024 |>
  mutate(species = factor(species, levels = c("DF", "WRC"))) |>
  group_by(in_out, species) |>
  summarise(pct_rotate = mean(rotation_risk_y_n == "1") * 100, .groups = "drop") |>
  ggplot(aes(x = in_out, y = pct_rotate, fill = in_out, pattern = species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.8),
                   width = 0.7, color = "black",
                   pattern_fill = "black", pattern_angle = 45,
                   pattern_density = 0.15, pattern_spacing = 0.03) +
  annotate("text", x = 1.5, y = 100,
           label = paste("Fence effect x WRC:", format_p(p_rotate_2024["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = 95,
           label = paste("Fence effect x DF:", format_p(p_rotate_2024["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = 90,
           label = paste("Species effect:", format_p(p_rotate_2024["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  # annotate("text", x = 1.15, y = 97, label = format_p(p_rotate_2024),
  #          color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "", y = "Rotation Risk (%) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")

diam_2018 <- ggplot(cow_2018 |> mutate(species = factor(species, levels = c("DF", "WRC"))),
                    aes(x = in_out, y = diameter_mm, fill = in_out, pattern = species,
                        group = interaction(in_out, species))) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8), color = "black",
                       pattern_fill = "black", pattern_angle = 45,
                       pattern_density = 0.15, pattern_spacing = 0.03) +
  annotate("text", x = 1.5, y = max(cow_2018$diameter_mm, na.rm = TRUE) * 1.17,
           label = paste("Fence effect x WRC:", format_p(p_diam_mod_2018["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2018$diameter_mm, na.rm = TRUE) * 1.11,
           label = paste("Fence effect x DF:", format_p(p_diam_mod_2018["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2018$diameter_mm, na.rm = TRUE) * 1.05,
           label = paste("Species effect:", format_p(p_diam_mod_2018["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.13))) + #getting more space for sig
  # annotate("text", x = 1.175, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) - 9,
  #          label = format_p(p_diam_mod_2024), color = "black", size = 5, hjust = 0) +
  labs(x = "", y = "Diameter (mm) in Year 3") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")

ht_2018 <- ggplot(cow_2018 |> mutate(species = factor(species, levels = c("DF", "WRC"))),
                  aes(x = in_out, y = height_cm, fill = in_out, pattern = species,
                      group = interaction(in_out, species))) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8), color = "black",
                       pattern_fill = "black", pattern_angle = 45,
                       pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.13))) + #getting more space for sig
  annotate("text", x = 1.5, y = max(cow_2018$height_cm, na.rm = TRUE) * 1.17,
           label = paste("Fence effect x WRC:", format_p(p_ht_mod_2018["in_outIn:speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2018$height_cm, na.rm = TRUE) * 1.11,
           label = paste("Fence effect x DF:", format_p(p_ht_mod_2018["in_outIn"])),
           color = "black", size = 6, hjust = 0.5) +
  annotate("text", x = 1.5, y = max(cow_2018$height_cm, na.rm = TRUE) * 1.05,
           label = paste("Species effect:", format_p(p_ht_mod_2018["speciesWRC"])),
           color = "black", size = 6, hjust = 0.5) +
  labs(x = "", y = "Height (cm) in Year 3") +
  theme_bw() +
  theme(axis.title = element_text(size = 8 * .pt), axis.text = element_text(size = 8 * .pt),
        legend.position = "none")

library(patchwork)

eight_plots <- wrap_plots(browse_2016, browse_2018, ht_2018,diam_2018, 
           deform_2024, rotate_plot_2024, ht_2024, diam_2024,
           nrow = 2, ncol = 4)

ggsave("figures/cow_all.png",
       plot = eight_plots, dpi = 800, width = 24, height = 12, units = "in")
####################
library(broom)

# qqline(residuals(cow_height_2018))
# 
# check_resids(cow_height_2018)
# 
# ###### 3.3 Diameter for 2018 #####
# hist(log(cow_2018$diameter_mm))
# hist(cow_2018$diameter_mm)
# 
# #adding species effect
# cow_diam_2018 <- lm(diameter_mm ~ in_out*species, cow_2018)
# summary(cow_diam_2018)
# #p_diam_mod_2024 <- summary(cow_diam_2024)$coefficients[, "Pr(>|t|)"]
# 
# check_resids(cow_diam_2018)

model_table <- bind_rows(
  tidy(cow_height_2016)           |> mutate(response = "Height, Year 1"),
  tidy(chi_browse_2016)           |> mutate(response = "Browse, Year 1"), #move this one later
  tidy(cow_diam_2016)             |> mutate(response = "Diameter, Year 1"),
  tidy(chi_browse_2018)           |> mutate(response = "Browse, Year 3"),
  tidy(cow_height_2018)           |> mutate(response = "Height, Year 3"),
  tidy(cow_diam_2018)             |> mutate(response = "Diameter, Year 3"),
  tidy(cow_2024_deform_mod_final) |> mutate(response = "Deformity, Year 8"),
  tidy(n_leaders_mod_2024)        |> mutate(response = "Number of Leaders, Year 8"),
  tidy(rotate_2024)               |> mutate(response = "Rotation Risk, Year 8"),
  tidy(cow_height_2024)           |> mutate(response = "Height, Year 8"),
  tidy(cow_diam_2024)             |> mutate(response = "Diameter, Year 8"),

) |> #change term names
  mutate(term = recode(term,
                       "in_outIn"            = "Fence effect x DF",
                       "speciesWRC"          = "Species Effect",
                       "in_outIn:speciesWRC" = "Fence effect x WRC",
                       "(Intercept)"         = "Intercept"
  )) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  dplyr::select(response, everything())

#View(model_table)
#model_table$term

write.csv(model_table, "../Browse_analysis/tables/model_results_cow.csv", row.names = FALSE)

#old
# "in_outIn"            = "Fence Effect",
# "speciesWRC"          = "Species Effect",
# "in_outIn:speciesWRC" = "Fence x Species",
# "(Intercept)"         = "Intercept"

