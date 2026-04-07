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

############
#1. Cow 2024
############

cow_2024 <- read_excel("./analysis_ready_data/cow_plots/Cow Data 2024.xlsx",
                     sheet = "Data")
colnames(cow_2024)
cow_2024 <- clean_names(cow_2024)
cow_2024 <- cow_2024 |>
  rename(east_west = e_w)
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
cont_deform_2024 <- table(cow_2024$in_out, cow_2024$deformity_y_n)
mosaicplot(cont_deform_2024)

chi_deform_2024 <- chisq.test(cont_deform_2024) #very sig
p_deform_2024 <- chi_deform_2024$p.value
fisher.test(cont_deform_2024) #very sig

#with species effect
cow_2024_deform_mod_final <- glm(deformity_y_n ~ in_out*species, cow_2024, family = "binomial")
summary(cow_2024_deform_mod_final) #DF is control 
p_deform_2024 <- summary(cow_2024_deform_mod_final)$coefficients[, "Pr(>|z|)"]

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

#doing t test on in versus out -- means are normal even if the data are not, when you have a sufficient sample size
# t_result_d2024<- t.test(diameter_mm ~ in_out, data = cow_24diam_df)
# p_diam_mod_2024 <- t_result_d2024$p.value

#adding species effect
cow_diam_2024 <- lm(diameter_mm ~ in_out*species, cow_24diam_df)
summary(cow_diam_2024)
p_diam_mod_2024 <- summary(cow_diam_2024)$coefficients[, "Pr(>|t|)"]


##################
# 1.3 Ht 2024 data
#################
#for continous ones, if you ahve large enough sample, t test 

hist(log(cow_2024$height_m))
hist(cow_2024$height_m) #looks pretty normal -- maybe just reg lm here

# t_result_h2024 <- t.test(height_m ~ in_out, data = cow_2024) #not sig
# p_ht_mod_2024 <- t_result_h2024$p.value

#adding species effect
cow_height_2024 <- lm(height_m ~ in_out*species, cow_2024)
summary(cow_height_2024)
p_ht_mod_2024 <- summary(cow_height_2024)$coefficients[, "Pr(>|t|)"]

qqnorm(residuals(cow_height_2024), main = "QQ plot (residuals)", pch = 16)
qqline(residuals(cow_height_2024))

##################
# 1.4 Rotation Risk 2024 data
#################
#there was one little n, fixing that here 
cow_2024 <- cow_2024 %>%
  mutate(rotation_risk_y_n = toupper(rotation_risk_y_n))

# cont_rotation_2024 <- table(cow_2024$in_out, cow_2024$rotation_risk_y_n)
# mosaicplot(cont_rotation_2024)
# 
# chi_rotate_2024 <- chisq.test(cont_rotation_2024) #very sig
# p_rotate_2024 <- chi_rotate_2024$p.value

#switching how rotation risk is coded
cow_2024$rotation_risk_y_n <- ifelse(cow_2024$rotation_risk_y_n == "Y", 1, 0)

#adding species effect
rotate_2024 <- glm(rotation_risk_y_n ~ in_out*species, cow_2024, family = "binomial")
summary(rotate_2024)
p_rotate_2024 <- summary(rotate_2024)$coefficients[, "Pr(>|z|)"]

##################
# 1.5 Number of leaders: 2024 data
#################
hist(cow_2024$number_of_leaders)

n_leaders_mod_2024 <- wilcox.test(number_of_leaders ~ in_out, data = cow_2024)

p_n_leaders_2024 <- n_leaders_mod_2024$p.value

#poisson glm for count data -- USE THIS
n_leaders_mod_2024 <- glm(number_of_leaders ~ in_out, cow_2024, family = Poisson) #log transformed is sig!

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

chisq.test(cont_browse_2016) #very sig

#doing fisher test because of low numbers
chi_browse_2016 <- fisher.test(cont_browse_2016) #very sig

p_browse_2016 <- chi_browse_2016$p.value

################################
#####Bringing in 2018 Data#######
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
  filter(!is.na(browse_y_n))

#removing empty strings (making them NA's first)
cow_2018 <- cow_2018 %>%
  mutate(browse_y_n = na_if(browse_y_n, ""),
         browse_y_n = as.character(browse_y_n))

cont_browse_2018 <- table(cow_2018$in_out, cow_2018$browse_y_n)
print(cow_2018$browse_y_n)
unique(cow_2018$browse_y_n)


mosaicplot(cont_browse_2018)
chisq.test(cont_browse_2018)
#doing fisher test because of low numbers
chi_browse_2018 <- fisher.test(cont_browse_2018)
p_browse_2018 <- chi_browse_2018$p.value

################

#1) Plotting ht, diameter, browse and deformity in multi panel plot
format_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 3)))
}
# 
# # 1.1 Plotting deformity risk
# deform_2024 <- cow_2024 |>
#   group_by(in_out) |>
#   summarise(pct_deformity = mean(deformity_y_n == "Y") * 100) |> #getting deformity as percentage
#   ggplot(aes(x = in_out, y = pct_deformity, fill = in_out)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   scale_y_continuous(limits = c(0, 100)) +
#   #p value
#   annotate("text", x = 1.15, y = 97,
#            label = format_p(p_deform_2024),
#            color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
#   labs(x = "Fence",
#        y = "Deformity (%) in Year 8") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12)
#   )
# 
# #1.2 
# browse_2016 <- cow_2016 |> #NOTE THIS IS 2016 DATA
#   group_by(in_out) |>
#   summarise(pct_deformity = mean(browse_y_n == "Y") * 100) |> #getting deformity as percentage
#   ggplot(aes(x = in_out, y = pct_deformity, fill = in_out)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   labs(x = "Fence",
#        y = "Browse (%) in Year 1") +
#   scale_y_continuous(limits = c(0, 100)) +
#   annotate("text", x = 1.15, y = 97,
#            label = format_p(p_browse_2016),
#            color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12),
#   ) +
#   annotate("segment", x = 0.5, xend = 1.5, y = 0, yend = 0,
#            color = "#009E73", linewidth = 1)
# browse_2016
# 
# #1.3 
# browse_2018 <- cow_2018 |> #NOTE THIS IS 2018 DATA
#   group_by(in_out) |>
#   summarise(pct_deformity = mean(browse_y_n == "Y") * 100) |> #getting deformity as percentage
#   ggplot(aes(x = in_out, y = pct_deformity, fill = in_out)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   labs(x = "Fence",
#        y = "Browse (%) in Year 3") +
#   scale_y_continuous(limits = c(0, 100)) +
#   annotate("text", x = 1.15, y = 97,
#            label = format_p(p_browse_2018),
#            color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12),
#   ) +
#   annotate("segment", x = 0.5, xend = 1.5, y = 0, yend = 0,
#            color = "#009E73", linewidth = 1)
# browse_2018
# 
# # 1.4 Plotting diameter 
# 
# diam_2024 <- ggplot(cow_2024, aes(x = in_out, y = diameter_mm, fill = in_out)) +
#   geom_boxplot(show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   labs(x = "Fence",
#        y = "Diameter (mm) in Year 8") +
#   annotate("text", x = 1.175, y = max(cow_2024$diameter_mm, na.rm = TRUE)-9,
#            label = format_p(p_diam_mod_2024),
#            color = "black", size = 5, hjust = 0) +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12)
#   )
# 
# # 1.5 plotting height
# ht_2024 <- ggplot(cow_2024, aes(x = in_out, y = height_m, fill = in_out)) +
#   geom_boxplot(show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   labs(x = "Fence",
#        y = "Height (m) in Year 8") +
#   annotate("text", x = 1.175, y = max(cow_2024$height_m, na.rm = TRUE)-.47,
#            label = format_p(p_ht_mod_2024),
#            color = "black", size = 5, hjust = 0) +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12)
#   )
# 
# # 1.6 plotting rotation risk 
# rotate_plot_2024 <- cow_2024 |>
#   group_by(in_out) |>
#   summarise(pct_rotate = mean(rotation_risk_y_n == "Y") * 100) |> #getting rotation risk as percentage
#   ggplot(aes(x = in_out, y = pct_rotate, fill = in_out)) +
#   geom_bar(stat = "identity", show.legend = FALSE) +
#   scale_fill_manual(values = c("In" = "#009E73", "Out" = "#0072B2"),
#                     name = "Fence") +
#   scale_y_continuous(limits = c(0, 100)) +
#   #p value
#   annotate("text", x = 1.15, y = 97,
#            label = format_p(p_rotate_2024),
#            color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
#   labs(x = "Fence",
#        y = "Rotation Risk (%) in Year 8") +
#   theme_bw() +
#   theme(
#     axis.title.x = element_text(size = 14),
#     axis.title.y = element_text(size = 14),
#     axis.text = element_text(size = 12)
#   )
# 
# 
# 
# 
# library(patchwork)
# 
# four_vertical <- (browse_2016 + browse_2018 + deform_2024) /
#   (rotate_plot_2024 + diam_2024 + ht_2024 )
# 
# # Display
# four_vertical
# 
# # Save (same output path you used earlier, adjust name as desired)
# ggsave(
#   filename = "figures/cow_all.png",
#   plot = four_vertical, dpi = 800, width = 10, height = 7, units = "in"
# )



################################
#version 2
#################################
library(ggpattern)

species_pattern <- c("DF" = "none", "WRC" = "stripe")
species_labels  <- c("DF" = "Douglas-fir", "WRC" = "Western Red Cedar")

cow_colors <- c("In" = "#009E73", "Out" = "#0072B2")

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
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1.15, y = 97, label = format_p(p_deform_2024),
           color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "Fence", y = "Deformity (%) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
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
  annotate("text", x = 1.15, y = 97, label = format_p(p_browse_2016),
           color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "Fence", y = "Browse (%) in Year 1") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "bottom") +  # this one carries the legend
  guides(pattern = guide_legend(override.aes = list(fill = "white")))

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
  annotate("text", x = 1.15, y = 97, label = format_p(p_browse_2018),
           color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "Fence", y = "Browse (%) in Year 3") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none")

# 1.4 Diameter

diam_2024 <- ggplot(cow_24diam_df |> mutate(species = factor(species, levels = c("DF", "WRC"))),
                    aes(x = in_out, y = diameter_mm, fill = in_out, pattern = species,
                        group = interaction(in_out, species))) +
  geom_boxplot_pattern(position = position_dodge(width = 0.8), color = "black",
                       pattern_fill = "black", pattern_angle = 45,
                       pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  annotate("text", x = 1.175, y = max(cow_24diam_df$diameter_mm, na.rm = TRUE) - 9,
           label = format_p(p_diam_mod_2024), color = "black", size = 5, hjust = 0) +
  labs(x = "Fence", y = "Diameter (mm) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
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
  annotate("text", x = 1.175, y = max(cow_2024$height_m, na.rm = TRUE) - 0.47,
           label = format_p(p_ht_mod_2024), color = "black", size = 5, hjust = 0) +
  labs(x = "Fence", y = "Height (m) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none")

# 1.6 Rotation Risk
rotate_plot_2024 <- cow_2024 |>
  mutate(species = factor(species, levels = c("DF", "WRC"))) |>
  group_by(in_out, species) |>
  summarise(pct_rotate = mean(rotation_risk_y_n == "Y") * 100, .groups = "drop") |>
  ggplot(aes(x = in_out, y = pct_rotate, fill = in_out, pattern = species)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(width = 0.8),
                   width = 0.7, color = "black",
                   pattern_fill = "black", pattern_angle = 45,
                   pattern_density = 0.15, pattern_spacing = 0.03) +
  scale_fill_manual(values = cow_colors, guide = "none") +
  scale_pattern_manual(values = species_pattern, labels = species_labels, name = "Species") +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1.15, y = 97, label = format_p(p_rotate_2024),
           color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "Fence", y = "Rotation Risk (%) in Year 8") +
  theme_bw() +
  theme(axis.title = element_text(size = 14), axis.text = element_text(size = 12),
        legend.position = "none")

# Combined
four_vertical <- (browse_2016 + browse_2018 + deform_2024) /
  (rotate_plot_2024 + diam_2024 + ht_2024)

png("figures/cow_all.png", width = 12, height = 8, units = "in", res = 800)
print(four_vertical)
dev.off()

