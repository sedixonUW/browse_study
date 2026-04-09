#Coburg Tree Farm

library(faraway)
library(dplyr)
library(arm)
library(generalhoslem)
library(tidyverse)
library(readxl)
#install.packages("janitor")
library(janitor)
library(ggplot2)
source("./functions/check_resids_func.R")

############
#Violet helped lead 2024 data collection and she told me to combine both of these sheets;
#one has more columns than the other but otherwise, good to use both

#all measured trees are Douglas-fir

pair_2024_1 <- read_excel("./analysis_ready_data/Coburg_DF/Coburg DF Browse Study Data 2024.xlsx",
                              sheet = "Pair - Useable")

pair_2024_2 <- read_excel("./analysis_ready_data/Coburg_DF/Coburg DF Browse Study Data 2024.xlsx",
                        sheet = "CB VH Full Pair Data")
#combines them but keeps extra columns
pair_2024 <- bind_rows(pair_2024_1, pair_2024_2)

#########
vex_2024_1 <- read_excel("./analysis_ready_data/Coburg_DF/Coburg DF Browse Study Data 2024.xlsx",
                       sheet = "Vexar - Useable")
vex_2024_2 <- read_excel("./analysis_ready_data/Coburg_DF/Coburg DF Browse Study Data 2024.xlsx",
                       sheet = "CB VH Full Vexar Data")
vex_2024 <- bind_rows(vex_2024_1, vex_2024_2)

colnames(vex_2024)
colnames(pair_2024)

coburg_2024 <- bind_rows(pair_2024, vex_2024)

# verify that things look okay
colnames(coburg_2024)
nrow(coburg_2024)

coburg_2024 <- clean_names(coburg_2024)


#dropping extra row for deformity measurements for now 
#WILL WANT TO CHANGE THIS IF LOOKING CLOSELY AT DEFORM PATTERNS
coburg_2024 <- coburg_2024 %>%
 filter(!is.na(number_of_leaders)) #if this is NA, then it's just a second measurment

#drop measurements that were for blue tubes, not vexar
coburg_2024 <- coburg_2024 |>
  filter(!grepl("blue tube", comments, ignore.case = TRUE))

#############################
#1.1 Deformity
################################
#dropping where deform is NA
coburg_2024_deform <- coburg_2024 %>%
  filter(!is.na(deformity_y_n)) %>% #if this is NA, then it's just a second measurment
  filter(!deformity_y_n == "SW") #odd SW entry here

unique(coburg_2024_deform$deformity_y_n)
cont_deform_2024 <- table(coburg_2024_deform$treatment, coburg_2024_deform$deformity_y_n)
mosaicplot(cont_deform_2024)

chi_deform_2024 <- chisq.test(cont_deform_2024) #very sig
p_deform_2024 <- chi_deform_2024$p.value
#fisher.test(cont_deform_2024) #very sig

#significant negative treatment effect of vexar on deformity

#############################
#1.2 Rotation Risk
################################

cont_rotation_2024 <- table(coburg_2024$treatment, coburg_2024$rotation_risk_y_n)
mosaicplot(cont_rotation_2024)

chi_rotate_2024 <- chisq.test(cont_rotation_2024) #not sig
p_rotate_2024 <- chi_rotate_2024$p.value

fisher.test(cont_rotation_2024) # NOT sig
#rotation risk is not significant
#take away: deformity is a bigger problem for vexar but not
#significant enough to cause large issues with final growth form

##################
#1.3 Height 2024 Coburg
####################
hist(coburg_2024$height_m)

#coburg_2024_ht_mod <- lmerTest::lmer(height_m ~ treatment + (1|plot_number), coburg_2024)
coburg_2024_ht_mod <- lm(height_m ~ treatment, coburg_2024)
#AIC(coburg_2024_ht_mod)
#AIC(coburg_2024_ht_mod_2) #no difference
coburg_2024_ht_mod_final <- coburg_2024_ht_mod

summary(coburg_2024_ht_mod_final) #vexar had shorter height

check_resids(coburg_2024_ht_mod_final)
#looks good
p_coburg_2024_ht <- summary(coburg_2024_ht_mod_final)$coefficients[, "Pr(>|t|)"]


##################
#1.4 Diameter 2024 Coburg
####################
hist(coburg_2024$diameter_mm)

coburg_2024_diam_df <- coburg_2024 %>%
  filter(!grepl("D & DfV taken at 0.5m|Diameter", comments))

#coburg_2024_diam_mod <- lmerTest::lmer(diameter ~ treatment + (1|plot), coburg_2024)
coburg_2024_diam_mod <- lm(diameter_mm ~ treatment, coburg_2024_diam_df)
AIC(coburg_2024_diam_mod)
#AIC(coburg_2024_diam_mod_2)

coburg_2024_diam_mod_final <- coburg_2024_diam_mod
summary(coburg_2024_diam_mod_final) #sig neg with vexar (paired planting outperformed on ht and diameter)

check_resids(coburg_2024_diam_mod_final)

p_coburg_2024_diam <- summary(coburg_2024_diam_mod_final)$coefficients[, "Pr(>|t|)"]

#looks good

##################
#1.5 number of leaders 2024 Coburg
####################

hist(coburg_2024$number_of_leaders)

cont_leaders_2024 <- table(coburg_2024$treatment, coburg_2024$number_of_leaders)
mosaicplot(cont_leaders_2024)

n_leaders_mod_2024_neg_binom <- glm.nb(number_of_leaders ~ treatment, data = coburg_2024) 
n_leaders_mod_2024_poisson <- glm((number_of_leaders) ~ treatment, data = coburg_2024, family = poisson) 

AIC(n_leaders_mod_2024_neg_binom)
AIC(n_leaders_mod_2024_poisson) #better AIC

summary(n_leaders_mod_2024_poisson)
summary(n_leaders_mod_2024_neg_binom)
#outcome does not change

p_n_leaders_2024 <- summary(n_leaders_mod_2024_poisson)$coefficients[, "Pr(>|z|)"]

#neg binomial check
check_resids(n_leaders_mod_2024)

simulationOutput <- simulateResiduals(n_leaders_mod_2024_neg_binom)
plot(simulationOutput) #this looks bad
testOutliers(simulationOutput, type = "bootstrap") #but this looks okay

#poisson
simulationOutput <- simulateResiduals(n_leaders_mod_2024_poisson)
plot(simulationOutput) #this looks bad
testOutliers(simulationOutput, type = "bootstrap") #but this looks okay


#going with poisson

coburg_2024 |>
  group_by(treatment) |>
  summarise(median = median(number_of_leaders, na.rm = TRUE),
            n = n())
#pair plant has more leaders!

########################################################################
#2.1 Plotting
########################################################################


library(ggplot2)
library(patchwork)

format_p <- function(p) {
  ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 2)))
}


my_theme <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 18),
    axis.text    = element_text(size = 16),
    axis.text.x  = element_text(angle = 30, hjust = 1),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text  = element_text(size = 11),
    legend.key    = element_rect(fill = "white", color = NA),
    aspect.ratio = 1.2,
    plot.margin = margin(2, 2, 2, 2, "pt")
    
  )

coburg_2024 <- coburg_2024 |>
  mutate(treatment = recode(treatment, "Pair Plant" = "SS Co Planting"))

coburg_2024_diam_df <- coburg_2024_diam_df |>
  mutate(treatment = recode(treatment, "Pair Plant" = "SS Co Planting"))

coburg_2024_deform <- coburg_2024_deform |>
  mutate(treatment = recode(treatment, "Pair Plant" = "SS Co Planting"))

# Colors for Pair Plant and Vexar
coburg_colors <- c("SS Co Planting" = "#E69F00", "Vexar" = "#56B4E9")

# 1.1 Deformity
deform_coburg <- coburg_2024_deform |>
  group_by(treatment) |>
  summarise(pct_deformity = mean(deformity_y_n == "Y", na.rm = TRUE) * 100) |>
  ggplot(aes(x = treatment, y = pct_deformity, fill = treatment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = coburg_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1.5, y = 98,
           label = paste0("Treatment effect: ", format_p(p_deform_2024)),
           size = 4, hjust = 0.5) +
  labs(x = "", y = "Deformity (%) in Year 5") +
  theme_bw() +
  my_theme

# 1.2 Rotation Risk
rotate_coburg <- coburg_2024 |>
  group_by(treatment) |>
  summarise(pct_rotate = mean(rotation_risk_y_n == "Y", na.rm = TRUE) * 100) |>
  ggplot(aes(x = treatment, y = pct_rotate, fill = treatment)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = coburg_colors) +
  scale_y_continuous(limits = c(0, 100)) +
  annotate("text", x = 1.5, y = 98,
           label = paste0("Treatment effect: ", format_p(p_rotate_2024)),
           size = 4, hjust = 0.5) +
  labs(x = "", y = "Rotation Risk (%) in Year 5") +
  theme_bw() +
  my_theme

# 1.3 Height
ht_coburg <- ggplot(coburg_2024, aes(x = treatment, y = height_m, fill = treatment)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = coburg_colors) +
  annotate("text", x = 1.5, y = max(coburg_2024_diam_df$height_m, na.rm = TRUE) * 1.12,
           label = paste0("Treatment effect: ", format_p(p_coburg_2024_ht["treatmentVexar"])),
           size = 4, hjust = 0.5) +
  # annotate("text", x = 1.175, y = max(coburg_2024$height_m, na.rm = TRUE),
  #          label = format_p(summary(coburg_2024_ht_mod_final)$coefficients[2, 4]),
  #          color = "black", size = 5, hjust = 0) +
  labs(x = "", y = "Height (m) in Year 5") +
  theme_bw() +
  my_theme

# 1.4 Diameter
diam_coburg <- ggplot(coburg_2024_diam_df, aes(x = treatment, y = diameter_mm, fill = treatment)) +
  geom_boxplot(show.legend = FALSE) +
  scale_fill_manual(values = coburg_colors) +
  annotate("text", x = 1.5, y = max(coburg_2024_diam_df$diameter_mm, na.rm = TRUE) * 1.12,
           label = paste0("Treatment effect: ", format_p(p_coburg_2024_diam["treatmentVexar"])), # getting vexar value
           size = 4, hjust = 0.5) +
  # annotate("text", x = 1.175, y = max(coburg_2024_diam_df$diameter_mm, na.rm = TRUE),
  #          label = format_p(summary(coburg_2024_diam_mod_final)$coefficients[2, 4]),
  #          color = "black", size = 5, hjust = 0) +
  labs(x = "", y = "Diameter (mm) in Year 5") +
  theme_bw() +
  my_theme

# # # 1.5 Number of Leaders
leaders_coburg <- ggplot(coburg_2024, aes(x = treatment, y = number_of_leaders, fill = treatment)) +
  geom_violin(show.legend = FALSE) +
  scale_fill_manual(values = coburg_colors) +
  scale_y_continuous(limits = c(1, max(coburg_2024$number_of_leaders, na.rm = TRUE) + 1)) + #gives breathing room at the top 
  annotate("text", x = 1.5, y = 10,
           label = paste0("Treatment effect: ", format_p(p_n_leaders_2024["treatmentVexar"])),
           size = 4, hjust = 0.5) +
  labs(x = "", y = "Number of Leaders in Year 5") +
  theme_bw() +
  my_theme

# design <- "
#   112233
#   #4455#
# "
# 
# coburg_combined <- (deform_coburg + rotate_coburg + ht_coburg + 
#                       diam_coburg + leaders_coburg) +
#   plot_layout(design = design, guides = "collect")
# 
# coburg_combined
# 
# ggsave("figures/coburg_all.png",
#        plot = coburg_combined, dpi = 800, width = 12, height = 8, units = "in")

design <- "
  112233
  #4455#
"

coburg_combined <- (deform_coburg + rotate_coburg + ht_coburg + 
                      diam_coburg + leaders_coburg) +
  plot_layout(design = design, guides = "collect",
              widths = c(1, 1, 1),
              heights = c(1, 1))

ggsave("figures/coburg_all.png",
       plot = coburg_combined, dpi = 800, width = 18, height = 13, units = "in")

# png("figures/coburg_all.png", width = 12, height = 8, units = "in", res = 800)
# print(coburg_combined)
# dev.off()
# 
# fig_combined <- 
#   (p_browse_2019 + p_ht_2019 + p_diam_2019 + 
#      p_deform_2024 + p_rotate_2024 + p_ht_2024 + p_diam_2024) +
#   plot_layout(design = design, guides = "collect") +
#   plot_annotation(theme = theme(legend.position = "none"))

#TABLES##
summary(coburg_2024_ht_mod_final)
model_table <- bind_rows(
  tidy(coburg_2024_ht_mod_final)           |> mutate(response = "Height, Year 5"),
  tidy(chi_deform_2024)           |> mutate(response = "Deform, Year 5"),
  tidy(chi_rotate_2024)           |> mutate(response = "Deform, Year 5"),
  tidy(coburg_2024_diam_mod_final)  |> mutate(response = "Diameter, Year 5"),
  tidy(n_leaders_mod_2024_poisson) |> mutate(response = "Number of Leaders, Year 5"),

) |> #change term names
  mutate(term = recode(term,
                       "treatmentVexar"      = "Vexar Treatment",
                       "(Intercept)"         = "Intercept"
  )) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  dplyr::select(response, everything())

#View(model_table)
#model_table$term

write.csv(model_table, "../Browse_analysis/tables/model_results_coburg.csv", row.names = FALSE)


