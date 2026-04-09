#Pack Forest Rows Trial 
library(janitor)
library(readxl)
library(tidyverse)
library(dplyr)
library(lme4)
library(pbkrtest)
library(faraway)
library(ggplot2)
library(ggsignif)
library(ggpattern)


source("./functions/check_resids_func.R")

Rows_2024 <- read_excel("./analysis_ready_data/pack_forest_rows/Pack Rows Data 2024.xlsx",
                          sheet = "All Data")
Rows_2024 <- clean_names(Rows_2024)

#ht is in cm, diameter is in mm
Rows_2015 <- read.csv("./analysis_ready_data/pack_forest_rows/2015_meas_cleanv2.csv")
Rows_2015 <- clean_names(Rows_2015)
######################
#1.1 2015 Data Clean
######################

#need to clean this one up
colnames(Rows_2015)

# colnames(Rows_2015) <- c(
#   "rep", "row", "treat_name", "concat_treatment",
#   "tree_nmbr", "species",
#   # planting
#   "plant_date", "plant_ht", "plant_diam",
#   # week 1
#   "wk1_date", "wk1_browse", "wk1_mort", "wk1_ht", "wk1_initial_current_ht",
#   "wk1_browse_loss", "wk1_browse_freq", "wk1_mort_count",
#   # 2 month
#   "month2_date", "month2_browse", "month2_mort", "month2_ht", "month2_current_ht",
#   "month2_browse_loss", "month2_browse_freq", "month2_mort_count",
#   # 4 month
#   "month4_date", "month4_browse", "month4_mort", "month4_ht", "month4_current_ht",
#   "month4_browse_loss", "month4_browse_freq", "month4_mort_count"
# )

colnames(Rows_2015) <-c("rep", "row", "treat_name", "tree_nmbr",
  "species",
  #planting
  "plant_date", "plant_ht", "plant_diam",
  #week 1
  "wk1_date", "wk1_browse_y_n", "wk1_mort_y_n", "wk1_ht",
  #2 month
  "month2_date", "month2_browse_y_n_1", "month2_mort_y_n_1", "month2_vexar_damage_failure",
  "month2_ht_2",
  #month 4
  "month4_date", "month4_browse_y_n", "month4_mort_y_n",
  "month4_vexar_damage_failure_1", "month4_ht", "month4_diam")     

#there were a couple extra rows we need to drop 
Rows_2015 <- Rows_2015 %>%
  filter(!is.na(rep) & rep != "")

# cleaning up month four mort
Rows_2015$month4_mort <- as.character(Rows_2015$month4_mort)
Rows_2015$month4_mort[is.na(Rows_2015$month4_mort) | Rows_2015$month4_mort == ""] <- "N"
Rows_2015$month4_mort <- as.factor(Rows_2015$month4_mort)

# cleaning up month four browse
Rows_2015$month4_browse_y_n <- as.character(Rows_2015$month4_browse_y_n)
Rows_2015$month4_browse_y_n[is.na(Rows_2015$month4_browse_y_n) | Rows_2015$month4_browse_y_n == ""] <- "N"
Rows_2015$month4_browse_y_n <- as.factor(Rows_2015$month4_browse_y_n)

Rows_2015$treat_name <- gsub("CO-PLANTING", "CO_PLANTING", Rows_2015$treat_name)


#########
#1.2 WRC 4 month mortality 2015
##########

#making WRC the ref treatment
Rows_2015$treat_name <- relevel(factor(Rows_2015$treat_name), ref = "WRC CTRL")

#Let's Just Look at WRC Response
WRC_Rows_2015 <- Rows_2015 %>%
  filter(species == "WRC")

levels(factor(WRC_Rows_2015$treat_name))
unique(WRC_Rows_2015$treat_name)

wrc_mort_mod_2015_final <- glmer(month4_mort ~ treat_name + (1|rep), WRC_Rows_2015, family = "binomial")

summary(wrc_mort_mod_2015_final) #vexar sig

check_resids(wrc_mort_mod_2015_final) #okay for binomial 

simulationOutput <- simulateResiduals(wrc_mort_mod_2015_final)
plot(simulationOutput)


#########
#1.25 WRC 4 month browse 2015
##########

wrc_browse_mod_2015_final <- glmer(month4_browse_y_n ~ treat_name + (1|rep), WRC_Rows_2015, family = "binomial")

summary(wrc_browse_mod_2015_final) #vexar sig

check_resids(wrc_browse_mod_2015_final) #okay for binomial 

simulationOutput <- simulateResiduals(wrc_browse_mod_2015_final)
plot(simulationOutput)

#########
#1.3 WRC 4 month height 2015
##########

hist(WRC_Rows_2015$month4_ht) #looks very normal

#filter so that we don't have any without height values
WRC_Rows_2015_ht_vals <- WRC_Rows_2015 %>%
  filter(!is.na(month4_ht))

wrc_ht_mod_2015_final <- lmerTest::lmer(month4_ht ~ treat_name + (1|rep), WRC_Rows_2015_ht_vals) #mixed effect model so using lmer

summary(wrc_ht_mod_2015_final) #all sig

check_resids(wrc_ht_mod_2015_final) #much better here 


#################
#1.4 WRC 4 month diameter 2015
####################
#filer where don't have diameter 
#filter so that we don't have any without height values
WRC_Rows_2015_diam_vals <- WRC_Rows_2015 %>%
  filter(!is.na(month4_diam)) %>%
  filter(!month4_diam == 41.00) #bad measurement

hist(WRC_Rows_2015_diam_vals$month4_diam)

wrc_diam_mod_2015_final <- lmerTest::lmer(month4_diam ~ treat_name + (1|rep), WRC_Rows_2015_diam_vals) #mixed effect model so using lmer

summary(wrc_diam_mod_2015_final) #all sig

check_resids(wrc_diam_mod_2015_final) #much better here 



####################################
#2. 2024 Data
####################################
Rows_2024

#t test to see whether we can combine these
t.test(diameter_mm ~ treatment, 
       data = Rows_2024 |> filter(treatment %in% c("P OR CC 1", "P OR CC 2"))) #diammeter not different 
t.test(height_m ~ treatment, 
       data = Rows_2024 |> filter(treatment %in% c("P OR CC 1", "P OR CC 2")))
# deformity
# chisq.test(table(Rows_2024 |> 
#                    filter(treatment %in% c("P OR CC 1", "P OR CC 2")) |>
#                    select(treatment, deformity_y_n)))
# # rotation risk
# chisq.test(table(Rows_2024 |>
#                    filter(treatment %in% c("P OR CC 1", "P OR CC 2")) |>
#                    select(treatment, rotation_risk_y_n)))

table(Rows_2024$rotation_risk_y_n, Rows_2024$treatment)

#Good to combine


#combine "P OR CC 1", "P OR CC 2" because considered the same in this analysis and is messing up models
Rows_2024$treatment <- as.character(Rows_2024$treatment)
Rows_2024$treatment[Rows_2024$treatment %in% c("P OR CC 1", "P OR CC 2")] <- "P OR CC"

#Drop empty rows 
Rows_2024 <- Rows_2024 %>%
  filter(!is.na(plot))

#this drops Sitka spruce and drops extra deformity info for each WRC
WRC_2024 <- Rows_2024 %>%
  filter(species == "WRC")         

##################
#2.1 Deformity 2024
####################

#testing taking this out because it was so significant
# WRC_2024 <- WRC_2024 %>%
#   filter(treatment != "FC") 

WRC_2024$deformity_y_n <- as.factor(WRC_2024$deformity_y_n)


#making this treatment the ref treatment
WRC_2024$treatment <- relevel(factor(WRC_2024$treatment), ref = "P OR CC")

wrc_deform_mod_2024_final <- glmer(deformity_y_n ~ treatment + (1|plot), WRC_2024, family = "binomial")

summary(wrc_deform_mod_2024_final)
#fenced treatment caused high deformity 


check_resids(wrc_deform_mod_2024_final)

simulationOutput <- simulateResiduals(wrc_deform_mod_2024_final)
plot(simulationOutput)
##################
#2.2 Rotation Risk 2024
####################
WRC_2024$rotation_risk_y_n <- as.factor(WRC_2024$rotation_risk_y_n)

wrc_rotate_mod_2024_final <- glmer(rotation_risk_y_n ~ treatment + (1|plot), WRC_2024, family = "binomial")

summary(wrc_rotate_mod_2024_final) 
#Positive coefficient → that treatment has higher odds of rotation risk compared to the reference
#fenced treatment caused sig high risk of rotation

check_resids(wrc_rotate_mod_2024_final)

#dharma check resids
simulationOutput <- simulateResiduals(wrc_rotate_mod_2024_final)
plot(simulationOutput)

##################
#2.3 Height 2024
####################

hist(WRC_2024$height_m)

wrc_ht_mod_2024_final <- lmerTest::lmer(height_m ~ treatment + (1|plot), WRC_2024)

summary(wrc_ht_mod_2024_final) #vexar sig
check_resids(wrc_ht_mod_2024_final)

##################
#2.4 Diameter 2024
####################
hist(WRC_2024$diameter_mm)
#need to drop weird diameter measurements here
WRC_2024diam_df <- WRC_2024 %>%
  filter(!grepl("Diameter|measured at|diam @ |m @ 0\\.5|m @ 0\\.25|m @ 0\\.9|measurements at 0\\.5|measurements at 0\\.25|measurements at 0\\.9|D at 0\\.|D taken at|D, DfV|Diameter taken at", 
                comments, ignore.case = TRUE))

wrc_diam_mod_2024_final <- lmerTest::lmer(diameter_mm ~ treatment + (1|plot), WRC_2024diam_df)

summary(wrc_diam_mod_2024_final) #all are sig
#vexar is the winner here
#co planting reduced diameter
#FC also increased diameter but with the deformities, and difficulty of maitenance, not a good option
check_resids(wrc_diam_mod_2024_final)


################################
#3) Plotting
################################

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
  #  aspect.ratio = 1.2
  )

############
#3.05 plotting browse
wrc_browse_mod_2015_final

browse_2015 <- WRC_Rows_2015 |>
  group_by(treat_name) |>
  summarise(pct_browse = mean(month4_browse_y_n  == "Y") * 100) |>
  ggplot(aes(x = treat_name, y = pct_browse, fill = treat_name)) +
  geom_bar_pattern(stat = "identity", show.legend = FALSE,
                   pattern = "stripe",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black") +
  scale_fill_manual(values = c(
    "WRC CTRL"    = "#0072B2",
    "CO_PLANTING" = "#E69F00",
    "FENCE CTRL"  = "#009E73",
    "PLANTSKYDD"  = "#CC79A7",
    "VEXAR"       = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "WRC CTRL" = "Control",
    "PLANTSKYDD"= "Plantskydd",
    "CO_PLANTING"  = "SS Co Planting",
    "FENCE CTRL"    = "Flexible Fencing",
    "VEXAR"       = "Vexar"
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  # annotate("text", x = 4.5, y = 97,
  #          label = format_p(p_mort_2015),
  #          color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "",
       y = "Browse (%) in Month 4") +
  theme_bw() +
  annotate("text", x = 3, y = 96,
           label = "All effects: p < 0.05",
           size = 4, hjust = 0.5) +
  my_theme

#3.1) Plotting 2015 Mortality 
summary(wrc_mort_mod_2015_final) #vexar sig

mort_2015 <- WRC_Rows_2015 |>
  group_by(treat_name) |>
  summarise(pct_mort = mean(month4_mort == "Y") * 100) |>
  ggplot(aes(x = treat_name, y = pct_mort, fill = treat_name)) +
  geom_bar_pattern(stat = "identity", show.legend = FALSE,
                   pattern = "stripe",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black") +
  scale_fill_manual(values = c(
    "WRC CTRL"    = "#0072B2",
    "CO_PLANTING" = "#E69F00",
    "FENCE CTRL"  = "#009E73",
    "PLANTSKYDD"  = "#CC79A7",
    "VEXAR"       = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "WRC CTRL" = "Control",
    "PLANTSKYDD"= "Plantskydd",
    "CO_PLANTING"  = "SS Co Planting",
    "FENCE CTRL"    = "Flexible Fencing",
    "VEXAR"       = "Vexar"
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  # annotate("text", x = 4.5, y = 97,
  #          label = format_p(p_mort_2015),
  #          color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "",
       y = "Mortality (%) in Month 4") +
  theme_bw() +
  annotate("text", x = 3, y = 96,
           label = "Vexar: p = 0.039\nAll other effects: ns",
           size = 4, hjust = 0.5) +
  my_theme

mort_2015
############
#3.2) Plotting 2015 Ht 
summary(wrc_ht_mod_2015_final)


ht_2015 <- ggplot(WRC_Rows_2015, aes(x = treat_name, y = month4_ht, fill = treat_name)) +
  geom_boxplot_pattern(show.legend = FALSE,
                       pattern = "stripe",
                       pattern_fill = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black") +
  scale_fill_manual(values = c(
    "WRC CTRL"    = "#0072B2",
    "CO_PLANTING" = "#E69F00",
    "FENCE CTRL"  = "#009E73",
    "PLANTSKYDD"  = "#CC79A7",
    "VEXAR"       = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "WRC CTRL" = "Control",
    "PLANTSKYDD"= "Plantskydd",
    "CO_PLANTING"  = "SS Co Planting",
    "FENCE CTRL"    = "Flexible Fencing",
    "VEXAR"       = "Vexar"
  )) +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = "",
       y = "Height (cm) in Month 4") +
  theme_bw() +
  annotate("text", x = 3, y = max(WRC_Rows_2015$month4_ht, na.rm = TRUE)*1.12,
           label = "All effects: p < 0.05",
           size = 4, hjust = 0.5) +
  my_theme

ht_2015

##### 
#Plotting 2015 diameter
#####


#hist(WRC_Rows_2015_diam_vals$month4_diam)
summary(wrc_diam_mod_2015_final) #all s

diam_2015 <- ggplot(WRC_Rows_2015_diam_vals, aes(x = treat_name, y = month4_diam, fill = treat_name)) +
  geom_boxplot_pattern(show.legend = FALSE,
                       pattern = "stripe",
                       pattern_fill = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black") +
  scale_fill_manual(values = c(
    "WRC CTRL"    = "#0072B2",
    "CO_PLANTING" = "#E69F00",
    "FENCE CTRL"  = "#009E73",
    "PLANTSKYDD"  = "#CC79A7",
    "VEXAR"       = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "WRC CTRL" = "Control",
    "PLANTSKYDD"= "Plantskydd",
    "CO_PLANTING"  = "SS Co Planting",
    "FENCE CTRL"    = "Flexible Fencing",
    "VEXAR"       = "Vexar"
  )) +
  #scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  labs(x = "",
       y = "Diameter (mm) in Month 4") +
  theme_bw() +
  annotate("text", x = 3, y = max(WRC_Rows_2015_diam_vals$month4_diam, na.rm = TRUE)*1.12,
           label = "Flexible fencing and plantskydd: p < 0.05",
           size = 4, hjust = 0.5) +
  my_theme




############
#3.3) Plotting 2024 deformity 

# unique(WRC_2024$treatment)
summary(wrc_deform_mod_2024_final)

# "WRC CTRL"    = "#0072B2",
# "CO_PLANTING" = "#009E73",
# "FENCE CTRL"  = "#E69F00",
# "PLANTSKYDD"  = "#CC79A7",
# "VEXAR"       = "#56B4E9"

deform_2024 <- WRC_2024 |>
  group_by(treatment) |>
  summarise(pct_deform = (sum(deformity_y_n == "Y", na.rm = TRUE) / n()) * 100) |>
  ggplot(aes(x = treatment, y = pct_deform, fill = treatment)) +
  geom_bar_pattern(stat = "identity", show.legend = FALSE,
                   pattern = "stripe",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black") +
  scale_fill_manual(values = c(
    "P OR CC"  = "#0072B2",
    "CP"       = "#E69F00",
    "FC"       = "#009E73",
    "V"        = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "P OR CC" = "Control",
    "CP"      = "SS Co Planting",
    "FC"      = "Flexible Fencing",
    "V"       = "Vexar"
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  # annotate("text", x = 3.5, y = 97,
  #          label = format_p(p_deform_2024),
  #          color = "black", size = 5, hjust = 0, fontface = "bold.italic") +
  labs(x = "",
       y = "Deformity (%) in Year 10") +
  theme_bw() +
  annotate("text", x = 2.5, y = 96,
           label = "Flexible fencing: p < 0.001\nAll other effects: ns",
           size = 4, hjust = 0.5) +
  my_theme

deform_2024

############
#3.4) 2024 Rotation Risk

summary(wrc_rotate_mod_2024_final) #FC sig

rotate_2024 <- WRC_2024 |>
  group_by(treatment) |>
  summarise(pct_rotate = (sum(rotation_risk_y_n == "Y", na.rm = TRUE) / n()) * 100) |>
  ggplot(aes(x = treatment, y = pct_rotate, fill = treatment)) +
  geom_bar_pattern(stat = "identity", show.legend = FALSE,
                   pattern = "stripe",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.15,
                   pattern_spacing = 0.03,
                   color = "black") +
  scale_fill_manual(values = c(
    "P OR CC"  = "#0072B2",
    "CP"       = "#E69F00",
    "FC"       = "#009E73",
    "V"        = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "P OR CC" = "Control",
    "CP"      = "SS Co Planting",
    "FC"      = "Flexible Fencing",
    "V"       = "Vexar"
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "",
       y = "Rotation risk (%) in Year 10") +
  theme_bw() +
  annotate("text", x = 2.5, y = 95,
           label = "Flexible fencing: p = 0.011\nAll other effects: ns",
           size = 4, hjust = 0.5) +
  my_theme

rotate_2024

############
#3.5) 2024 height
#hist(WRC_2024$height_m)
summary(wrc_ht_mod_2024_final) #sig effect of vexar on height

ht_2024 <- ggplot(WRC_2024, aes(x = treatment, y = height_m, fill = treatment)) +
  geom_boxplot_pattern(show.legend = FALSE,
                       pattern = "stripe",
                       pattern_fill = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black") +
    scale_fill_manual(values = c(
      "P OR CC"  = "#0072B2",
      "CP"       = "#E69F00",
      "FC"       = "#009E73",
      "V"        = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "P OR CC" = "Control",
    "CP"      = "SS Co Planting",
    "FC"      = "Flexible Fencing",
    "V"       = "Vexar"
  )) +
  labs(x = "",
       y = "Height (m) in Year 10") +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) + #little padding at top
  theme_bw() +
  annotate("text", x = 2.5, y = max(WRC_2024$height_m, na.rm = TRUE) * 1.12,
           label = "Vexar: p < 0.05\nAll other effects: ns",
           size = 4, hjust = 0.5) +
  my_theme

#ht_2024


############
#3.6) 2025 diameter
summary(wrc_diam_mod_2024_final) #vexar is the winner here
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

diam_2024 <- ggplot(WRC_2024diam_df, aes(x = treatment, y = diameter_mm, fill = treatment)) +
  geom_boxplot_pattern(show.legend = FALSE,
                       pattern = "stripe",
                       pattern_fill = "black",
                       pattern_angle = 45,
                       pattern_density = 0.15,
                       pattern_spacing = 0.03,
                       color = "black") +
  scale_fill_manual(values = c(
    "P OR CC"  = "#0072B2",
    "CP"       = "#E69F00",
    "FC"       = "#009E73",
    "V"        = "#56B4E9"
  )) +
  scale_x_discrete(labels = c(
    "P OR CC" = "Control",
    "CP"      = "SS Co Planting",
    "FC"      = "Flexible Fencing",
    "V"       = "Vexar"
  )) +
  labs(x = "",
       y = "Diameter (mm) in Year 10") +
  theme_bw() +
  annotate("text", x = 2.5, y = max(WRC_2024diam_df$diameter_mm, na.rm = TRUE) * 1.12,
           label = "All effects: p < 0.05",
           size = 4, hjust = 0.5) +
  my_theme

diam_2024

###

library(patchwork)

six_plots <- (browse_2015 + mort_2015 + ht_2015 + diam_2015) /
  (deform_2024 + rotate_2024 +  ht_2024 + diam_2024 )

# Display
six_plots

# Save (same output path you used earlier, adjust name as desired)
# ggsave(
#   filename = "figures/PF_row_all.png",
#   plot = six_plots, dpi = 800, width = 22, height = 12, units = "in"
# )

eight_plots <- wrap_plots(list(browse_2015, mort_2015, ht_2015, diam_2015,
                               deform_2024, rotate_2024, ht_2024, diam_2024),
                          nrow = 2, ncol = 4)

ggsave("figures/PF_row_all.png",
       plot = eight_plots, dpi = 800, width = 24, height = 12, units = "in")



#TABLES##
wrc_browse_mod_2015_final

model_table <- bind_rows(
  tidy(wrc_browse_mod_2015_final)           |> mutate(response = "Browse, Month 4"),
  tidy(wrc_mort_mod_2015_final)           |> mutate(response = "Mortality, Month 4"),
  tidy(wrc_ht_mod_2015_final)           |> mutate(response = "Height, Month 4"),
  tidy(wrc_diam_mod_2015_final)           |> mutate(response = "Diameter, Month 4"),
  tidy(wrc_deform_mod_2024_final)           |> mutate(response = "Deform Risk, Year 10"),
  tidy(wrc_rotate_mod_2024_final)           |> mutate(response = "Rotion Risk, Year 10"),
  tidy(wrc_ht_mod_2024_final)  |> mutate(response = "Height, Year 10"),
  tidy(wrc_diam_mod_2024_final) |> mutate(response = "Diameter, Year 10"),
  
) |> #change term names
  mutate(term = recode(term,
                       "treatmentCP"            = "SS Co-Planting",
                       "treatmentFC"            = "Flexible Fencing",
                       "treatmentV"            = "Vexar",
                       "(Intercept)"         = "Intercept",
                       "treat_nameCO_PLANTING"            = "SS Co-Planting",
                       "treat_nameFENCE CTRL"            = "Flexible Fencing",
                       "treat_nameVEXAR"            = "Vexar",
                       "treat_namePLANTSKYDD"         = "Plantskydd"
  )) |>
  mutate(across(where(is.numeric), ~round(., 4))) |>
  dplyr::select(response, everything())

#View(model_table)
#model_table$term

write.csv(model_table, "../Browse_analysis/tables/model_results_row_plots.csv", row.names = FALSE)

wrc_diam_mod_2024_final
wrc_mort_mod_2015_final

colnames(WRC_2024)

