#10/1/2024
#Example analysis of cow pre generals
install.packages("faraway")
install.packages("arm")
install.packages("generalhoslem")

library(faraway)
library(dplyr)
library(arm)
library(generalhoslem)
library(tidyverse)
library(readxl)
install.packages("janitor")
library(janitor)
library(ggplot2)

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

#building model for deformity presence/absence
defmod1 <- glm(deformity_y_n ~ in_out + east_west, cow_2024, family = "binomial")
summary(defmod1)
defmod2 <- glm(deformity_y_n ~ in_out, cow_2024, family = "binomial")
defmod3 <- glm(deformity_y_n ~ east_west, cow_2024, family = "binomial")
defmod4 <- glm(deformity_y_n ~ 1, cow_2024, family = "binomial")

#glmer when using random errors 

#AIC 
AIC.tab <- data.frame(NA,nrow=4,ncol=4)
AIC.tab [1,2] <- summary(defmod1)$deviance + 2*3
AIC.tab [2,2] <- summary(defmod2)$deviance + 2*2
AIC.tab [3,2] <- summary(defmod3)$deviance + 2*2
AIC.tab [4,2] <- summary(defmod4)$deviance + 2*1
AIC.tab [,3] <- AIC.tab[,2] - min(AIC.tab[,2])
AIC.tab [,4] <- exp(-0.5*AIC.tab[,3]) / sum(exp(-0.5*AIC.tab[,3]))
AIC.tab [,1] <- c("in_out + east_west","in_out","east_west","1")

AIC.tab

#Predicting deformity as a funciton of in_out of fence with my best model

#best model
defmod <- glm(deformity_y_n ~ in_out, cow_2024, family = "binomial")

summary(defmod)
#getting unique values
fence_vals <- sort(unique(cow_2024$in_out))
#creating new dataframe
newData1 <- data.frame(in_out = c(rep("In",length(fence_vals)),rep("Out",length(fence_vals))))
newData1$in_out <- as.factor(newData1$in_out)
#creating empty df
summ.best <- matrix(NA,nrow = nrow(newData1), ncol=3)
boots <- 1000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  y <- unlist(simulate(defmod)) #putting in best model here
  ymod <- update(defmod,y ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response") #type = response means we get response variable directly #putting predictions in yestimate dataframe
}
for(i in 1:nrow(newData1)){
  summ.best[i,1] <- mean(yest[i,]) 
  summ.best[i,2] <- quantile(yest[i,],probs=0.025) #confidence interval
  summ.best[i,3] <- quantile(yest[i,],probs=0.975) #confidence interval 
}

#reformatting summ.best for plotting
summ.best <- as.data.frame(summ.best)
summ.best[1,4] <- "In"
summ.best[3,4] <- "Out"
colnames(summ.best)[4] <- "Fence"
# Remove rows 2 and 4
summ.best <- summ.best[-c(2, 4), ]

colnames(summ.best)[1] <- "mean"
colnames(summ.best)[2] <- "low_CI"
colnames(summ.best)[3] <- "high_CI"

# Plotting deformity risk
plot <- ggplot(summ.best, aes(x = Fence, y = mean)) +
  geom_point(size = 4, color = "#0072B2", position = position_dodge(0.5)) +  # Add points for means
  geom_errorbar(aes(ymin = low_CI, ymax = high_CI), width = 0.2, color = "#D55E00", position = position_dodge(0.5)) +  # Confidence intervals
  labs(title = "Predicted Risk of Deformity at Age 8",
       x = "Fence",
       y = "Predicted Occurrence of Deformity") +
  theme_bw() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title
    axis.title.x = element_text(size = 14),  # X-axis title size
    axis.title.y = element_text(size = 14),  # Y-axis title size
    axis.text = element_text(size = 12)       # Axis text size
  )

plot

################################
#####Bringing in 2016 Data#######
################################

cow_2016 <- read.csv("./analysis_ready_data/cow_plots/cow_data_4_7_2016.csv")
#Trim any potential leading/trailing spaces in the deformity_y_n column
cow_2016$browse_y_n <- trimws(cow_2016$browse_y_n)
cow_2016$browse_y_n <- as.factor(cow_2016$browse_y_n)

unique(cow_2016$browse_y_n)

#making all empty cells NA
cow_2016[cow_2016 == ""] <- NA

#building model for deformity presence/absence
bromod1 <- glm(browse_y_n ~ in_out + east_west, cow_2016, family = "binomial")
bromod2 <- glm(browse_y_n ~ in_out, cow_2016, family = "binomial")
bromod3 <- glm(browse_y_n ~ east_west, cow_2016, family = "binomial")
bromod4 <- glm(browse_y_n ~ 1, cow_2016, family = "binomial")

summary(bromod2)
#AIC 
AIC.tab.2016 <- data.frame(NA,nrow=4,ncol=4)
AIC.tab.2016 [1,2] <- summary(defmod1)$deviance + 2*3
AIC.tab.2016 [2,2] <- summary(defmod2)$deviance + 2*2
AIC.tab.2016 [3,2] <- summary(defmod3)$deviance + 2*2
AIC.tab.2016 [4,2] <- summary(defmod4)$deviance + 2*1
AIC.tab.2016 [,3] <- AIC.tab.2016[,2] - min(AIC.tab.2016[,2])
AIC.tab.2016 [,4] <- exp(-0.5*AIC.tab.2016[,3]) / sum(exp(-0.5*AIC.tab.2016[,3]))
AIC.tab.2016 [,1] <- c("in_out + east_west","in_out","east_west","1")

AIC.tab.2016

best_2016_mod <- glm(browse_y_n ~ in_out, cow_2016, family = "binomial")
summary(best_2016_mod)

#getting unique values
fence_vals <- sort(unique(cow_2016$in_out))
#creating new dataframe
newData1 <- data.frame(in_out = c(rep("in",length(fence_vals)),rep("out",length(fence_vals))))
newData1$in_out <- as.factor(newData1$in_out)
#creating empty df
summ.best <- matrix(NA,nrow = nrow(newData1), ncol=3)
boots <- 1000
yest <- matrix(NA,nrow=nrow(newData1),ncol=boots)
for(i in 1:boots){
  y <- unlist(simulate(best_2016_mod)) #putting in best model here
  ymod <- update(best_2016_mod,y ~ .)
  yest[,i] <- predict(ymod,newdata = newData1, type="response") #putting predictions in yestimate dataframe
}
for(i in 1:nrow(newData1)){
  summ.best[i,1] <- mean(yest[i,]) 
  summ.best[i,2] <- quantile(yest[i,],probs=0.025) #confidence interval
  summ.best[i,3] <- quantile(yest[i,],probs=0.975) #confidence interval 
}

#reformatting summ.best for plotting
summ.best <- as.data.frame(summ.best)
summ.best[1,4] <- "In"
summ.best[3,4] <- "Out"
colnames(summ.best)[4] <- "Fence"
# Remove rows 2 and 4
summ.best <- summ.best[-c(2, 4), ]

colnames(summ.best)[1] <- "mean"
colnames(summ.best)[2] <- "low_CI"
colnames(summ.best)[3] <- "high_CI"

# Plotting deformity risk
plot <- ggplot(summ.best, aes(x = Fence, y = mean)) +
  geom_point(size = 4, color = "#0072B2", position = position_dodge(0.5)) +  # Add points for means
  geom_errorbar(aes(ymin = low_CI, ymax = high_CI), width = 0.2, color = "#D55E00", position = position_dodge(0.5)) +  # Confidence intervals
  labs(title = "Predicted Risk of Browse at Year 1",
       x = "Fence",
       y = "Predicted Occurrence of Browse") +
  theme_bw() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center title
    axis.title.x = element_text(size = 14),  # X-axis title size
    axis.title.y = element_text(size = 14),  # Y-axis title size
    axis.text = element_text(size = 12)       # Axis text size
  )

plot

################
#example model for continuous response (height, diameter)
#mod_height <- lmer(height ~ in_out + (1 | site), data = cow_2024)
