##############################
#                            #
#        Blue Team 4         #
#                            #
#     Survival Analysis:     #
#       Homework #1          #
#                            #
#                            #
#                            #
##############################




library(ggplot2)
library(survival)
library(survminer)
library(muhaz)
library(tidyverse)
library(scales)

# This data is Type 1 Right Censored.
# Read in the Katrina dataset.

kat <- read_csv("C:/Users/senor/Documents/Survival_Analysis/katrina.csv") # Define katrina dataset path. 
str(kat) #Checking the structure of the dataset and unique values of reason. 
unique(kat$reason)

# Look at summary statistics for each reason. % survived? % of pumps in each failure? median survival times?

#Histogram of the percentage that survived.
summary(kat$survive)

kat$outcome <- ifelse(kat$survive == 1, "Survived", "Failed") # Changing 1s and 0s to Survived and Failed respectively. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Creating a Histogram of Failed and Survived % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggplot(kat, aes(x = outcome)) + geom_bar(aes(y = ..prop.., group = 1), 
                                         position = "identity", 
                                         alpha = 0.8, fill = "tomato2") + 
  geom_text(aes( label = scales::percent(..prop..),  y= ..prop.. , group = 1), stat= "count", vjust = -.5) +   
  labs(y = "Percent", x = "Outcome") + 
  scale_y_continuous(labels=percent, limits = c(0,1)) +
  ggtitle("Overall Survival Rates")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Creating a Histogram of Failure by Reason % ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

kat$reason_fail <- ordered(kat$reason, levels = c(0,1,2,3,4), labels = c("No Failure", "Flood", "Motor", "Surged", "Trash"))
ggplot(subset(kat, reason != 0 ), aes(x = reason_fail)) + geom_bar(aes(y = ..prop.., group = 1), 
                                                                   position = "identity", 
                                                                   alpha = 0.8, fill = "cyan") + 
  geom_text(aes( label = scales::percent(..prop..),  y= ..prop.. , group = 1), stat= "count", vjust = -.5) +   
  labs(y = "Percent", x = "Reason for Failure") + 
  scale_y_continuous(labels=percent, limits = c(0,.5)) +
  ggtitle("Distribution of Failure By Reason")

# Looks like an even distribution of reasons for failure. 

survived <- subset(kat, survive == 1)
summary(survived$hour)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Survival analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Overall survival plot.
#Ask about the reason subsetting.
kat_fit <- survfit(Surv(hour, survive ==  0) ~ 1, data = kat)
ggsurvplot(kat_fit, data = kat, conf.int = FALSE, xlim = c(4,55), palette = "purple1", surv.median.line = "v", title = "Overall Survival Plot")

# Stratify by reason

kat_reason <- survfit(Surv(hour, survive == 0) ~ reason, data = kat, subset = reason !=0)
ggsurvplot(kat_reason, conf.int = TRUE, alpha = 1,legend.labs = c("Flood", "Motor", "Surged", "Trash"), 
           xlim = c(5, 50), surv.median.line = "v", title = "Survival Plots by Reason of Failure")

# Pairwise Difference Tests

pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 0, data = subset(kat, reason != 0))
pairwise_survdiff(Surv(hour, survive == 0) ~ reason, rho = 1, data = subset(kat, reason != 0))

# Creating Hazard plots
kat$inverse_survive <- ifelse(kat$survive == 1, 0, 1)
kat_haz <- with(kat, kphaz.fit(hour, inverse_survive)) 
kphaz.plot(kat_haz, main = "Hazard Function", las = 0, bty = "n")

#Cumulative hazard plot
ggsurvplot(kat_fit, fun = "cumhaz") + ggtitle("Overall Cumulative Hazard Plot")
ggsurvplot(kat_reason, fun = "cumhaz", conf.int = TRUE, alpha = 1,legend.labs = c("Flood", "Motor", "Surged", "Trash"), title = "Cumulative Hazard Plots by Reason of Failure")
