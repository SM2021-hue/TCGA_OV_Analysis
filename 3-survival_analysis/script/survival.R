# Load required packages
library(survival)
library(survminer)
library(dplyr)
# Import the ovarian cancer dataset and have a look at it
data(ovarian)
glimpse(ovarian)
## Observations: 26
## Variables: 6
## $ futime   <dbl> 59, 115, 156, 421, 431, 448, 464, 475, 477, 563, 638,...
## $ fustat   <dbl> 1, 1, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,...
## $ age      <dbl> 72.3315, 74.4932, 66.4658, 53.3644, 50.3397, 56.4301,...
## $ resid.ds <dbl> 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 2, 1,...
## $ rx       <dbl> 1, 1, 1, 2, 1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 1, 1, 1, 1,...
## $ ecog.ps  <dbl> 1, 1, 2, 1, 1, 2, 2, 2, 1, 2, 2, 1, 2, 1, 1, 2, 2, 1,...
help(ovarian)

# Dichotomize age and change data labels
ovarian$rx <- factor(ovarian$rx, 
                     levels = c("1", "2"), 
                     labels = c("A", "B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, 
                           levels = c("1", "2"), 
                           labels = c("no", "yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, 
                          levels = c("1", "2"), 
                          labels = c("good", "bad"))

# Data seems to be bimodal
hist(ovarian$age) 

ovarian <- ovarian %>% mutate(age_group = ifelse(age >=50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# Fit survival data using the Kaplan-Meier method (create survival object)
surv_object <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_object #A + behind survival times indicates censored data points.

#fit the Kaplan-Meier curves
fit1 <- survfit(surv_object ~ rx, data = ovarian)
summary(fit1)  

#examine the corresponding survival curve
ggsurvplot(fit1, data = ovarian, pval = TRUE)   
#By convention, vertical lines indicate censored data, their corresponding x values the time at which censoring occurred.
#The log-rank p-value of 0.3 indicates a non-significant result if you consider p < 0.05 to indicate statistical significance. 
#In this study, none of the treatments examined were significantly superior, although patients receiving treatment B are doing better in the first month of follow-up.

# Examine prdictive value of residual disease status
fit2 <- survfit(surv_object ~ resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE) 
#The curves diverge early and the log-rank test is almost significant.
# patients with positive residual disease status have a significantly worse prognosis compared to patients without residual disease.


#Cox proportional hazards models allow you to include covariates. 
#You can build Cox proportional hazards models using the coxph function and visualize them using the ggforest. These type of plot is called a forest plot. 
#It shows so-called hazard ratios (HR) which are derived from the model for all covariates that we included in the formula in coxph. 
#Briefly, an HR > 1 indicates an increased risk of death (according to the definition of h(t)) if a specific condition is met by a patient. 
#An HR < 1, on the other hand, indicates a decreased risk. 
# Fit a Cox proportional hazards model
fit.coxph <- coxph(surv_object ~ rx + resid.ds + age_group + ecog.ps, 
                   data = ovarian)
ggforest(fit.coxph, data = ovarian)

#Every HR represents a relative risk of death that compares one instance of a binary feature to the other instance.
#For example, a hazard ratio of 0.25 for treatment groups tells you that patients who received treatment B have a reduced risk of dying compared to patients who received treatment A (which served as a reference to calculate the hazard ratio). 
#As shown by the forest plot, the respective 95% confidence interval is 0.071 - 0.89 and this result is significant.

##############################################################################################################################################################################################################################################################################
# Load required packages
library(survival)
library(survminer)
library(dplyr)
library(gdata)
avg=read.xls("input.xlsx",sheet=1)

hist(avg$Major_Axis)
avg <- avg %>% mutate(Major_Axis = ifelse(Major_Axis >=55, "High_image_value", "Low_image_value"))
avg$Major_Axis <- factor(avg$Major_Axis)

hist(avg$Minor_Axis)
avg <- avg %>% mutate(Minor_Axis = ifelse(Minor_Axis >=30, "High_image_value", "Low_image_value"))
avg$Minor_Axis <- factor(avg$Minor_Axis)

hist(avg$Ratio)
avg <- avg %>% mutate(Ratio = ifelse(Ratio >=1.8, "High_image_value", "Low_image_value"))
avg$Ratio <- factor(avg$Ratio)


hist(avg$Area)
avg <- avg %>% mutate(Area = ifelse(Area >=400, "High_image_value", "Low_image_value"))
avg$Area <- factor(avg$Area)

hist(avg$Min_distance)
avg <- avg %>% mutate(Min_distance = ifelse(Min_distance >=50, "High_image_value", "Low_image_value"))
avg$Min_distance <- factor(avg$Min_distance)

hist(avg$Max_distance)
avg <- avg %>% mutate(Max_distance = ifelse(Max_distance >=975, "High_image_value", "Low_image_value"))
avg$Max_distance <- factor(avg$Max_distance)

hist(avg$Mean_distance)
avg <- avg %>% mutate(Mean_distance = ifelse(Mean_distance >=480, "High_image_value", "Low_image_value"))
avg$Mean_distance <- factor(avg$Mean_distance)

avg$fustat <- as.numeric(c(1,1,1,1,1,1,1,1,1,1))
avg1=avg[c(2:10,13)]
surv_object <- Surv(time = avg1$days_to_death, event = avg1$fustat)
surv_object #A + behind survival times indicates censored data points.

#fit the Kaplan-Meier curves and examine survival curve
fit1 <- survfit(surv_object ~ Major_Axis, data = avg1)
ggsurvplot(fit1, data = avg1, pval = TRUE)   

fit2 <- survfit(surv_object ~ Minor_Axis, data = avg1)
ggsurvplot(fit2, data = avg1, pval = TRUE)  

fit3 <- survfit(surv_object ~ Ratio, data = avg1)
ggsurvplot(fit3, data = avg1, pval = TRUE)  

fit4 <- survfit(surv_object ~ Area, data = avg1)
ggsurvplot(fit4, data = avg1, pval = TRUE)

fit5 <- survfit(surv_object ~ Min_distance, data = avg1)
ggsurvplot(fit5, data = avg1, pval = TRUE)

fit6 <- survfit(surv_object ~ Max_distance, data = avg1)
ggsurvplot(fit6, data = avg1, pval = TRUE)

fit7 <- survfit(surv_object ~ Mean_distance, data = avg1)
ggsurvplot(fit7, data = avg1, pval = TRUE)


fit.coxph <- coxph(surv_object ~ Ratio + Area + Min_distance + Min_distance + Mean_distance, 
                   data = avg1)
jpeg("hazard_ratio.jpeg",height=720,width=920)
ggforest(fit.coxph, data = avg1)
dev.off()
