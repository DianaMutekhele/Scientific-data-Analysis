##########################################################
####### Lab 8 for Scientific Data Analysis using R #######
####### Copyright - HURU School 2021 #######
##########################################################

# Import Libraries
library(tidyverse) # library for tidying data
library(rstatix) # -- For dplyr pipe friendly statistical tests
library(readr) # ----> Import Readr
library(ggpubr) # ---> Publication ready plots

# Import Data for New Fertilizer treatment 
newfertilizer <- read_csv(file ="tomato_new_treatment.csv") 
glimpse(newfertilizer)

view(newfertilizer)

# Import Data for investigation of Altitude Differences in Airlines
altitudedifference <- read_csv(file ="airline_altitude.csv") 
glimpse(altitudedifference)

view(altitudedifference)

# Import Data for flight delay times
flighttimes <- read_csv(file ="flightdelays.csv") 
glimpse(flighttimes)

view(flighttimes)

# Import Data for flight weight and toonage coorelation 
weight_load <- read_csv(file ="flight_coorelation_weight_tonnage.csv")
glimpse(weight_load)

view(weight_load)
################ QUESTION 1: ######################################
## 1. Does application of the new fertilizer increase tomato yield tonnage ##
### Null hypothesis: There is no difference in tomato yield tonnage when there is or there is no fertilizer ###
#### Alternative hypothesis: There is a difference in tomato yield tonnage when fertilizer is added ###

############### Scientific Test - Dependent/Paired T-test/Related Samples/Matched Pairs/Paired Sample T-test #####################


glimpse(newfertilizer)

## Transform from Wide to Long Format ####

newfertilizer <- newfertilizer %>%
  gather(key = "Treatment", value = "Tonnage", Control, Treatment)
head(newfertilizer, 20)

# Using the "pipe-friendly" R-statix package

fertilizerstats <- newfertilizer  %>% 
  t_test(Tonnage ~ Treatment, paired = TRUE) %>%
  add_significance()

fertilizerstats 

# Create a box plot

bxp <- ggpaired(newfertilizer, x = "Treatment", y = "Tonnage", order = c("Control", "Treatment"),
                ylab = "Tonnage", xlab = "Groups")

# Add p-value and significance levels
fertilizerstats <- fertilizerstats %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(fertilizerstats, tip.length = 0) +
  labs(subtitle = get_test_label(fertilizerstats, detailed= TRUE))
# Interpretation
# The p-value of the test is 0.00001, which is less than the significance level alpha = 0.05. 
#We can then reject null hypothesis and conclude that the average tonnage of the untreated tomato farms is statistically significant from the treated farms

## Conclusions ##
#---------------------------------------------------------------------------------------------

################ QUESTION 2: ######################################
## 1. Does Airline one fly at a higher altitude compared to Airline 2 ##
### Null hypothesis: There is no difference in altitude between the first and second airline ###
#### Alternative hypothesis: There is a difference in altitude between the first and second airline ###

############### Scientific Test - Independent/ UnPaired T-test #####################

glimpse(altitudedifference)

### Carry out the unpaired T-test with paired = False ###
altitudestats <- altitudedifference %>% 
  t_test(Altitude ~ Airline  , paired = FALSE) %>%
  add_significance()

altitudestats

# Create a box-plot
bxp <- ggboxplot(
  altitudedifference, x = "Airline", y = "Altitude", 
  ylab = "Altitude", xlab = "Airline", add = "jitter"
)

# Add p-value and significance levels
stat.test <- altitudestats %>% add_xy_position(x = "group")
bxp + 
  stat_pvalue_manual(stat.test, tip.length = 0) +
  labs(subtitle = get_test_label(stat.test, detailed = TRUE))

# Interpretation
# The p-value of the test is <0.0001, which is less than the significance level alpha = 0.05. We can then reject null hypothesis and conclude that the average altitude of Qatar is statistically significant from that of KQ #

## Conclusions ##


---------------------------------------------------------------------------------------------
  
############################ QUESTION 3: ######################################
## 1. Do Different  ##
### Null hypothesis: There is no difference in delays between (ANOVA) and within Airlines (Tukey T-test) ###
#### Alternative hypothesis: There is a difference in delays between and within Airlines ###

############### Scientific Test - Independent/ UnPaired T-test #####################

View(flighttimes)

ggboxplot(flighttimes, x = "Airline", y = "Delay")

anova_one_way <- aov(Delay~Airline, data = flighttimes)
summary(anova_one_way)

#
# Interpretation
# The p-value of the test is <2e-16 , which is less than the significance level alpha = 0.05 and very significant with ***. We can then reject null hypothesis and conclude that there is a difference in punctuality between the airlines #

#### BUT WHICH ONE ####

# Tukey test to study each pair of treatment :

TUKEY <- TukeyHSD(x=anova_one_way, 'Airline', conf.level=0.95)

# P-values to look for comparisons

TUKEY

# diff       lwr       upr     p adj
# Luftansa-KQ    -0.8728641 -3.616039  1.870311 0.7241041 <- No difference
# Qatar-KQ       45.4565476 42.713373 48.199723 0.0000000 <- Significant difference
# Qatar-Luftansa 46.3294118 43.477194 49.181629 0.0000000 <- Significant difference

# Tuckey test graphical representations

plot(TUKEY, col="blue")

## Conclusions ##


#-------------------------------------------------------------------------------------
  
  ############################ QUESTION 4: ######################################
## Relationships  ##
### Null hypothesis: There is no relationship between weights and tonnage ###
#### Alternative hypothesis: There is a difference in delays between and within Airlines ###

############### Scientific Test - Independent/ UnPaired T-test #####################
glimpse(weight_load)# Fertilizer treatment data

# Change the NA's
weight_load <- weight_load %>%
  mutate(Tonnage = if_else(is.na(Tonnage), 0, Tonnage))
weight_load <- weight_load %>%
  filter(!is.na(Tonnage))
glimpse(weight_load)

# Plot a Scatter plot
plot(weight_load$Weight,weight_load$Tonnage)

# Get the coorelation plot
corelation <- cor(weight_load$Weight,weight_load$Tonnage)

# Store the coorelation

corelation


# Interpretation

# The Coorelation is 0.8896119 - meaning thats' how much x explains 

################################################################################
### Chi Square test Example ####################################################

data_frame <- read_csv("treatment.csv")  #Reading CSV




View(data_frame)
### Null hypothesis: There is no significant relationship between treatment and improvement ###
#### Alternative hypothesis: There is significant relationship between treatment and improvement groups

table(data_frame$treatment, data_frame$improvement)

chisq.test(data_frame$treatment, data_frame$improvement, correct=FALSE)

#We have a high chi-squared value and a p-value of less than 0.05 significance level.
#So we reject the null hypothesis and conclude that treated and not-treated groups have a significant relationship.
##################################################################################
## Assumptions above is that data is normal #####################################
#################################################################################

#################################################################
#Many of statistical tests including correlation, regression, t-test, and analysis of variance (ANOVA) assume some certain characteristics about the data. They require the data to follow a normal distribution or Gaussian distribution. These tests are called parametric tests, because their validity depends on the distribution of the data.
#################################################################
################# Checking normality visually ###################
#################################################################

library("ggpubr")
ggdensity(newfertilizer$Tonnage[newfertilizer$Treatment == "Treatment"],
          fill = "lightgray",
          main = "Toonage after Treatment ",
          xlab = "Toonage")

library(car)
qqPlot(newfertilizer$Tonnage[newfertilizer$Treatment == "Treatment"])
#################################################################
################# Visual inspection, described in the previous section, is usually unreliable. It’s possible to use a significance test comparing the sample distribution to a normal one in order to ascertain whether data show or not a serious deviation from normality.There are several methods for normality test such as Kolmogorov-Smirnov (K-S) normality test and Shapiro-Wilk’s test. ###################
#################################################################
?car

# If the test is significant that means our data is non-normal
#Spread the data into treatment & control columns
newfertilizer <- newfertilizer %>%
  spread(key = Treatment, value = Tonnage)
glimpse(newfertilizer)

shapiro.test(newfertilizer$Control)
shapiro.test(newfertilizer$Treatment)

############################################################################
###### Code for non-normal tests ##########
###########################################################################

# Hints 
#########################################################################
########## Non-normal two paired datasets - WILCOXON TEST ###############
##########################################################################
# Compute t-test
res <- wilcox.test(weight ~ group, data = my_data, paired = TRUE)

# print only the p-value
res$p.value

#####################################################
########## Non-normal more than two paired datasets/columns  - FRIEDMAN TEST ###########
######################################################
# Compute t-test
friedman_test()


#####################################################
########## Non-normal two un-paired datasets - MANN -U - WHITNEY ###########
######################################################
# Compute t-test
res <- wilcox.test(weight ~ group, data = my_data, paired = FALSE)

# print only the p-value
res$p.value

#####################################################
########## Non-normal more than two paired datasets/columns - KRUSKAL WALLIS TEST  ###########
######################################################
# Compute t-test
kruskal.test(weight ~ group, data = my_data)
