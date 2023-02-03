# Load Library 
install.packages("readxl")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)



# ------------------------DELIVERABLE 1 ----------------------------------------

# Import and read csv as a dataframe
mecha_table<- read.csv('MechaCar_mpg.csv', check.names = F, stringsAsFactors = F )
class(mecha_table)
head(mecha_table)

# Perform linear regression using the lm() function
# In the lm() function , pass 6 variables (columns)
# add columns to the dataframe
#create linear model
lm(mpg~vehicle_length+vehicle_weigth+spoiler_angle+ground_clearance+AWD, date = mecha_table) 

# Using the summary() function, determine the p-value and the r-squared value for the linear regression model.
#summarize linear model
summary(lm(mpg~vehicle_length+vehicle_weigth+spoiler_angle+ground_clearance+AWD, date = mecha_table)) 


# ------------------------DELIVERABLE 2 ----------------------------------------

# Import and read in the Suspension_Coil.csv file as a table.
sus_coil <- read.csv('Suspension_Coil.csv', check.names = F, stringsAsFactors = F )

# use summarize function to get mean, median and mode
total_summary <- sus_coil %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI))

# create a lot_summary dataframe using the group_by() 
lot_summary <- sus_coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI), Median = median(PSI), Variance = var(PSI), SD = sd(PSI), .groups = 'keep')


# ------------------------DELIVERABLE 3 ----------------------------------------

# perform T-Test

t.test((sus_coil$PSI), mu = 1500)
t.test(subset(sus_coil,Manufacturing_Lot=="Lot1")$PSI, mu = 1500)
t.test(subset(sus_coil,Manufacturing_Lot=="Lot2")$PSI, mu = 1500)
t.test(subset(sus_coil,Manufacturing_Lot=="Lot3")$PSI, mu = 1500)



