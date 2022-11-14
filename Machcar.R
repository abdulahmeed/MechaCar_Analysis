library(dplyr)
library(tidyverse)

megadata<-read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)

# Linear Regression
LinearRegression<-lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data = megadata)

# P-Value, R-Squared
summary(LinearRegression)



susdata<-read.csv("Suspension_Coil.csv",stringsAsFactors = F,check.names = F)
# Total Summary
total_summary<-susdata %>% summarise(Mean=mean(PSI), median=median(PSI),variance=var(PSI),sd=sd(PSI))

# Lot Summary
lot_summary<-susdata %>%  group_by(Manufacturing_Lot) %>% summarise(Mean=mean(PSI), median=median(PSI),variance=var(PSI),sd=sd(PSI), .groups='keep')

# Total T-Test
t.test(susdata$PSI,mu=1500)

# Lot T-Test
t.test(subset(susdata,Manufacturing_Lot=="Lot1")$PSI,mu=1500)
t.test(subset(susdata,Manufacturing_Lot=="Lot2")$PSI,mu=1500)
t.test(subset(susdata,Manufacturing_Lot=="Lot3")$PSI,mu=1500)
