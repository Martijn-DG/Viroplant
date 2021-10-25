rm(list=ls())

library(tidyverse)
library(gmodels)

#load data
data <- read.csv(file = '//clo.be/home/home_lm/mdegussem/Desktop/residue.csv', sep = ";", dec = ",")

data$Country <- as.factor(data$Country)
levels(data$Country)
data$Country <- relevel(data$Country, ref=6)

#data <- data[data$Country == "BE" | data$Country == "IT", ] #IT is gemiddeld strenger -> grotendeels door meer retailers waar eisen zijn 

data <- data[data$Country == "BE" | data$Country == "IT"| data$Country == "DE", ] #BE minder vaak extra eisen, IT extra eisen gemiddeld strenger 
data$Country <- relevel(data$Country, ref=4)

data$X.MRL.simplified <- as.numeric(sub(",", ".", data$X.MRL.simplified, fixed = TRUE))

mean(data$X.MRL.simplified, na.rm = T)
mod <- lm(X.MRL.simplified ~ Country, data)
summary(mod)
aggregate(data$X.MRL.simplified, by = list(data$Country), FUN = mean, na.rm=TRUE) #BE en DK hogere % dan de rest

data$X.MRL.simplified2 <- data$X.MRL.simplified
data$X.MRL.simplified2[data$X.MRL.simplified2==100] <- NA
mean(data$X.MRL.simplified2, na.rm = T)
mod <- lm(X.MRL.simplified2 ~ Country, data)
summary(mod)
aggregate(data$X.MRL.simplified2, by = list(data$Country), FUN = mean, na.rm=TRUE) #van de retailers met een extralegale limiet zijn DE en NL het minst streng

data$X.MRL.simplified3 <- data$X.MRL.simplified2
data$X.MRL.simplified3[!is.na(data$X.MRL.simplified3)] <- T
data$X.MRL.simplified3[is.na(data$X.MRL.simplified3)] <- F
Cross.Table <- CrossTable(data$Country, data$X.MRL.simplified3, chisq=T, prop.c=F, prop.t=F, prop.chisq=F) 
aggregate(data$X.MRL.simplified3, by = list(data$Country), FUN = mean, na.rm=TRUE) #BE en FR hebben minder vaak extralegale limieten
mod <- lm(X.MRL.simplified3 ~ Country, data)
summary(mod)
