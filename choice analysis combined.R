#analysis of choice experiment
rm(list=ls())
library(survival)
library(mlogit) #install.packages("https://cran.r-project.org/src/contrib/Archive/mlogit/mlogit_1.0-2.tar.gz", repos=NULL,type="source")
library(dfidx)
library(gmnl) 
source('C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/lc_helpers.R')
library(ggplot2)
library(ggeffects)
library(sciplot)

table1 <- read.csv("C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/choice1 long.csv", sep = ";", dec = ",")
table3a <- read.csv("C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/FD/IT/choice3.csv", sep = ";", dec = ",")
table3a$country <- "IT"
table3b <- read.csv("C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/choice3.csv", sep = ";", dec = ",")
table3b$country <- "BE"
table3 <- rbind(table3a, table3b)
table3$Respondent <- row.names(table3)
table3 <- na.omit(table3)

data1 <- data.frame(matrix(ncol = 10, nrow = nrow(table3)*6*2))
colnames(data1) <- c("ID",  "Response", "Effectiveness", "Health", "Residue", "EOU", "IndividualX", "QuestionJ", "OptionI", "Country")
countr = 0
for (x in 1:nrow(table3)) { 
  for (j in 1:6) {
    for (i in 1:2) {
      countr = countr+1
      data1$Country[countr] <- table3$country[x]
      data1$ID[countr] <- paste(table3$Respondent[x], "_", j, sep = "")
      data1$Response[countr] <- as.numeric(table3[x, j+1] == i)
      for (n in 3:6) {
        twaalf <- ((j-1)*2)+i
        data1[countr, n] <- table1[twaalf, n]
      }
      data1$IndividualX[countr] <- x
      data1$QuestionJ[countr] <- j
      data1$OptionI[countr] <- i
    }
  }
}
data1$Effectiveness <- data1$Effectiveness/100

str(data1)
colMeans(data1[ ,2:6])
sapply(data1, function(x) sum(is.na(x)))

model1 <- clogit(Response~Effectiveness+Health+Residue+EOU+strata(ID), data=data1)
summary(model1)

model2 <- clogit(Response~Effectiveness*Country+Health*Country+Residue*Country+EOU*Country+strata(ID), data=data1)
summary(model2)

model2 <- clogit(Response~Effectiveness+Health+Residue*Country+EOU+strata(ID), data=data1)
summary(model2)

fancy_plot_m <- function(Model, Predictors) {
  test <- ggpredict(Model,Predictors)
  test$x <- jitter(test$x, 0.1)
  if (length(Predictors)==3) {
    renamed <- sapply(test$facet, function(x) paste(Predictors[3], test$facet))
    test$facet <- renamed[ ,1]
  }
  if (length(Predictors)==4) {
    renamed <- sapply(test$facet, function(x) paste(Predictors[3], test$facet))
    test$facet <- renamed[ ,1]
    renamed <- sapply(test$panel, function(x) paste(Predictors[4], test$panel))
    test$panel <- renamed[ ,1]
  }
  output <- ggplot(data = test, aes(x=x, y=predicted, ymin=conf.low, ymax=conf.high, colour = factor(group))) + 
    geom_errorbar(width = 0.05) + geom_pointrange() + geom_line() +
    labs(title ="", x = Predictors[1], y = "Estimated value", colour = Predictors[2]) #+ 
  #scale_color_manual(values=c("#F8766D", "#284C7A"))
  if (length(Predictors)==3) {
    output <- output + facet_wrap( ~ facet, nrow = 1)
  }
  if (length(Predictors)==4) {
    output <- output + facet_wrap(panel ~ facet, nrow = 2) 
  }
  return(output)
}

fancy_plot_m(model1, c("Effectiveness"))
fancy_plot_m(model1, c("Health"))
fancy_plot_m(model1, c("Residue"))
fancy_plot_m(model1, c("EOU"))
#
fancy_plot_m(model1, c("EOU", "Effectiveness"))
fancy_plot_m(model1, c("Health", "Effectiveness"))
fancy_plot_m(model1, c("Effectiveness", "Health"))
fancy_plot_m(model1, c("EOU", "Health"))
#
fancy_plot_m(model1, c("EOU","Health","Effectiveness"))
fancy_plot_m(model1, c("Effectiveness","Health","EOU"))
#
fancy_plot_m(model1, c("EOU","Health","Effectiveness","Residue"))
#
fancy_plot_m(model2, c("Residue", "Country"))

fit = glm(Response~Effectiveness, data=data1, family=binomial(link = "logit"))
summary(fit)
newdat <- data.frame(Effectiveness=seq(min(data1$Effectiveness), max(data1$Effectiveness), len=100))
newdat$Response = predict(fit, newdata=newdat, type="response")
plot(jitter(Response, 0.1) ~ jitter(Effectiveness, 0.1), data=data1, col="red4")
lines(Response~Effectiveness, data=newdat, col="green4", lwd=2)

#https://rpubs.com/msarrias1986/335556
data5 <- mlogit.data(data1, choice = "Response", shape = "long", chid.var = "ID", alt.var = "OptionI", id.var = "IndividualX", group.var = "QuestionJ" , varying = 3:6)
data5$Response <- 1-as.numeric(data5$Response)
head(data5)
mean(as.numeric(data5$Response)) 
sapply(data5, function(x) sum(is.na(x)))
model5 <- gmnl(Response~Effectiveness+Health+EOU| 0 | 0 | 0 | 1, data = data5, model = 'lc', Q = 2, method = "bfgs") 
summary(model5)
plot_ci_lc(model5)
shares(model5)

lineplot.CI(x.factor=Effectiveness, group=EOU, response=Response, data=data5, ci.fun=function(x) c(mean(x)-1.96*se(x), mean(x)+1.96*se(x)))
lineplot.CI(x.factor=Effectiveness, group=Health, response=Response, data=data5, ci.fun=function(x) c(mean(x)-1.96*se(x), mean(x)+1.96*se(x)))

#data4 <- dfidx(data1, idx = "ID",choice = "Response")
#head(data4)
model4 <- mlogit(Response~Effectiveness+Health+Residue+EOU, data=data5)
summary(model4)
sjPlot::plot_model(model4, type = "est") #type = c("est", "eff", "resid")
model4.1 <- mlogit(Response~Effectiveness+Health, data=data5)
summary(model4.1)
sjPlot::plot_model(model4.1, type = "est") #type = c("est", "eff", "resid")

