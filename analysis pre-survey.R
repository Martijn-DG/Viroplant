#exploratory analysis of short survey
library(plyr)
library(tidyverse)
rm(list=ls())


#import raw data from qualtrics as numeric .csv in the new format (3 header rows) and choose the option "Remove line breaks"
#input <- qualtRics::read_survey("C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/Rhizogenic+Agrobacterium+-+Short+-+Attribute+identification+-+VIROPLANT_September+25,+2020_02.34.csv")
input <- qualtRics::read_survey("C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/Rhizogenic+Agrobacterium+-+Short+-+Attribute+identification+-+VIROPLANT_April+22,+2021_02.34.csv")
data <- input

#removing incomplete responses
data <- data[data$Progress == 100, ]
data <- data[data$Q1 != 4, ] #not his main crop so questionnaire aborted

#first responder (25/06) has no HR, is only one that takes no measures against it and harvests only 6 ton per ha

#removing irrelevant columns
data <- data[ ,c(6, 8, 17:93)]

#adding ID
data$ID <- c(1:nrow(data))

#duration
data$`Duration (in seconds)` <- data$`Duration (in seconds)`/60
colnames(data)[1] <- "Duration (in minutes)"
meantMinutes <- mean(data$`Duration (in minutes)`[data$`Duration (in minutes)`!= 8963.150000])

#HR infection
hr <- data[data$Q8 == 1, ]
nohr <- data[data$Q8 == 2, ]

measureEffects <- data[ ,c(23, 24, 27, 29, 31, 32, 19:22)]

#yield loss
loss <- data[!is.na(data$Q9), ]
loss <- loss[!is.na(loss$Q10), ]
losses <- as.data.frame(loss$Q9)
losses$Q10 <- loss$Q10
losses$diff <- (((loss$Q9 - loss$Q10)/loss$Q9)*100)
losses$Q11 <- loss$Q11_1
losses$Q17 <- ((loss$Q17/loss$Q9)*100)
colnames(losses) <- c("unafected yield per ha", "affected yield per ha","yield loss (calculated%)", "yield loss (direct%)", "yield gained with measures (direct%)")
losses <- as.matrix(losses)
#write.csv(losses, "C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/losses1.csv")

#costs
costPha1 <- mean(data$Q16, na.rm=TRUE)#cost per ha
gainPha1 <- mean(data$Q17, na.rm=TRUE) #gained ton per ha
costPton1 <- costPha1/gainPha1
#costs excluding 0 gainers
costPha2 <- mean(data$Q16[data$Q17!=0], na.rm=TRUE)#cost per ha
gainPha2 <- mean(data$Q17[data$Q17!=0], na.rm=TRUE) #gained ton per ha
costPton2 <- costPha2/gainPha2
#cost per ton
#farmer 4(in data)/3(in worth) states highest cost(5000) for 0 gain
worth <- data[!is.na(data$Q16), ]
worth <- worth[!is.na(worth$Q17), ]
costs <- worth$Q16 / worth$Q17 #costs per ton gained
costs <- costs[costs != Inf]
costPton3 <- mean(costs[costs != Inf])
#matrix
costmat <- rbind(c(costPha1,gainPha1,costPton1),c(costPha2,gainPha2,costPton2),c(NA, NA, costPton3))
rownames(costmat) <- c("raw","excluding 0 gain","pairwise")
colnames(costmat) <- c("costPha", "gainPha", "costPton")
rm(costPha1, gainPha1, costPton1, costPha2, gainPha2, costPton2, costPton3)
#write.csv(costmat, "C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/costmat.csv")

#loss and worth contain the same 3 people
#both on 06-29 + one on 07-05 (durations 2041 966 537789)
#worth 2 3 4 = loss 1 2 3
z <- as.data.frame(losses[1:3, ])
z$costsPton <- costs[2:4]
losses <- as.matrix(z)
rm(z)
#write.csv(losses, "C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/losses2.csv")

#ease of use characteristics
Q19 <- data[ ,c(80,41:47)]
colnames(Q19) <- c("ID", "applecation.mode", "availability", "shelf.life", "storage", "spec.instructions", "equipement", "frequency")

Q19n <- Q19 #for all combined

Q19 <- gather(Q19, key = "characteristic", value = "importance", applecation.mode, availability, shelf.life, storage, spec.instructions, equipement, frequency)
Q19 <- rstatix::convert_as_factor(Q19, ID, characteristic)

pwc19 <- rstatix::pairwise_t_test(Q19, importance ~ characteristic, paired = TRUE, p.adjust.method = "bonferroni")
pwc19

box19 <- ggplot(Q19, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + xlab("characteristic") +
  ggpubr::stat_pvalue_manual(pwc19[pwc19$p.adj.signif != "ns", ], y.position = c(5.5, 6, 6.5, 7))
box19

#effectiveness
Q20 <- data[ ,c(80,48:50)]
colnames(Q20) <- c("ID", "reliability", "action.time", "effectiveness")

Q20n <- Q20[, 2:ncol(Q20)]

Q20 <- gather(Q20, key = "characteristic", value = "importance", reliability, action.time, effectiveness)
Q20 <- rstatix::convert_as_factor(Q20, ID, characteristic)

pwc20 <- rstatix::pairwise_t_test(Q20, importance ~ characteristic, paired = TRUE, p.adjust.method = "bonferroni")
pwc20

box20 <- ggplot(Q20, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + xlab("characteristic") +
  ggpubr::stat_pvalue_manual(pwc20[pwc20$p.adj.signif != "ns", ], y.position = c(5.5, 6))
box20

#rules
Q21 <- data[ ,c(80,51:53)]
colnames(Q21) <- c("ID", "government", "preharvest.applicationT", "buyers")

Q21n <- Q21[, 2:ncol(Q21)]

Q21 <- gather(Q21, key = "characteristic", value = "importance", government, preharvest.applicationT, buyers)
Q21 <- rstatix::convert_as_factor(Q21, ID, characteristic)

pwc21 <- rstatix::pairwise_t_test(Q21, importance ~ characteristic, paired = TRUE, p.adjust.method = "bonferroni")
pwc21

box21 <- ggplot(Q21, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + xlab("characteristic") +
  ggpubr::stat_pvalue_manual(pwc21[pwc21$p < 0.1, ], y.position = c(5.5), label = "p = {p}, p-adj = 0.246")
box21

#ethics
Q22 <- data[ ,c(80,54:61)]
colnames(Q22) <- c("ID", "organic", "non.gmo", "selectivity", "containment", "residues", "controversial", "employee.health", "consumer.health")

Q22n <- Q22[, 2:ncol(Q22)]

Q22 <- gather(Q22, key = "characteristic", value = "importance", organic, non.gmo, selectivity, containment, residues, controversial, employee.health, consumer.health)
Q22 <- rstatix::convert_as_factor(Q22, ID, characteristic)

pwc22 <- rstatix::pairwise_t_test(Q22, importance ~ characteristic, paired = TRUE, p.adjust.method = "bonferroni")
pwc22[pwc22$p.adj.signif != "ns", ]

box22 <- ggplot(Q22, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + xlab("characteristic") +
  ggpubr::stat_pvalue_manual(pwc22[pwc22$p.adj.signif != "ns", ], y.position = seq(5.5, 5+(nrow(pwc22[pwc22$p.adj.signif != "ns", ])*0.5), by=0.5))
box22

#combined
Qall <- as.data.frame(cbind(Q19n, Q20n, Q21n, Q22n))
colnames(Qall) <- c("ID", "applecation.mode", "availability", "shelf.life", "storage", "spec.instructions", "equipement", "frequency"
                    , "reliability", "action.time", "effectiveness20", "government", "preharvest.applicationT", "buyers"
                    , "organic", "non.gmo", "selectivity", "containment", "residues", "controversial", "employee.health", "consumer.health")

Qall <- gather(Qall, key = "characteristic", value = "importance", applecation.mode, availability, shelf.life, storage, spec.instructions, equipement, frequency
               , reliability, action.time, effectiveness20, government, preharvest.applicationT, buyers
               , organic, non.gmo, selectivity, containment, residues, controversial, employee.health, consumer.health)
Qall <- rstatix::convert_as_factor(Qall, ID, characteristic)

pwcall <- rstatix::pairwise_t_test(Qall, importance ~ characteristic, paired = TRUE, p.adjust.method = "bonferroni")
print(pwcall[pwcall$p.adj.signif != "ns", ], n = 22)
print(pwcall[pwcall$p.adj < 0.01, ])

boxall <- ggplot(Qall, aes(x = reorder(characteristic, c(Q19$importance, 10*Q20$importance, 200*Q21$importance, 3000*Q22$importance))
                           , y = importance)) + geom_boxplot() + xlab("characteristic") + theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) 
boxall

boxall1 <- ggplot(Qall, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + 
  xlab("characteristic") + theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) +
  ggpubr::stat_pvalue_manual(pwcall[pwcall$p.adj < 0.01, ], y.position = seq(5.5, 5+(nrow(pwcall[pwcall$p.adj < 0.01, ])*0.5), by=0.5))
boxall1 

boxall5 <- ggplot(Qall, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + 
  xlab("characteristic") + theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5)) +
  ggpubr::stat_pvalue_manual(pwcall[pwcall$p.adj < 0.05 & pwcall$p.adj > 0.01, ], y.position = seq(5.5, 5+(nrow(pwcall[pwcall$p.adj < 0.05 & pwcall$p.adj > 0.01, ])*0.5), by=0.5))
boxall5

boxalls <- ggplot(Qall, aes(x = reorder(characteristic, importance), y = importance)) + geom_boxplot() + 
  xlab("characteristic") + theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))
boxalls #selection based on importance: reliability, effectiveness, 3x residue, 2x health (related to residue), ?eou-application mode 

aggregate(importance ~ characteristic, data = Qall, FUN = mean)

#overall
Q18 <- 9-data[ ,c(80,33:40)]
colnames(Q18) <- c("ID", "price", "effectiveness", "eou", "environment", "health", "ethics", "time", "compatibility")

Q18n <- Q18[, 2:ncol(Q18)]

Q18 <- gather(Q18, key = "attribute", value = "importance_rank", price, effectiveness, eou, environment, health, ethics, time, compatibility)
Q18 <- rstatix::convert_as_factor(Q18, ID, attribute)

pwc18 <- rstatix::pairwise_t_test(Q18, importance_rank ~ attribute, paired = TRUE, p.adjust.method = "bonferroni")
pwc18[pwc18$p.adj.signif != "ns", ]

box18 <- ggplot(Q18, aes(x = reorder(attribute, importance_rank), y = importance_rank)) + geom_boxplot() + xlab("attribute") +
  ggpubr::stat_pvalue_manual(pwc18[pwc18$p.adj.signif != "ns", ], y.position = seq(8.5, 8+(nrow(pwc18[pwc18$p.adj.signif != "ns", ])*0.5), by=0.5))
box18

#willingness to use
Q24 <- data[ ,c(80,74:78)]
colnames(Q24) <- c("ID", "biocontrol", "virusbased", "phagebased", "agrochem", "gmo")

Q24n <- Q24[, 2:ncol(Q24)]

Q24 <- gather(Q24, key = "statement", value = "agreement", biocontrol, virusbased, phagebased, agrochem, gmo)
Q24 <- rstatix::convert_as_factor(Q24, ID, statement)

pwc24 <- rstatix::pairwise_t_test(Q24, agreement ~ statement, paired = TRUE, p.adjust.method = "bonferroni")
pwc24

box24 <- ggplot(Q24, aes(x = reorder(statement, agreement), y = agreement)) + geom_boxplot() + xlab("") + ylab("willingness to use") +
  ggpubr::stat_pvalue_manual(pwc24[pwc24$p.adj.signif != "ns", ], y.position = seq(7.5, 7+(nrow(pwc24[pwc24$p.adj.signif != "ns", ])*0.5), by=0.5))
box24



#correlations
Qcor <- as.data.frame(cbind(Q18n, Q19n[, 2:ncol(Q19n)], Q20n, Q21n, Q22n, Q24n))
correlations <- Hmisc::rcorr(as.matrix(Qcor[, 2:ncol(Qcor)]), type="spearman")
corrplot::corrplot(correlations$r, method = "circle", type = "upper", tl.srt=60, tl.cex = 0.7)

#question clustering (based on correlations)
clustering1 <- hclust(factoextra::get_dist(t(Qcor), method = "spearman"), method="ward.D2") 
plot(clustering1)
plot(length(clustering1$height):1, clustering1$height, type = "b")
corrplot::corrplot(correlations$r, method = "circle", tl.srt=60, tl.cex = 0.7, order="hclust", hclust.method="ward.D2", addrect = 7, p.mat = correlations$P, sig.level = 0.05, insig = "blank")


#farmer clustering
clustering3 <- hclust(dist(Qcor, method = "manhattan"), method="ward.D2")
plot(clustering3)
plot(length(clustering3$height):1, clustering3$height, type = "b")



#pca
pcDataQ18 <- as.data.frame(scale(9-data[ ,c(33:40)]))
pcDataOthers <- as.data.frame(scale(data[ ,c(41:61, 74:78)]))
pcDataAll <- as.data.frame(scale(cbind(pcDataQ18, pcDataOthers)))
pcDataSig <- as.data.frame(scale(data[ ,c(48, 50, 52:53, 60, 58, 61, 51 , 41)])) 

pcDataList <- list(pcDataQ18, pcDataOthers,pcDataAll, pcDataSig) 
pcDataNames <- c("pc18", "pcOthers","pcAll","pcSig")
pcPlotList <- list()
countr = 1
for (x in pcDataList) {
  pcData <- x
  
  #qnames <- c()
  #for (q in colnames(pcData)) {
  #  qnames <- c(qnames, attr(data[[q]], "label"))
  #}
  #colnames(pcData) <- qnames
  
  pca <- prcomp(pcData)
  write.csv(pca$rotation, paste("C:/Users/mdegussem/OneDrive - ILVO/D5/short Q/", pcDataNames[countr], '.csv', sep = ""))
  pca.sum <- summary(pca)
  print(pca.sum)
  pca.var <- as.data.frame(pca.sum$importance[2, ])
  pca.var$component <- factor(rownames(pca.var),levels = rownames(pca.var))
  colnames(pca.var) <- c("variance.explained", "component")
  ggplot(pca.var) + geom_col(aes(y = variance.explained, x = component))
  
  ctrL <- countr + (countr-1) 
  print(ctrL)
  pcPlotList[[ctrL]] <- ggbiplot::ggbiplot(pca)                #1 3 5 7
  ctrL <- ctrL + 1
  print(ctrL)
  pcPlotList[[ctrL]] <- ggbiplot::ggbiplot(pca,choices=c(3,4)) #2 4 6 8
  
  countr = countr+1
}

source("C:/Users/mdegussem/OneDrive - ILVO/multiplot.R") #http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
multiplot(pcPlotList[[1]], pcPlotList[[2]], cols=2) #Q18
multiplot(pcPlotList[[3]], pcPlotList[[4]], cols=2) #others
multiplot(pcPlotList[[5]], pcPlotList[[6]], cols=2) #together
multiplot(pcPlotList[[7]], pcPlotList[[8]], cols=2) #sig

pcDataSig
correlationsS <- Hmisc::rcorr(as.matrix(pcDataSig), type="spearman")
correlationsS$r
corrplot::corrplot(correlationsS$r, method = "circle", type = "upper", tl.srt=60, tl.cex = 0.7)

#cfa
library(lavaan)

model11 <- 'PU =~ Q20_1 + Q20_3     
          PEU =~ Q19_1 + Q19_5
          soc =~ Q22_8 + Q22_7
          cont =~ Q21_3 + Q22_5 + Q21_2 + Q21_1 ' 
fit11 <- cfa(model11, data=pcDataAll)
summary(fit11, fit.measures=T)

model12 <- 'PU =~ Q20_1 + Q20_3     
          PEU =~ Q19_1 + Q19_5 
          soc =~ Q22_8 + Q22_7
          contB =~ Q21_3 + Q22_5  
          contG =~ Q21_2 + Q21_1 ' 
fit12 <- cfa(model12, data=pcDataAll)
summary(fit12, fit.measures=T)



model1 <- 'PU =~ Q20_1 + Q20_3     
          PEU =~ Q19_1 + Q18_8
          soc =~ Q22_8 + Q22_7 + Q22_5
          cont =~ Q21_3 + Q22_5 + Q21_2 ' 
#PU = reliability + effectiveness #PEU = app mode + compatibility 
#soc = health c + health e + residue #cont = reg b + residue + app time
fit1 <- cfa(model1, data=pcDataAll)
summary(fit1, fit.measures=T)


model2 <- 'PU =~ Q20_1 + Q20_3     
          PEU =~ Q19_1
          soc =~ Q22_8 + Q22_7
          cont =~ Q21_3 + Q22_5 + Q21_2 ' 
#PU = reliability + effectiveness #PEU = application mode 
#soc = health c + health e #cont = reg b + residue + app time
fit2 <- cfa(model2, data=pcDataAll)
summary(fit2, fit.measures=T)

model3 <- 'PU =~ Q20_1 + Q20_3     
          soc =~ Q22_8 + Q22_7
          cont =~ Q21_3 + Q22_5 + Q21_2 ' 
#PU = reliability + effectiveness 
#soc = health c + health e #cont = reg b + residue + app time
fit3 <- cfa(model3, data=pcDataAll)
summary(fit3, fit.measures=T)

model4 <- 'PU =~ Q20_1 + Q20_3     
          soc =~ Q22_8 + Q22_7
          cont =~ Q21_3 + Q22_5 ' 
#PU = reliability + effectiveness 
#soc = health c + health e #cont = reg b + residue
fit4 <- cfa(model4, data=pcDataAll)
summary(fit4, fit.measures=T)


#regression
pcDataAll <- rbind(pcDataAll, pcDataAll, pcDataAll)
pcDataAll <- data.frame(lapply(pcDataAll, jitter))

modelv <- '
  # measurement model
    adoptionv =~ Q24_2 
    eff =~ Q20_1 + Q20_3
    health =~ Q22_8 + Q22_7
    regres=~ Q21_3 + Q22_5 + Q21_2 + Q21_1
    
  # regressions
    adoptionv ~ eff + health + regres
    
  # residual correlations
 
'
fitv <- sem(modelv, data=pcDataAll)
summary(fitv, standardized=TRUE)

modelg <- '
  # measurement model
    adoptiong =~ Q24_3
    eff =~ Q20_1 + Q20_3
    health =~ Q22_8 + Q22_7
    regres=~ Q21_3 + Q22_5 + Q21_2 + Q21_1
    
  # regressions
    adoptiong ~ eff + health + regres
    
  # residual correlations
 
'
fitg <- sem(modelg, data=pcDataAll)
summary(fitg, standardized=TRUE)
