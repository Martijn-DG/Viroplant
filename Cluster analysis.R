#clustering of farm economic data
rm(list=ls())
library(rlang)
library(plyr)
library(tidyverse)
library(distances)
library(dendextend)

#import raw data
input <- read.csv("W:/MODELLEN/DATA-MODELLEN_beperkt/LMN_AMS/2020_OD4OS/OD4OS10020_GLASGROENTEN/Verwerking data_cluster opdeling/Clustervariabelen_LB_rekeningen.csv", sep = ";", dec = ",")

input2 <- read.csv("W:/MODELLEN/DATA-MODELLEN_beperkt/LMN_AMS/2020_OD4OS/OD4OS10020_GLASGROENTEN/Verwerking data_cluster opdeling/Clustervariabelen_aanvulling15122020_LB.csv", sep = ";", dec = ",")
input2 <- input2[!(input2$Dossier %in% input$Dossier), ]
input <- rbind(input, input2)
rm(input2)

input <- subset(input, select = -c(Dossier))
input$ID <- c(1:nrow(input))

input_backup <- input



##########################reset point so you don't have to keep the vpn on###########################


rm(list=setdiff(ls(), "input_backup"))
input <- input_backup
par(mar=c(6,6,6,6)+2) #set margins for plots

#manual category
inputA <- read.csv("C:/Users/mdegussem/OneDrive - ILVO/D5/clusters/manual clusters/manual cluster list.csv", sep = ";", dec = ",")
inputB <- input[ ,c(2:5, 7:8)]

inputB$Cluster.manueel <- inputB$Activiteitsoort_omschrijving
for (i in 1:nrow(inputA)) {
  inputB$Cluster.manueel[inputB$Cluster.manueel == inputA[i,1]] <- inputA[i,2]
  #print(i)
  #print(inputA[i,1]) #activiteit
  #print(inputA[i,2]) #manuele cluster
}

inputB[sapply(inputB, is.character)] <- lapply(inputB[sapply(inputB, is.character)], as.factor)
str(inputB)
levels(inputB$Rekening_sector) #benadering manuele clusters
levels(inputB$Teeltsubstraat) #volle grond of substraat
levels(inputB$Gawasteeltmilieu) #openlucht of niet
levels(inputB$Cluster.manueel) #manuele clusters

#manual group naming
inputB$Cluster.manueel_2 <- inputB$Cluster.manueel
pre <- table(inputB$Cluster.manueel_2)
inputB$Cluster.manueel_2 <- as.character(inputB$Cluster.manueel_2)

for (i in 1:nrow(inputB)) {
  if (is.na(inputB$Rekening_sector[i])) {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- x
  }
  else if (inputB$Rekening_sector[i]=="ENERGIEGEWAS") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "nijverheid" of "granen"
  }
  else if (inputB$Rekening_sector[i]=="FRUIT") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "pitfruit" of "klein fruit" of "aardbij"
  }
  else if (inputB$Rekening_sector[i]=="GLASGROENTEN") {
    if (is.na(inputB$Teeltsubstraat[i])) {
      inputB$Cluster.manueel_2[i] <- "volle grond onder glas"
    } 
    else if (inputB$Teeltsubstraat[i]=="Volle grond") {
      inputB$Cluster.manueel_2[i] <- "volle grond onder glas"
    } 
    else if (inputB$Teeltsubstraat[i]!="Volle grond") {
      inputB$Cluster.manueel_2[i] <- "substraatteelt"
    }
  }
  else if (inputB$Rekening_sector[i]=="GLASSIERTEELT") {
    inputB$Cluster.manueel_2[i] <- "sierteelt glas"
  }
  else if (inputB$Rekening_sector[i]=="GROENBEMESTERS") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "x"
  }
  else if (inputB$Rekening_sector[i]=="GROENTEN") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "x"
  }
  else if (inputB$Rekening_sector[i]=="MARKTBAAR GEWAS") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "nijverheid" of "granen"
  }
  else if (inputB$Rekening_sector[i]=="PLANT- EN ZAAIGOED INTENSIEVE TEELTEN") {
    inputB$Cluster.manueel_2[i] <- "x"
  }
  else if (inputB$Rekening_sector[i]=="SIERTEELT") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "sierteelt"
  }
  else if (inputB$Rekening_sector[i]=="VOEDERGEWASSEN") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "nijverheid" of "granen"
  }
  else if (inputB$Rekening_sector[i]=="VOLLEVELDSGROENTEN INDUSTRIE") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "groenten openl"
  }
  else if (inputB$Rekening_sector[i]=="VOLLEVELDSGROENTEN VERSMARKT") {
    #inputB$Cluster.manueel_2[i] <- inputB$Cluster.manueel[i] # <- "groenten openl"
  }
  if (inputB$Gawasteeltmilieu[i]=="OPENL") {
    if (inputB$Cluster.manueel_2[i]=="substraatteelt") {
      inputB$Cluster.manueel_2[i] <- "groenten openl"
    }
    else if (inputB$Cluster.manueel_2[i]=="volle grond onder glas") {
      inputB$Cluster.manueel_2[i] <- "groenten openl"
    }
  }
  if (inputB$Cluster.manueel[i]=="aardbei") {
    if (inputB$Gawasteeltmilieu[i]=="SERRE") {
      inputB$Cluster.manueel_2[i] <- "aardbei glas"
    }
    if (inputB$Gawasteeltmilieu[i]!="SERRE") {
      inputB$Cluster.manueel_2[i] <- "aardbei openl"
    }
  } 
  if (inputB$Cluster.manueel[i]=="sierteelt") {
    if (inputB$Gawasteeltmilieu[i]=="SERRE") {
      inputB$Cluster.manueel_2[i] <- "sierteelt glas"
    }
    else if (inputB$Gawasteeltmilieu[i]!="SERRE") {
      inputB$Cluster.manueel_2[i] <- "sierteelt openl"
    }
  }
  if (inputB$Cluster.manueel[i]=="bomen") {
    inputB$Cluster.manueel_2[i] <- "bomen"
  }
  # if (inputB$Cluster.manueel[i]=="klein fruit") {
  #   #inputB$Cluster.manueel_2[i] <- "klein fruit"
  #   if (inputB$Gawasteeltmilieu[i]=="OPENL") {
  #     inputB$Cluster.manueel_2[i] <- "klein fruit openl"
  #   }
  #   else if (is.na(inputB$Teeltsubstraat[i])) {
  #     inputB$Cluster.manueel_2[i] <- "klein fruit vol glas"
  #   }
  #   else if (inputB$Teeltsubstraat[i]=="Volle grond") {
  #     inputB$Cluster.manueel_2[i] <- "klein fruit vol glas"
  #   }
  #   else if (inputB$Teeltsubstraat[i]!="Volle grond") {
  #     inputB$Cluster.manueel_2[i] <- "klein fruit substraat"
  #   }
  # }
  if (inputB$Cluster.manueel_2[i]=="substraatteelt")  {
    inputB$Cluster.manueel_2[i] <- "groenten substraat"
  }
  if (inputB$Cluster.manueel_2[i]=="volle grond onder glas")  {
    inputB$Cluster.manueel_2[i] <- "groenten vol glas"
  }
  if (inputB$Cluster.manueel_2[i]=="open lucht groenten")  {
    inputB$Cluster.manueel_2[i] <- "groenten openl"
  }
}

str(inputB)
inputB[sapply(inputB, is.character)] <- lapply(inputB[sapply(inputB, is.character)], as.factor)
str(inputB)

pre
post <- table(inputB$Cluster.manueel_2)
post
sort(post)

data <- input[ ,-c(2, 4:5)] #Gewascategorie, Rekening_omschrijving, Rekening_sector

#factorizing
data$Gawasteeltmilieu <- factor(data$Gawasteeltmilieu)
input$Gawasteeltmilieu <- factor(input$Gawasteeltmilieu)
data$Teeltsubstraat <- factor(data$Teeltsubstraat)
input$Teeltsubstraat <- factor(input$Teeltsubstraat)

#adding cluster name
data$cluster <- inputB$Cluster.manueel_2
data$cluster <- factor(data$cluster) 
data$Teeltsubstraat <- factor(data$Teeltsubstraat)
data$Gawasteeltmilieu <- factor(data$Gawasteeltmilieu)

data <- data[-c(196), ] #removing "1816 steenwol SERRE" (big outlier e.g. clustering2 - 2018 for lb_rekeningen)
data <- data[data$ID!=302, ] #removing "302 kruiden" (big outlier e.g. pcaplot 1)
data <- data[data$ID!=1815, ] #removing "1814-5-7 groenten substraat" (outliers in check)
data <- data[data$ID!=1814, ]
data <- data[data$ID!=1817, ]

#merge grain+production
data$Hoev_kg_Plantaardige.producten[is.na(data$Hoev_kg_Plantaardige.producten)] <- data$Hoev_kg_Granen[is.na(data$Hoev_kg_Plantaardige.producten)] #8+26 
data$Plantaardige.productie..kg.ha.[is.na(data$Plantaardige.productie..kg.ha.)] <- data$Hoev.granen..kg.ha.[is.na(data$Plantaardige.productie..kg.ha.)] #30+27
data <- data[ ,-c(26:27)] #drop grains

#calculate price per kg
#euro per ha 27 en 28 kg per ha -> euro per kg
data$prijs.per.kg <- data$Verkoop.gewassen.per.ha/data$Plantaardige.productie..kg.ha.

#english labels
levels(data$cluster) <- c("Potato", "Strawberry GH", "Strawberry OF", "Ligneous flor", "Cereals", "Veg OF", "Veg substrate", "Veg GH", "Ligneous small fruit", "Nonfood", "Pome fruit", "Flor GH", "Flor OF", "x") ############################################
#GreenHouse OpenField hydroponics Floriculture

#calculate saldo per ha
#data$Bruto.saldo.per.ha <- data$X3000_Bruto.saldo/data$Standaardwaarde..Ha.
data$Bruto.saldo <- data$X1000_Totale.opbrengsten..euro. - data$X2000_Totale.variabele.kosten..euro.
data$Bruto.saldo.per.ha <- data$Bruto.saldo/data$Standaardwaarde..Ha.
#data$Bruto.saldo.per.ha <- data$Totale.opbrengst.per.ha - data$Totale.variabele.kost..euro.ha.

#split data set
dataPH <- data[ ,c(1:2, 4:5, 10, 12, 33, 16, 26, 29:30)] #bruto saldo 14, 32 ; bruto saldo per ha 33
#ook aanpassen:   dataPHextended <- data_backup[ ,c(1:5, 10, 12, 33, 16, 26, 31, 29:30)] 
data_backup <- data
data <- dataPH
sort(colSums(summarise_all(tibble::as_tibble(dataPH), is.na)))

#correlations
Qcor <- data[ ,-c(1:4, 10)] #boekjaar, Activiteitsoort_omschrijving, Teeltsubstraat, Gewasteeltmilieu, ID, cluster
Qcor <- t(Qcor)
colnames(Qcor) <- paste(data$ID, data$cluster, sep = "")
# colnames(Qcor) <- paste(data$ID, data$Activiteitsoort_omschrijving, sep = "")
row.names(Qcor) <- c("Var_cost", "Var_cost_ppp", "Gross_Margin", "Fixed_cost", "Revenue", "Cluster") #############################################################################

NaCheck <- apply(Qcor[1:5,], 2, function(x) all(is.na(x))) #removing full NA columns
if (sum(NaCheck) > 0) {
  NaCol <- names(NaCheck[NaCheck == TRUE])
  Qcor <- Qcor[ , -which(colnames(Qcor) %in% NaCol)]
}
print(paste(as.character(as.numeric(sum(NaCheck))), "full NA columns removed"))
NaCheck <- apply(Qcor[1:5,], 1, function(x) all(is.na(x))) #removing full NA rows
if (sum(NaCheck) > 0) {
  NaCol <- names(NaCheck[NaCheck == TRUE])
  Qcor <- Qcor[-which(rownames(Qcor) %in% NaCol), ]
}
print(paste(as.character(as.numeric(sum(NaCheck))), "full NA rows removed"))

#checking wether agregation is ok
checkQcor <- as.data.frame(t(Qcor))
checkQcor$Var_cost <- as.numeric(checkQcor$Var_cost)
checkQcor$Var_cost_ppp <- as.numeric(checkQcor$Var_cost_ppp)
checkQcor$Gross_Margin <- as.numeric(checkQcor$Gross_Margin)
checkQcor$Fixed_cost <- as.numeric(checkQcor$Fixed_cost)
checkQcor$Revenue <- as.numeric(checkQcor$Revenue)
checkQcor$Cluster <- as.factor(checkQcor$Cluster)

test <- lm(cbind(Var_cost, Var_cost_ppp, Gross_Margin, Fixed_cost, Revenue)~Cluster, data=checkQcor)
summary(test)
car::Anova(test)
summary(car::Anova(test))

#aggregating
QcorAg <- t(Qcor)
QcorAg <- as.data.frame(QcorAg)
QcorAg$Var_cost <- as.numeric(QcorAg$Var_cost)
QcorAg$Var_cost_ppp <- as.numeric(QcorAg$Var_cost_ppp)
QcorAg$Fixed_cost <- as.numeric(QcorAg$Fixed_cost)
QcorAg$Revenue <- as.numeric(QcorAg$Revenue)
QcorAg$Gross_Margin <- as.numeric(QcorAg$Gross_Margin)
QcorAg$Cluster <- as.factor(QcorAg$Cluster)
QcorAg <- na.omit(QcorAg)
QcorAgSD <- QcorAg
QcorAgHefboom <- QcorAg[,-c(7)]
QcorAg <- aggregate(QcorAg, by = list(QcorAg$Cluster), FUN = mean) #returns NA for "cluster" so will give 1 warning for each row (14)
QcorAg <- QcorAg[1:(nrow(QcorAg)-1),] #drop x roup
QcorAg

QcorAgSD <- aggregate(QcorAgSD[ ,1:ncol(QcorAgSD)-1], by = list(QcorAgSD$Cluster), FUN = sd)
QcorAgSD <- QcorAgSD[1:(nrow(QcorAgSD)-1),] #drop x roup
QcorAgSD

correlations <- Hmisc::rcorr(as.matrix(t(Qcor[1:5,])), type="spearman")
correlations2 <- Hmisc::rcorr(as.matrix(QcorAg[,2:6]), type="spearman")

corrplot::corrplot(correlations$r, method = 'circle', type = "upper", addCoef.col = 'gold', number.cex = 1.2 , tl.col = 'black')
corrplot::corrplot(correlations2$r, method = 'number', type = "upper")

corrplot::corrplot(correlations$r, type = "upper", method = 'number', col = "black", cl.pos = 'n', tl.col = 'black', number.cex = metan::resca(values = c(abs(correlations$r[upper.tri(correlations$r, diag=T)])), new_min = 0.7, new_max = 1.6) ) #c(abs(test[upper.tri(test, diag=T)])+0.6)
corrplot::corrplot(correlations2$r, type = "upper")

#discriptive plotting
barplot(Var_cost~Group.1, data = QcorAg,las=2)
barplot(Var_cost_ppp~Group.1, data = QcorAg,las=2)
barplot(Gross_Margin~Group.1, data = QcorAg,las=2)
barplot(Fixed_cost~Group.1, data = QcorAg,las=2)
barplot(Revenue~Group.1, data = QcorAg,las=2)

#check within group variance vs between group variance of manual groupings
check <- lm(Var_cost + Var_cost_ppp + Gross_Margin + Fixed_cost + Revenue ~ Cluster, data = QcorAgHefboom)
summary(check)

####k-means clustering
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- QcorAg[,1:6]
#df <- na.omit(df)
#df <- scale(df)

#df <- data.matrix(df, rownames.force = NA)
df <- as.data.frame(df)
row.names(df) <- df$Group.1
df <- df[,2:6]
df

distance <- get_dist(df)
#fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

k2 <- kmeans(df, centers = 2, nstart = 25)
k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)
p1 <- fviz_cluster(k2, data = df, repel=T) + ggplot2::ggtitle("k = 2")
p2 <- fviz_cluster(k3,  data = df, repel=T) + ggplot2::ggtitle("k = 3")
p3 <- fviz_cluster(k4,  data = df, repel=T) + ggplot2::ggtitle("k = 4")
p4 <- fviz_cluster(k5,  data = df, repel=T) + ggplot2::ggtitle("k = 5")
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

fviz_cluster(k3,  data = df, repel=T) + ggplot2::ggtitle("k = 3") #zie pca onderaan voor dims (assen)

k6 <- kmeans(df, centers = 6, nstart = 25)
fviz_cluster(k6,  data = df, repel=T) + ggplot2::ggtitle("k = 6")
k6$cluster

fviz_nbclust(df, kmeans, method = "wss", k.max = 9) #elbow method
fviz_nbclust(df, kmeans, method = "silhouette", k.max = 9) #silhouette method

#cluster splitting
dataPHextended <- data_backup[ ,c(1:5, 10, 12, 33, 16, 26, 31, 29:30)]
QcorBackup <- Qcor
Qcor <- dataPHextended[ ,-c(1:2, 4:5, 12)] #boekjaar, Activiteitsoort_omschrijving, Teeltsubstraat, Gewasteeltmilieu, ID, cluster
Qcor <- t(Qcor)
colnames(Qcor) <- paste(data$ID, data$cluster, sep = "")
row.names(Qcor) <- c("Oppervlakte", "Var_cost", "Var_cost_ppp", "Gross_Margin", "Fixed_cost", "Revenue", "Kg_prijs", "Cluster")

NaCheck <- apply(Qcor[1:5,], 2, function(x) all(is.na(x))) #removing full NA columns
if (sum(NaCheck) > 0) {
  NaCol <- names(NaCheck[NaCheck == TRUE])
  Qcor <- Qcor[ , -which(colnames(Qcor) %in% NaCol)]
}
print(paste(as.character(as.numeric(sum(NaCheck))), "full NA columns removed"))
NaCheck <- apply(Qcor[1:5,], 1, function(x) all(is.na(x))) #removing full NA rows
if (sum(NaCheck) > 0) {
  NaCol <- names(NaCheck[NaCheck == TRUE])
  Qcor <- Qcor[-which(rownames(Qcor) %in% NaCol), ]
}
print(paste(as.character(as.numeric(sum(NaCheck))), "full NA rows removed"))

Qcor[1:8, 1:4]
ClustQcor <- Qcor 
ClustQcor <- ClustQcor[, ClustQcor[8, ]!="x"]
clusterkey <- as.data.frame(k6$cluster) ############ k6$cluster or other number for clustering result, 
#  #skip this to use Qcor[8, ] for labeling (manual clusters)
# for (i in 1:ncol(ClustQcor)) {
#   for (k in 1:nrow(clusterkey)) {
#     if (ClustQcor[8,i]==row.names(clusterkey)[k]) {
#       ClustQcor[8,i] <- clusterkey[k,1]
#     }
#   }
# }
# QcorBackup <- Qcor

#check within group variance vs between group variance of manual groupings
checkData <- t(ClustQcor)
checkData <- as.data.frame(checkData[ ,-c(1, 7)])
checkData$Var_cost <- as.numeric(checkData$Var_cost)
checkData$Var_cost_ppp <- as.numeric(checkData$Var_cost_ppp)
checkData$Fixed_cost <- as.numeric(checkData$Fixed_cost)
checkData$Revenue <- as.numeric(checkData$Revenue)
checkData$Gross_Margin <- as.numeric(checkData$Gross_Margin)
checkData$Cluster <- as.factor(checkData$Cluster)
check <- lm(Var_cost + Var_cost_ppp + Gross_Margin + Fixed_cost + Revenue ~ Cluster, data = checkData)
summary(check)

#make separate data set per cluster
levels(checkData$Cluster)

ClustQcor1 <- ClustQcor[, ClustQcor[8, ]=="1"]
ClustQcor2 <- ClustQcor[, ClustQcor[8, ]=="2"]
ClustQcor3 <- ClustQcor[, ClustQcor[8, ]=="3"]
ClustQcor4 <- ClustQcor[, ClustQcor[8, ]=="4"]
ClustQcor5 <- ClustQcor[, ClustQcor[8, ]=="5"]
ClustQcor6 <- ClustQcor[, ClustQcor[8, ]=="6"]

ClustQcor1 <- ClustQcor[, ClustQcor[8, ]=="Cereals"]
ClustQcor2 <- ClustQcor[, ClustQcor[8, ]=="Flor GH"]
ClustQcor3 <- ClustQcor[, ClustQcor[8, ]=="Flor OF"]
ClustQcor4 <- ClustQcor[, ClustQcor[8, ]=="Ligneous flor"]
ClustQcor5 <- ClustQcor[, ClustQcor[8, ]=="Ligneous small fruit"]
ClustQcor6 <- ClustQcor[, ClustQcor[8, ]=="Nonfood"]
ClustQcor7 <- ClustQcor[, ClustQcor[8, ]=="Pome fruit"]
ClustQcor8 <- ClustQcor[, ClustQcor[8, ]=="Potato"]
ClustQcor9 <- ClustQcor[, ClustQcor[8, ]=="Strawberry GH"]
ClustQcor10 <- ClustQcor[, ClustQcor[8, ]=="Strawberry OF"]
ClustQcor11 <- ClustQcor[, ClustQcor[8, ]=="Veg GH"]
ClustQcor12 <- ClustQcor[, ClustQcor[8, ]=="Veg OF"]
ClustQcor13 <- ClustQcor[, ClustQcor[8, ]=="Veg substrate"]

#agregate over clusters
#clusterkey loop needs to be run for this to work
ClustQcorAg <- t(ClustQcor)
ClustQcorAg <- as.data.frame(ClustQcorAg)
ClustQcorAg$Oppervlakte <- as.numeric(ClustQcorAg$Oppervlakte)
ClustQcorAg$Var_cost <- as.numeric(ClustQcorAg$Var_cost)
ClustQcorAg$Var_cost_ppp <- as.numeric(ClustQcorAg$Var_cost_ppp)
ClustQcorAg$Fixed_cost <- as.numeric(ClustQcorAg$Fixed_cost)
ClustQcorAg$Revenue <- as.numeric(ClustQcorAg$Revenue)
ClustQcorAg$Gross_Margin <- as.numeric(ClustQcorAg$Gross_Margin)
ClustQcorAg$Kg_prijs <- as.numeric(ClustQcorAg$Kg_prijs)
ClustQcorAg$Cluster <- as.factor(ClustQcorAg$Cluster)
ClustQcorAgSD <- ClustQcorAg
ClustQcorAg <- aggregate(ClustQcorAg, by = list(ClustQcorAg$Cluster), FUN = mean, na.rm=TRUE) #returns NA for "cluster" so will give 1 warning for each row (6)
ClustQcorAg <- ClustQcorAg[ ,1:(ncol(ClustQcorAg)-1)] #drop Cluster NA
colnames(ClustQcorAg)[1] <- "Cluster"
ClustQcorAg

ClustQcorAgSD <- aggregate(ClustQcorAgSD[ ,1:ncol(ClustQcorAgSD)-1], by = list(ClustQcorAgSD$Cluster), FUN = sd, na.rm=TRUE)
ClustQcorAgSD

#discriptive plotting
barplot(Var_cost~Cluster, data = ClustQcorAg)
barplot(Var_cost_ppp~Cluster, data = ClustQcorAg)
barplot(Gross_Margin~Cluster, data = ClustQcorAg)
barplot(Fixed_cost~Cluster, data = ClustQcorAg)
barplot(Revenue~Cluster, data = ClustQcorAg)

barplot(Oppervlakte~Cluster, data = ClustQcorAg)
barplot(Kg_prijs~Cluster, data = ClustQcorAg)

barplot(t(ClustQcor1))
barplot(t(ClustQcor2))
barplot(t(ClustQcor3))
barplot(t(ClustQcor4))
barplot(t(ClustQcor5))
barplot(t(ClustQcor6))

splitcor1 <- Hmisc::rcorr(as.matrix(t(ClustQcor1[2:6,])), type="spearman")
#corrplot::corrplot(splitcor1$r, method = 'number', type = "upper", title = "cluster 1", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor1$r, type = "upper", method = 'number', title = "cluster 1", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor1$r[upper.tri(splitcor1$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

splitcor2 <- Hmisc::rcorr(as.matrix(t(ClustQcor2[2:6,])), type="spearman")
#corrplot::corrplot(splitcor2$r, method = 'number', type = "upper", title = "cluster 2", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor2$r, type = "upper", method = 'number', title = "cluster 2", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor2$r[upper.tri(splitcor2$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

splitcor3 <- Hmisc::rcorr(as.matrix(t(ClustQcor3[2:6,])), type="spearman")
#corrplot::corrplot(splitcor3$r, method = 'number', type = "upper", title = "cluster 3", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor3$r, type = "upper", method = 'number', title = "cluster 3", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor3$r[upper.tri(splitcor3$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

splitcor4 <- Hmisc::rcorr(as.matrix(t(ClustQcor4[2:6,])), type="spearman")
#corrplot::corrplot(splitcor4$r, method = 'number', type = "upper", title = "cluster 4", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor4$r, type = "upper", method = 'number', title = "cluster 4", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor4$r[upper.tri(splitcor4$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

splitcor5 <- Hmisc::rcorr(as.matrix(t(ClustQcor5[2:6,])), type="spearman")
#corrplot::corrplot(splitcor5$r, method = 'number', type = "upper", title = "cluster 5", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor5$r, type = "upper", method = 'number', title = "cluster 5", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor5$r[upper.tri(splitcor5$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

splitcor6 <- Hmisc::rcorr(as.matrix(t(ClustQcor6[2:6,])), type="spearman")
#corrplot::corrplot(splitcor6$r, method = 'number', type = "upper", title = "cluster 6", mar = c(1, 1, 1, 1))
corrplot::corrplot(splitcor6$r, type = "upper", method = 'number', title = "cluster 6", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor6$r[upper.tri(splitcor6$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )

##
splitcor1 <- Hmisc::rcorr(as.matrix(t(ClustQcor1[2:6,])), type="spearman")
corrplot::corrplot(splitcor1$r, type = "upper", method = 'number', title = "Cereals", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor1$r[upper.tri(splitcor1$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor2 <- Hmisc::rcorr(as.matrix(t(ClustQcor2[2:6,])), type="spearman")
corrplot::corrplot(splitcor2$r, type = "upper", method = 'number', title = "Flor GH", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor2$r[upper.tri(splitcor2$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor3 <- Hmisc::rcorr(as.matrix(t(ClustQcor3[2:6,])), type="spearman")
corrplot::corrplot(splitcor3$r, type = "upper", method = 'number', title = "Flor OF", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor3$r[upper.tri(splitcor3$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor4 <- Hmisc::rcorr(as.matrix(t(ClustQcor4[2:6,])), type="spearman")
corrplot::corrplot(splitcor4$r, type = "upper", method = 'number', title = "Ligneous flor", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor4$r[upper.tri(splitcor4$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor5 <- Hmisc::rcorr(as.matrix(t(ClustQcor5[2:6,])), type="spearman")
corrplot::corrplot(splitcor5$r, type = "upper", method = 'number', title = "Ligneous small fruit", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor5$r[upper.tri(splitcor5$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor6 <- Hmisc::rcorr(as.matrix(t(ClustQcor6[2:6,])), type="spearman")
corrplot::corrplot(splitcor6$r, type = "upper", method = 'number', title = "Nonfood", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor6$r[upper.tri(splitcor6$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor7 <- Hmisc::rcorr(as.matrix(t(ClustQcor7[2:6,])), type="spearman")
corrplot::corrplot(splitcor7$r, type = "upper", method = 'number', title = "Pome fruit", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor7$r[upper.tri(splitcor7$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor8 <- Hmisc::rcorr(as.matrix(t(ClustQcor8[2:6,])), type="spearman")
corrplot::corrplot(splitcor8$r, type = "upper", method = 'number', title = "Potato", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor8$r[upper.tri(splitcor8$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor9 <- Hmisc::rcorr(as.matrix(t(ClustQcor9[2:6,])), type="spearman")
corrplot::corrplot(splitcor9$r, type = "upper", method = 'number', title = "Strawberry GH", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor9$r[upper.tri(splitcor9$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor10 <- Hmisc::rcorr(as.matrix(t(ClustQcor10[2:6,])), type="spearman")
corrplot::corrplot(splitcor10$r, type = "upper", method = 'number', title = "Strawberry OF", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor10$r[upper.tri(splitcor10$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor11 <- Hmisc::rcorr(as.matrix(t(ClustQcor11[2:6,])), type="spearman")
corrplot::corrplot(splitcor11$r, type = "upper", method = 'number', title = "Veg GH", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor11$r[upper.tri(splitcor11$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor12 <- Hmisc::rcorr(as.matrix(t(ClustQcor12[2:6,])), type="spearman")
corrplot::corrplot(splitcor12$r, type = "upper", method = 'number', title = "Veg OF", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor12$r[upper.tri(splitcor12$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )
splitcor13 <- Hmisc::rcorr(as.matrix(t(ClustQcor13[2:6,])), type="spearman")
corrplot::corrplot(splitcor13$r, type = "upper", method = 'number', title = "Veg substrate", mar = c(1, 1, 1, 1),
                   col = "black", cl.pos = 'n', tl.col = 'black', 
                   number.cex = metan::resca(values = c(abs(splitcor13$r[upper.tri(splitcor13$r, diag=T)])), 
                                             new_min = 0.7, new_max = 1.6) )


#cluster plotting
df %>% #Revenue~PPP
  as_tibble() %>%
  mutate(cluster = k3$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Revenue, Var_cost_ppp, color = factor(cluster), label = state)) +
  geom_text()

df %>% #Revenue~PPP
  as_tibble() %>%
  mutate(cluster = k6$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Revenue, Var_cost_ppp, color = factor(cluster), label = state)) +
  ggrepel::geom_text_repel() + geom_point()

ggpubr::ggscatter(df, x = "Revenue", y = "Var_cost_ppp", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
          xlab = "Revenue", ylab = "Var_cost_ppp", label = row.names(df), cor.coeff.args = list(label.x.npc = "center", label.y.npc = "top"))

ggpubr::ggscatter(df, x = "Revenue", y = "Fixed_cost", add = "reg.line", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson",
                  xlab = "Revenue", ylab = "Vaste kost", label = row.names(df), cor.coeff.args = list(label.x.npc = "center", label.y.npc = "top"))


df %>% #Saldo~PPP
  as_tibble() %>%
  mutate(cluster = k3$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Gross_Margin, Var_cost_ppp, color = factor(cluster), label = state)) +
  geom_text()

df %>% #Saldo~PPP
  as_tibble() %>%
  mutate(cluster = k6$cluster,
         state = row.names(df)) %>%
  ggplot(aes(Gross_Margin, Var_cost_ppp, color = factor(cluster), label = state)) +
  geom_text()

###Hefboom
#(maybe also with results from large survey, by putting them in around inputA)
QcorAgHefboom$Hefboom_PPP_Revenue <- QcorAgHefboom$Revenue/(QcorAgHefboom$Revenue+QcorAgHefboom$Var_cost_ppp)
QcorAgHefboom$Hefboom_Opbrenst_Saldo <- QcorAgHefboom$Gross_Margin/(QcorAgHefboom$Gross_Margin+QcorAgHefboom$Revenue)
QcorAgHefboom$Hefboom_PPP_Saldo <- QcorAgHefboom$Gross_Margin/(QcorAgHefboom$Gross_Margin+QcorAgHefboom$Var_cost_ppp)

QcorAgHefboom <- aggregate(QcorAgHefboom, by = list(QcorAgHefboom$Cluster), FUN = mean) #returns NA for "cluster" so will give 1 warning for each row (12)
QcorAgHefboom <- QcorAgHefboom[1:(nrow(QcorAgHefboom)-1),] #drop x roup
row.names(QcorAgHefboom) <- QcorAgHefboom$Group.1

QcorAgHefboom %>%
  as_tibble() %>%
  mutate(cluster = k3$cluster,
         state = row.names(QcorAgHefboom)) %>%
  ggplot(aes(Hefboom_PPP_Revenue, Var_cost_ppp, color = factor(cluster), label = state)) +
  geom_text()

QcorAgHefboom %>%
  as_tibble() %>%
  mutate(cluster = k6$cluster,
         state = row.names(QcorAgHefboom)) %>%
  ggplot(aes(Hefboom_PPP_Revenue, Var_cost_ppp, color = factor(cluster), label = state)) +
  ggrepel::geom_text_repel() + geom_point()

QcorAgHefboom %>%
  as_tibble() %>%
  mutate(cluster = k6$cluster,
         state = row.names(QcorAgHefboom)) %>%
  ggplot(aes(Hefboom_Opbrenst_Saldo, Revenue, color = factor(cluster), label = state)) +
  ggrepel::geom_text_repel() + geom_point()

QcorAgHefboom %>%
  as_tibble() %>%
  mutate(cluster = k3$cluster,
         state = row.names(QcorAgHefboom)) %>%
  ggplot(aes(Hefboom_PPP_Saldo, Hefboom_Opbrenst_Saldo, color = factor(cluster), label = state)) +
  geom_text()


####hierarchical
distmat <- cluster::daisy(QcorAg[,2:6], metric = c("euclidean"))
summary(distmat)
x = as.matrix(distmat) 
# x = x[rowSums(is.na(x)) == 0, colSums(is.na(x)) == 0, drop = FALSE] #removing NA's from distmat
x[is.na(x)] <- mean(x, na.rm=TRUE) #replacing NA's from distmat with average distances
distmat <- as.dist(x)
summary(distmat)
clustering1 <- hclust(distmat, method="ward.D2") 
clustering1$labels <- QcorAg$Group.1

plot(clustering1) #dendrogram
plot(hang.dendrogram(raise.dendrogram(as.dendrogram(clustering1), 100000), hang_height = 9000), horiz = T, main = "hclust ward D²", axes = F)

plot(length(clustering1$height):1, clustering1$height, type = "b") #elbow

silhouette1 = c(NA)
for(i in 2:10){
  pam_clusters = cluster::pam(as.matrix(distmat), diss = TRUE, k = i)
  silhouette1 = c(silhouette1 ,pam_clusters$silinfo$avg.width)
  print(i)
}
plot(1:10, silhouette1, xlab = "Clusters", ylab = "Silhouette")
lines(1:10, silhouette1) #silhouette

#bootstrap for hclust
pv.clust.mat <- QcorAg[2:6]
fit <- pvclust::pvclust(t(pv.clust.mat), method.hclust="ward.D2", method.dist="euclidean") #bootstrap
fit$hclust$labels <- QcorAg$Group.1
plot(fit) #provides two types of p-values: AU (Approximately Unbiased) p-value and BP (Bootstrap Probability) value. AU p-value, which is computed by multiscale bootstrap resampling, is a better approximation to unbiased p-value than BP value computed by normal bootstrap resampling
pvclust::pvrect(fit, alpha=.95, pv="au", type="geq", max.only = T)


####PCA
#pca
#install.packages("BiocManager")  #BiocManager::install("pcaMethods")
QcorAg[,2:6] <- scale(QcorAg[,2:6])
pcData <- pcaMethods::prep(QcorAg[,2:6], scale="uv", center=TRUE)

x <- apply(pcData, 2, is.na)
x <- apply(x, 2, all)
x <-  as.logical(x)
pcData <- pcData[ , !x]

pca <- pcaMethods::pca(pcData, method="ppca", center=FALSE, nPcs=4)

pca2 <- pcaMethods::pca(pcData, method="ppca", center=T) #To explain the axes (dims of fviz_cluster(kclust) )
summary(pca2)
pcaMethods::loadings(pca2) #alles laad samen op 1 dimentie (omzet?) + extra variatie (die afwijkt van omzet) in Var_cost_ppp en Gross_Margin maken verder nog een klein verschil

pca.sum <- summary(pca)
loading_out <- pcaMethods::loadings(pca) ###############################
# write.table(loading_out, "W:/MODELLEN/DATA-MODELLEN_beperkt/LMN_AMS/2020_OD4OS/OD4OS10020_GLASGROENTEN/Verwerking data_cluster opdeling/output/1 per ha/loadings.csv", sep = ";", dec = ",", row.names = T, col.names = T)

corrplot::corrplot(correlations$r, method = "color", type = "upper", tl.pos = "d") ###########################################

pca.var <- as.data.frame(pca.sum[1, ])
pca.var$component <- factor(rownames(pca.var),levels = rownames(pca.var))
colnames(pca.var) <- c("variance.explained", "component")
ggplot(pca.var) + geom_col(aes(y = variance.explained, x = component)) + geom_line(aes(y = variance.explained, x = component, group=1)) ###############################

#pca plotting
pcDummy <- pcData #run normal pca with dummy data (na's replaced) then replace values with ppca results (ggbiplot can't handle ppca's object type)
pcDummy[is.na(pcDummy)]<-12345
pcaDum <- prcomp(pcDummy, center=TRUE, scale.=TRUE)
pcaPlot <- pcaDum

pcaPlot$x<-pca@scores 
pcaPlot$rotation<-pca@loadings 
pcaPlot$sdev<-pca@sDev
pcaPlot$center<-pca@center
pcaPlot$scale<-pca@scale

cluscf <- cutree(clustering1, 3)
clustfact <- as.factor(as.vector(cluscf))

ggbiplot::ggbiplot(pcaPlot, groups = clustfact) + #xlim(-5, 1.5) + ylim(-2, 6) + #xlim(-10, 5) + ylim(-10, 5) + 
  geom_text(aes(label=row.names(df)), hjust=0, vjust=0) #pca plot

