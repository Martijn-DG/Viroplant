rm(list=ls())


lmOut <- function(res, ndigit=4) {
  # If summary has not been run on the model then run summary
  if (length(grep("summary", class(res)))==0) res <- summary(res)
  
  co <- res$coefficients
  nvar <- nrow(co)
  ncol <- ncol(co)
  f <- res$fstatistic
  formatter <- function(x) format(round(x,ndigit),nsmall=ndigit)
  
  # This sets the number of rows before we start recording the coefficients
  nstats <- 4
  
  # G matrix stores data for output
  G <- matrix("", nrow=nvar+nstats, ncol=ncol+1)
  
  G[1,1] <- toString(res$call)
  
  # Save rownames and colnames
  G[(nstats+1):(nvar+nstats),1] <- rownames(co)
  G[nstats, 2:(ncol+1)] <- colnames(co)
  
  # Save Coefficients
  G[(nstats+1):(nvar+nstats), 2:(ncol+1)] <- formatter(co)
  
  # Save F-stat
  G[1,2] <- paste0("F(",f[2],",",f[3],")")
  G[2,2] <- formatter(f[1])
  
  # Save F-p value
  G[1,3] <- "Prob > P"
  G[2,3] <- formatter(1-pf(f[1],f[2],f[3]))
  
  # Save R2
  G[1,4] <- "R-Squared"
  G[2,4] <- formatter(res$r.squared)
  
  # Save Adj-R2
  G[1,5] <- "Adj-R2"
  G[2,5] <- formatter(res$adj.r.squared)
  
  print(G)
  return(G)
}

library(Hmisc)
library(tidyverse)
library(gmodels)
library(xlsx)

df_names <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/broad Q/General survey_Text.csv',
                      n_max = 2, col_names = F) #reading in column names (both short and long)
df_values <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/broad Q/General survey_Text.csv',
                       skip = 2, col_names = F) #reading in values
Hmisc::label(df_values) <- as.list(df_names[2,]) #assigning long names as labels
names(df_values) <- as.character(df_names[1,]) #assigning short names as headers
data <- as.data.frame(df_values)
rm("df_names", "df_values")

#adding ID
data$ID <- c(1:nrow(data))

#duration
data$`Duration (in seconds)` <- data$`Duration (in seconds)`/60
colnames(data)[2] <- "Duration"
Hmisc::label(data)[2] <- as.list("Duration (in minutes)")
quantile(data$Duration)

#removing non starters
data <- data[data$Q1.2 == "Ja", ]
#removing X
data <- data[data$Q2.1_1 != "x", ]
#removing useless cols
data <- data[, -c(1, 3:4)]
#filling meaningfull na's
data$Q3.3[is.na(data$Q3.3)] <- "Weet het niet"

#Q3.3 split into yes/no and know/don't know
data$Q3.3_Accept <- NA
data$Q3.3_Accept[data$Q3.3 == "Ja"] <- T
data$Q3.3_Accept[data$Q3.3 == "Nee"] <- F

data$Q3.3_Unsure <- NA
data$Q3.3_Unsure[data$Q3.3 == "Weet het niet"] <- T
data$Q3.3_Unsure[data$Q3.3 == "Ja"] <- F
data$Q3.3_Unsure[data$Q3.3 == "Nee"] <- F

#Q3.2 split into yes/no and technology
data <- separate(data, Q3.2, c(NA, "Q3.2_How"), sep = ", ", remove=F)
data <- separate(data, Q3.2, c("Q3.2_Known", NA), sep = ", ")
data$Q3.2_Known <- data$Q3.2_Known=="Ja"
#Make levels identical
data$Q3.2_How <- factor(data$Q3.2_How)
A <- c(levels(data$Q3.2_How), "Minder belang", "Anorganische stof", "Extract micro-organisme", 
       "Insect", "Micro-organisme", "Plantenextract", "Wil weten")
dim(A) <- c(length(levels(data$Q3.2_How)),2)
data$Q3.2_How <- as.character(data$Q3.2_How)
for (i in 1:nrow(A)) {
  data$Q3.2_How[data$Q3.2_How == A[i,1]] <- A[i,2]
}

data$Q3.1_2 <- factor(data$Q3.1_2)
A <- c(levels(data$Q3.1_2), "Anorganische stof", NA, "Micro-organisme", NA, NA, NA, NA, "Extract micro-organisme", 
       NA, "Insect", NA, NA, "Micro-organisme", NA, NA, NA, "Plantenextract", NA, "Micro-organisme", NA, NA, NA, "Micro-organisme")
dim(A) <- c(length(levels(data$Q3.1_2)),2)
data$Q3.1_2 <- as.character(data$Q3.1_2)
for (i in 1:nrow(A)) {
  data$Q3.1_2[data$Q3.1_2 == A[i,1]] <- A[i,2]
}
rm(A)

#compare 3.2 to 3.1.2 and make it a new column (correctness)
data$Correct <- data$Q3.1_2==data$Q3.2_How

#make new categorical var for organic farmers
data$Bio <- data$Q5.3_1 == 100
data$Bio[data$Q4.1_8_EDIT == "Bioteler"] <- T

#reduce categories (n<30 samenzetten)
barplot(sort(table(data$Q2.1_1)), main="Deelnemers per teelt categorie", xlab="Deelnemers", cex.names=0.9, las=1)
t1 <- as.data.frame(sort(table(data$Q2.1_1)))
data$Q2.1_1[data$Q2.1_1=="nijverheid"] <- "akkerbouw"
data$Q2.1_1[data$Q2.1_1=="peul"] <- "akkerbouw"
data$Q2.1_1[data$Q2.1_1=="aardappelen"] <- "akkerbouw"
data$Q2.1_1[data$Q2.1_1=="granen"] <- "akkerbouw"
data$Q2.1_1[data$Q2.1_1=="klein fruit"] <- "aardbei + kf"
data$Q2.1_1[data$Q2.1_1=="aardbei"] <- "aardbei + kf"
data$Q2.1_1[data$Q2.1_1=="bomen"] <- "sierteelt"
sort(table(data$Q2.1_1))

#translate data
data$Q2.1_1 <- as.factor(data$Q2.1_1)
levels(data$Q2.1_1) <- c("Strawberry+Small ligneous", "Arable crops", "Vegetables", "Veg substrate", "Vegetables", "Pomme fruit", "Floriculture")

data$Q3.1_3[data$Q3.1_3=="Nuttige"] <- "BCA insect"
data$Q3.1_3[data$Q3.1_3=="Nematoden"] <- "Nematodes"
data$Q3.1_3[data$Q3.1_3=="Nematodes"] <- NA
data$Q3.1_3 <- as.factor(data$Q3.1_3)

data$Q3.1_4 <- as.factor(data$Q3.1_4)
levels(data$Q3.1_4) <- c("Bacterium", "Fungus", "Insect", "Weed", "Plant Strengthener", "Virus")

data$Q3.1_2 <- as.factor(data$Q3.1_2)
levels(data$Q3.1_2) <- c("Anorganic substance", "Extract micro-organism", "Insect", "Micro-organism", "Plant extract")
data$Q3.2_How <- as.factor(data$Q3.2_How)
levels(data$Q3.2_How) <- c("Anorganic substance", "Extract micro-organism", "Insect", "Micro-organism", "Less important", "Plant extract", "Want to know")

#relevel factors
data$Q2.1_1 <- relevel(data$Q2.1_1, ref=3) #Veg non substrate
data$Q3.1_4 <- relevel(data$Q3.1_4, ref=1) #Bacterium
data$Q3.1_3 <- relevel(data$Q3.1_3, ref=1) #insect
data$Q3.1_2 <- relevel(data$Q3.1_2, ref=1) #Anorganic substance
data$Q3.2_How <- relevel(data$Q3.2_How, ref=1) #Anorganic substance

#split columns
long <- separate_rows(data,Q4.1 , sep = ",", convert = FALSE)
long$Q4.1_FIXED <- long$Q4.1
long <- separate_rows(long,Q4.1_8_EDIT , sep = ", ", convert = FALSE)
long <- separate_rows(long,Q4.2 , sep = ",", convert = FALSE)
long$Q4.2_FIXED <- long$Q4.2
long <- separate_rows(long,Q4.2_7_EDIT , sep = ", ", convert = FALSE)
long <- separate_rows(long,Q4.3 , sep = ",", convert = FALSE)
long <- separate_rows(long,Q5.1 , sep = ",", convert = FALSE)

#process open text fields
long$Q4.1[(long$Q4.1 == "Andere") & !is.na(long$Q4.1)] <- long$Q4.1_8_EDIT[(long$Q4.1 == "Andere") & !is.na(long$Q4.1)]
long$Q4.2[(long$Q4.2 == "Andere") & !is.na(long$Q4.2)] <- long$Q4.2_7_EDIT[(long$Q4.2 == "Andere") & !is.na(long$Q4.2)]
long$Q4.3_FIXED <- long$Q4.3
long$Q4.3[(long$Q4.3 == "Andere") & !is.na(long$Q4.3)] <- long$Q4.3_6_EDIT[(long$Q4.3 == "Andere") & !is.na(long$Q4.3)]
long$Q5.1[(long$Q5.1 == "Andere") & !is.na(long$Q5.1)] <- long$Q5.1_6_EDIT[(long$Q5.1 == "Andere") & !is.na(long$Q5.1)]
long$Q5.1_FIXED <- long$Q5.1
long$Q4.3[is.na(long$Q4.3)] <- long$Q5.1[is.na(long$Q4.3)]
long$Q4.3_FIXED[is.na(long$Q4.3_FIXED)] <- long$Q5.1_FIXED[is.na(long$Q4.3_FIXED)]

#set var types
data$Q5.2_1 <- as.numeric(data$Q5.2_1)
data$Q2.2 <- data$Q2.2=="Ja"

#translate long
long$Q4.1_FIXED <- as.factor(long$Q4.1_FIXED)
levels(long$Q4.1_FIXED) <- c("Other", "Effectiveness", "No alternatives", "Ease of use", "Health (user)", "Environment", "Price", "Residue")
long$Q4.2_FIXED <- as.factor(long$Q4.2_FIXED)
levels(long$Q4.2_FIXED) <- c("Other", "Effectiveness", "Ease of use", "Health (user)", "Environment", "Price", "Residue")
long$Q4.3_FIXED <- as.factor(long$Q4.3_FIXED)
levels(long$Q4.3_FIXED) <- c("Other", "Colleagues", "Experience", "Fyto rep", "Marketing material", "Independant advisor", "Research stations")
#long$Q5.1_FIXED <- as.factor(long$Q5.1_FIXED)
#levels(long$Q5.1_FIXED) <- c("Other", "Colleagues", "Experience", "Fyto rep", "Marketing material", "Independant advisor", "Research stations")

#relevel factors
long$Q4.1_FIXED <- relevel(long$Q4.1_FIXED, ref=2) #Effectiviteit
long$Q4.2_FIXED <- relevel(long$Q4.2_FIXED, ref=2) #Effectiviteit
long$Q4.3_FIXED <- relevel(long$Q4.3_FIXED, ref=7) #Proefstation

############################prep done#######################################################

#discriptives
barplot(sort(table(data$Q2.1_1)), main="Number of participants per category", xlab="N", cex.names=0.9, las=1)
t2 <- as.data.frame(sort(table(data$Q2.1_1)))

t3 <- as.data.frame(sort(table(data$Q3.1_1))) #product
t4 <- as.data.frame(sort(table(data$Q3.1_2))) #soort technologie
t5 <- as.data.frame(sort(table(data$Q3.1_3))) #formulering
t6 <- as.data.frame(sort(table(data$Q3.1_4))) #target

t7 <- as.data.frame(sort(table(data$Q3.2_Known))) #denkt technologie te kennen
t8 <- as.data.frame(sort(table(data$Q3.2_How))) #details/reden
t9 <- as.data.frame(sort(table(data$Correct))) #aandeel dat beschikt over kennis van technologie
t10 <- as.data.frame(sort(table(data$Correct[data$Q3.2_Known==T]))) #aandeel correcte kennis van zei die de kennis denken te hebben (zie crosstable(chisq) later)
t11 <- as.data.frame(sort(table(data$Q3.3))) #acceptatie virus

count_unique <- function(x) {
  y <- length(unique(x)) 
  return(y)
}
sort_M <- function(x) {
  y <- x[order(x$x), ] 
  return(y)
}
t12 <- as.data.frame(sort_M(aggregate(long$ID, list(long$Q4.1), FUN = "count_unique"))) #product keuze pros
t13 <- as.data.frame(sort_M(aggregate(long$ID, list(long$Q4.2), FUN = "count_unique"))) #cons
t14 <- as.data.frame(sort_M(aggregate(long$ID, list(long$Q4.3), FUN = "count_unique"))) #info

t15 <- data.frame(x=as.numeric(c(1:5)))
x1 <- mean(data$Q5.2_1, na.rm=T) #effectiviteit
x2 <- mean(data$Q5.2_2, na.rm=T) #prijs
x3 <- mean(data$Q5.2_4, na.rm=T) #millieu-impact
x4 <- mean(data$Q5.2_5, na.rm=T) #gebruiksgemak
x5 <- mean(data$Q5.2_6, na.rm=T) #gezondheid
t15$attribute <- c("effectiveness", "price", "environment", "eou", "health")
t15$score <- c(x1, x2, x3, x4, x5)

quantile(data$Q5.3_1, na.rm=T) #%bio
sort(table(data$Bio))
quantile(data$Q5.3_1[data$Bio==F], na.rm=T) #%bio bij conventionele telers

t16 <- data.frame(x=as.numeric(c(1:length(unique(data$Q2.1_1)))))
i=1
for (n in unique(data$Q2.1_1)) {
  x <- sort(table(data$Q3.1_1[data$Q2.1_1 == n]),decreasing=TRUE) #most popular products per crop type (nuttigen = enkel nuttigen)
  
  t16$NProducts[i] <- length(x)
  t16$CropCat[i] <- n
  i=i+1
  
  print(paste(n, "(5 of", length(x), "products shown) n =", length(data$Q3.1_1[data$Q2.1_1 == n]) ))
  print(x[1:5])
  cat("\n\n")
}

#generating output for discriptives
write.xlsx(t1, file="output_disc.xlsx", sheetName="Participants per category", row.names=FALSE)
write.xlsx(t2, file="output_disc.xlsx", sheetName="Participants per category (merged)", append=TRUE, row.names=FALSE)
write.xlsx(t3, file="output_disc.xlsx", sheetName="Products", append=TRUE, row.names=FALSE)
write.xlsx(t16, file="output_disc.xlsx", sheetName="ProductsXcat", append=TRUE, row.names=FALSE)
write.xlsx(t4, file="output_disc.xlsx", sheetName="Technology", append=TRUE, row.names=FALSE)
write.xlsx(t5, file="output_disc.xlsx", sheetName="Formulation", append=TRUE, row.names=FALSE)
write.xlsx(t6, file="output_disc.xlsx", sheetName="Target", append=TRUE, row.names=FALSE)
write.xlsx(t7, file="output_disc.xlsx", sheetName="Denkt tech te kennen", append=TRUE, row.names=FALSE)
write.xlsx(t8, file="output_disc.xlsx", sheetName="Genoemde tech", append=TRUE, row.names=FALSE)
write.xlsx(t9, file="output_disc.xlsx", sheetName="Correcte kennis (vs geen of foute)", append=TRUE, row.names=FALSE)
write.xlsx(t10, file="output_disc.xlsx", sheetName="Correcte kennis (vs foute)", append=TRUE, row.names=FALSE)
write.xlsx(t11, file="output_disc.xlsx", sheetName="Acceptatie", append=TRUE, row.names=FALSE)
write.xlsx(t12, file="output_disc.xlsx", sheetName="Pro", append=TRUE, row.names=FALSE)
write.xlsx(t13, file="output_disc.xlsx", sheetName="Contra", append=TRUE, row.names=FALSE)
write.xlsx(t14, file="output_disc.xlsx", sheetName="Info", append=TRUE, row.names=FALSE)
write.xlsx(t15, file="output_disc.xlsx", sheetName="Scores", append=TRUE, row.names=FALSE)
#write.xlsx(t17, file="output_disc.xlsx", sheetName="x", append=TRUE, row.names=FALSE)

#missing values?
#Amelia::missmap(data, col=c("blue", "red"), legend=T)
#Amelia::missmap(long, col=c("blue", "red"), legend=T)

#regressions and chi-sq tests
Cross.Table <- CrossTable(data$Q2.1_1, data$Q2.2, chisq=F, prop.c=F, prop.t=F, prop.chisq=F) 
c1 <- reshape(as.data.frame(Cross.Table[["t"]]), idvar = "x", timevar = "y", direction = "wide")
c2 <- reshape(as.data.frame(Cross.Table[["prop.row"]]), idvar = "x", timevar = "y", direction = "wide")
fit <- lm(Q2.2~Q2.1_1, data) #estimate = probability of having experience
m1 <- lmOut(summary(fit))

Cross.Table <- CrossTable(data$Q3.2_Known, data$Correct, chisq=T) #64% denkt technologie te kennen, 75% daarvan is correct (48% van totaal); Pearson's Chi-squared test with Yates' continuity correction p =  4.78403e-32
c3 <- reshape(as.data.frame(Cross.Table[["t"]]), idvar = "x", timevar = "y", direction = "wide")
c4 <- reshape(as.data.frame(Cross.Table[["prop.row"]]), idvar = "x", timevar = "y", direction = "wide")
c5 <- reshape(as.data.frame(Cross.Table[["prop.col"]]), idvar = "x", timevar = "y", direction = "wide")

fit <- lm(Correct~Q3.1_2+Q2.1_1, data) #estimate = probability of being correct
m2 <- lmOut(summary(fit)) 

fit <- lm(Q3.3_Unsure~Q2.1_1, data) #estimate = probability of Uncertainty
m3 <- lmOut(summary(fit)) #bulk = more uncertainty
fit <- lm(Q3.3_Accept~Q2.1_1, data) #estimate = probability of acceptance
m4 <- lmOut(summary(fit)) #strawberry GH = less acceptance 
Cross.Table <- CrossTable(data$Q2.1_1, data$Q3.3, chisq=T, prop.c=F, prop.t=F, prop.chisq=F) #akker meer weet het niet, aardbei meer nee 
c3B <- reshape(as.data.frame(Cross.Table[["t"]]), idvar = "x", timevar = "y", direction = "wide")
c4B <- reshape(as.data.frame(Cross.Table[["prop.row"]]), idvar = "x", timevar = "y", direction = "wide")
c5B <- reshape(as.data.frame(Cross.Table[["prop.col"]]), idvar = "x", timevar = "y", direction = "wide")

fit <- lm(Q2.2~Q3.3_Accept*Q2.1_1, data) #Q2.2 = experience
m5 <- lmOut(summary(fit)) 
fit <- lm(Q2.2~Q3.3_Unsure*Q2.1_1, data) 
m6 <- lmOut(summary(fit)) #probability of having experience is lower for "i don't know"

fit <- lm(Q2.2~Q5.2_1+Q5.2_2+Q5.2_4+Q5.2_5+Q5.2_6, data) #ervaring = positiever beeld (vooral eff) maar niet over prijs
m7 <- lmOut(summary(fit))
m7[6:10, 1] <- c("Effectiveness", "Price", "Environment", "Ease of use", "Health")
fit <- lm(Q2.2~Q5.2_1, data) #eff
summary(fit) 
fit <- lm(Q2.2~Q5.2_2, data) #prijs
summary(fit)  
fit <- lm(Q2.2~Q5.2_4, data) #millieu
summary(fit)  
fit <- lm(Q2.2~Q5.2_5, data) #gemak
summary(fit)  
fit <- lm(Q2.2~Q5.2_6, data) #gezondheid
summary(fit) 

fit <- lm(Q5.2_1+Q5.2_2+Q5.2_4+Q5.2_5+Q5.2_6~Q2.1_1, data) #ervaring = positiever beeld (vooral eff) maar niet over prijs
m8 <- lmOut(summary(fit))

fit <- lm(Q5.2_1~Q2.1_1, data)
summary(fit) #alles behalve substraat en aardbei is negatiever
sort_M(aggregate(data$Q5.2_1, list(data$Q2.1_1), FUN = "mean", na.rm=T)) #eff 
s1 <- aggregate(data$Q5.2_1, list(data$Q2.1_1), FUN = "mean", na.rm=T)

fit <- lm(Q5.2_2~Q2.1_1, data)
summary(fit) #iedereen scoort matig negatief
sort_M(aggregate(data$Q5.2_2, list(data$Q2.1_1), FUN = "mean", na.rm=T)) #prijs 
s2 <- aggregate(data$Q5.2_2, list(data$Q2.1_1), FUN = "mean", na.rm=T)
  
fit <- lm(Q5.2_4~Q2.1_1, data)
summary(fit) #akker, pit en groenten openl en volglas zijn minder positief
sort_M(aggregate(data$Q5.2_4, list(data$Q2.1_1), FUN = "mean", na.rm=T)) #millieu 
s3 <- aggregate(data$Q5.2_4, list(data$Q2.1_1), FUN = "mean", na.rm=T)

fit <- lm(Q5.2_5~Q2.1_1, data)
summary(fit) #enkel akker pit en sier scoren negatief
sort_M(aggregate(data$Q5.2_5, list(data$Q2.1_1), FUN = "mean", na.rm=T)) #eou 
s4 <- aggregate(data$Q5.2_5, list(data$Q2.1_1), FUN = "mean", na.rm=T)

fit <- lm(Q5.2_6~Q2.1_1, data)
summary(fit) #akker, pit, groenten vol glas en openl zijn minder positief, aardbei is extra positief
sort_M(aggregate(data$Q5.2_6, list(data$Q2.1_1), FUN = "mean", na.rm=T)) #health
s5 <- aggregate(data$Q5.2_6, list(data$Q2.1_1), FUN = "mean", na.rm=T)

c6 <- as.data.frame(s1[ ,1])
c6$effectiveness <- s1[ ,2]
c6$price <- s2[ ,2]
c6$environment <- s3[ ,2]
c6$eou <- s4[ ,2]
c6$health <- s5[ ,2]

fit <- lm(Q5.2_5~Q3.1_3, data) 
#fit <- lm(Q5.2_5~Q3.1_3+Q2.1_1, data) #remains if you control for crop type
m9 <- lmOut(summary(fit)) #enkel ervaring met nuttigen = beter beeld over EOU; SL (niet sig) scoort het beste
c7 <- sort_M(aggregate(data$Q5.2_5, list(data$Q3.1_3), FUN = "mean", na.rm=T)) #eou
sort_M(aggregate(data$Q5.2_5, list(data$Q3.1_3), FUN = "count_unique")) #very little data

chisq.test(long$Q4.1_FIXED, long$Q2.1_1, simulate.p.value = T) #not sure what the simulate p value does but it gives the same result on the 3 below as well :/
chisq.test(long$Q4.1_FIXED, long$Q2.1_1, simulate.p.value = F) #this is the default (+ same used by crosstable) and is different for each below
#good thing is the above are always both significant
chisq.test(long$Q4.1_FIXED, long$Q2.1_1, simulate.p.value = T, correct = T) #some cells are 0 so yates correction is needed
chisq.test(long$Q4.1_FIXED, long$Q2.1_1, simulate.p.value = F, correct = T) #the correction doesn't change anything
pro <- CrossTable(long$Q4.1_FIXED, long$Q2.1_1, chisq=T, prop.c=F, prop.r=F, prop.t=F, prop.chisq=F)
c8 <- reshape(as.data.frame(pro[["t"]]), idvar = "x", timevar = "y", direction = "wide")
c9 <- reshape(as.data.frame(pro[["prop.row"]]), idvar = "x", timevar = "y", direction = "wide")
ct <- chisq.test(long$Q4.1_FIXED, long$Q2.1_1, simulate.p.value = F, correct = T)
corrplot::corrplot(pro[["prop.col"]], title = paste0("chisq.test p=", ct[["p.value"]]))

chisq.test(long$Q4.2_FIXED, long$Q2.1_1, simulate.p.value = F, correct = T)
con <- CrossTable(long$Q4.2_FIXED, long$Q2.1_1, chisq=T, prop.c=F, prop.r=F, prop.t=F, prop.chisq=F) 
c10 <- reshape(as.data.frame(con[["t"]]), idvar = "x", timevar = "y", direction = "wide")
c11 <- reshape(as.data.frame(con[["prop.row"]]), idvar = "x", timevar = "y", direction = "wide")
ct <- chisq.test(long$Q4.2_FIXED, long$Q2.1_1, simulate.p.value = F, correct = T)
corrplot::corrplot(con[["prop.col"]], title = paste0("chisq.test p=", ct[["p.value"]]))
corrplot::corrplot(con[["prop.col"]], method = 'number', col = "black", cl.pos = 'n', 
                   tl.col = 'black', title = paste0("chisq.test p=", ct[["p.value"]]), mar = c(1, 1, 1, 1),
                   number.cex = metan::resca(values = c(abs(con[["prop.col"]])), new_min = 0.7, new_max = 1.6) )

chisq.test(long$Q3.1_3, long$Q4.1_FIXED, simulate.p.value = F, correct = T) #not enough data per cell
pro2 <- CrossTable(long$Q3.1_3, long$Q4.1_FIXED, chisq=F, prop.c=F, prop.t=F, prop.chisq=F)
pro2[["t"]]
corrplot::corrplot(pro2[["prop.row"]])

chisq.test(long$Q3.1_3, long$Q4.2_FIXED, simulate.p.value = F, correct = T) #not enough data per cell
con2 <- CrossTable(long$Q3.1_3, long$Q4.2_FIXED, chisq=F, prop.c=F, prop.t=F, prop.chisq=F)
con2[["t"]]
corrplot::corrplot(con2[["prop.row"]])

chisq.test(long$Q4.1_FIXED, long$Q3.1_4, simulate.p.value = F, correct = T) #not enough data per cell
ct <- CrossTable(long$Q4.1_FIXED, long$Q3.1_4, chisq=F, prop.c=T, prop.r=F, prop.t=F, prop.chisq=F) #geen verschillen
ct[["t"]]
corrplot::corrplot(ct[["prop.col"]])

chisq.test(long$Q4.2_FIXED, long$Q3.1_4, simulate.p.value = F, correct = T) #not enough data per cell
ct <- CrossTable(long$Q4.2_FIXED, long$Q3.1_4, chisq=F, prop.c=T, prop.r=F, prop.t=F, prop.chisq=F) #geen verschillen
ct[["t"]]
corrplot::corrplot(ct[["prop.col"]])

info <- CrossTable(long$Q4.3_FIXED, long$Q2.1_1, chisq=T, prop.c=F, prop.r=T, prop.t=F, prop.chisq=F)
info[["t"]] 
round(info[["prop.col"]], digits=2) #andere info voorkeuren afhankelijk van teelt
corrplot::corrplot(info[["prop.col"]]) #e.g. de gemiddelde akkerbouwer luisterd naar de fyto rep

fit <- lm(Q3.3_Accept~Q4.3_FIXED, long) 
m10 <- lmOut(summary(fit)) #geen effect
#CrossTable(long$Q4.3_FIXED, long$Q3.3_Accept, chisq=F, prop.c=F, prop.r=T, prop.t=F, prop.chisq=F)

fit <- lm(Q3.3_Unsure~Q4.3_FIXED, long) 
m11 <- lmOut(summary(fit)) #fyto rep is meer onzeker
#CrossTable(long$Q4.3_FIXED, long$Q3.3_Unsure, chisq=F, prop.c=F, prop.r=T, prop.t=F, prop.chisq=F)

fit <- lm(Correct~Q4.3_FIXED, long) 
m12 <- lmOut(summary(fit)) #andere, marketing en onafhankelijke adv zijn meer correct 
CrossTable(long$Q4.3_FIXED, long$Correct, chisq=T, prop.c=F, prop.r=T, prop.t=F, prop.chisq=F) #maar wel weinig data

fit <- lm(Correct~Q3.3_Unsure, long) 
m13 <- lmOut(summary(fit)) #no effect of knowledge on certainty
CrossTable(long$Q3.3_Unsure, long$Correct, chisq=T, prop.c=T, prop.r=T, prop.t=F, prop.chisq=F)

fit <- lm(Correct~Q3.3_Accept, long) 
m14 <- lmOut(summary(fit)) #there is more knowledge in the non acceptance group = lack of knowledge is associated with a slightly higher acceptance
CrossTable(long$Q3.3_Accept, long$Correct, chisq=T, prop.c=T, prop.r=T, prop.t=F, prop.chisq=F)

fit <- lm(Q5.3_1~Q2.1_1 ,data) 
m15 <- lmOut(summary(fit)) #meer bio gebruik in groenten substraat en strawGH, minder in bulk

fit <- lm(Q5.3_1~Correct ,data) 
m16 <- lmOut(summary(fit)) #correct knowledge = more organic use
sort_M(aggregate(data$Q5.3_1, list(data$Correct), FUN = "mean", na.rm=T))

fit <- lm(Q5.3_1~Q4.3_FIXED ,long) 
m17 <- lmOut(summary(fit)) #more organic use in other and independant
sort_M(aggregate(long$Q5.3_1, list(long$Q4.3_FIXED), FUN = "mean", na.rm=T))

fit <- lm(Q5.3_1~Q3.3_Accept ,data) 
#fit <- lm(Q3.3_Accept~Q5.3_1 ,data) 
m18 <- lmOut(summary(fit)) #trend towards less acceptance in organic (less organic in non accepters)
sort_M(aggregate(data$Q5.3_1, list(data$Q3.3_Accept), FUN = "mean", na.rm=T))
boxplot(Q5.3_1~Q3.3_Accept ,data)

#generate output for statistics

write.xlsx(m1, file="output_mod.xlsx", sheetName="experience&crop", row.names=F)
write.xlsx(m2, file="output_mod.xlsx", sheetName="correrct&realtech", append=T, row.names=F)
write.xlsx(m3, file="output_mod.xlsx", sheetName="certainty&crop", append=T, row.names=F)
write.xlsx(m4, file="output_mod.xlsx", sheetName="acceptance&crop", append=T, row.names=F)
write.xlsx(m5, file="output_mod.xlsx", sheetName="experience&acceptance", append=T, row.names=F)
write.xlsx(m6, file="output_mod.xlsx", sheetName="experience&certainty", append=T, row.names=F)
write.xlsx(m7, file="output_mod.xlsx", sheetName="experience&scores", append=T, row.names=F)
write.xlsx(m8, file="output_mod.xlsx", sheetName="scores&crop", append=T, row.names=F)
write.xlsx(m9, file="output_mod.xlsx", sheetName="eou&form", append=T, row.names=F)
write.xlsx(m10, file="output_mod.xlsx", sheetName="info&acceptance", append=T, row.names=F)
write.xlsx(m11, file="output_mod.xlsx", sheetName="info&certainty", append=T, row.names=F)
write.xlsx(m12, file="output_mod.xlsx", sheetName="info&correct", append=T, row.names=F)
write.xlsx(m13, file="output_mod.xlsx", sheetName="certainty&correct", append=T, row.names=F)
write.xlsx(m14, file="output_mod.xlsx", sheetName="acceptance&correct", append=T, row.names=F)
write.xlsx(m15, file="output_mod.xlsx", sheetName="crop&bio", append=T, row.names=F)
write.xlsx(m16, file="output_mod.xlsx", sheetName="correct&bio", append=T, row.names=F)
write.xlsx(m17, file="output_mod.xlsx", sheetName="info&bio", append=T, row.names=F)
write.xlsx(m18, file="output_mod.xlsx", sheetName="acceptance&bio", append=T, row.names=F)

write.xlsx(c1, file="output_ct.xlsx", sheetName="n experience&crop", row.names=F)
write.xlsx(c2, file="output_ct.xlsx", sheetName="p.row experience&crop", append=T, row.names=F)
write.xlsx(c3, file="output_ct.xlsx", sheetName="n correct&knowledge", row.names=F)
write.xlsx(c4, file="output_ct.xlsx", sheetName="p.row correct&knowledge", append=T, row.names=F)
write.xlsx(c5, file="output_ct.xlsx", sheetName="p.col correct&knowledge", append=T, row.names=F)
write.xlsx(c3B, file="output_ct.xlsx", sheetName="n acceptance&crop", append=T, row.names=F)
write.xlsx(c4B, file="output_ct.xlsx", sheetName="p.row acceptance&crop", append=T, row.names=F)
write.xlsx(c5B, file="output_ct.xlsx", sheetName="p.col acceptance&crop", append=T, row.names=F)
write.xlsx(c6, file="output_ct.xlsx", sheetName="scores&crop", append=T, row.names=F)
write.xlsx(c7, file="output_ct.xlsx", sheetName="eou&form", append=T, row.names=F)
write.xlsx(c8, file="output_ct.xlsx", sheetName="n crop&pro", append=T, row.names=F)
write.xlsx(c9, file="output_ct.xlsx", sheetName="p.row crop&pro", append=T, row.names=F)
write.xlsx(c10, file="output_ct.xlsx", sheetName="n crop&con", append=T, row.names=F)
write.xlsx(c11, file="output_ct.xlsx", sheetName="p.row crop&con", append=T, row.names=F)





#ook nul results in ppt zetten

#clustering expliciet stappen in methode 
#+ moet duidelijk gezegd worden dat ervaring niet enkel op eigen teeltcategorie slaat

#engelse versie: 
#target: versterker/Plant Strengtheners? fungus? 
#teelt cluster: akkerbouw (agricultur bulk / non horticulture)? 
#info bron: proefstation/research stations?