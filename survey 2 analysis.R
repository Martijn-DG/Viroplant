rm(list=ls())

library(Hmisc)
library(tidyverse)

#load data
df_names <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/FD/IT/Data_FD_IT_num.csv',
                     n_max = 2, col_names = F) #reading in column names (both short and long)
df_values <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/FD/IT/Data_FD_IT_num.csv',
                      skip = 2, col_names = F) #reading in values
label(df_values) <- as.list(df_names[2,]) #assigning long names as labels
names(df_values) <- as.character(df_names[1,]) #assigning short names as headers
data_IT <- df_values
rm("df_names", "df_values")

df_names <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/FD/IT/Data_FD_IT_text.csv',
                      n_max = 2, col_names = F) #reading in column names (both short and long)
df_values <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q int/FD/IT/Data_FD_IT_text.csv',
                       skip = 2, col_names = F) #reading in values
label(df_values) <- as.list(df_names[2,]) #assigning long names as labels
names(df_values) <- as.character(df_names[1,]) #assigning short names as headers
data_text_IT <- df_values
rm("df_names", "df_values")

df_names <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/long tomato q numeric.csv',
                      n_max = 2, col_names = F) #reading in column names (both short and long)
df_values <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/long tomato q numeric.csv',
                       skip = 2, col_names = F) #reading in values
label(df_values) <- as.list(df_names[2,]) #assigning long names as labels
names(df_values) <- as.character(df_names[1,]) #assigning short names as headers
data_BE <- df_values[-1,]
rm("df_names", "df_values")

df_names <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/long tomato q text.csv',
                      n_max = 2, col_names = F) #reading in column names (both short and long)
df_values <- read_csv2(file = 'C:/Users/mdegussem/OneDrive - ILVO/D5/long Q/results/long tomato q text.csv',
                       skip = 2, col_names = F) #reading in values
label(df_values) <- as.list(df_names[2,]) #assigning long names as labels
names(df_values) <- as.character(df_names[1,]) #assigning short names as headers
data_text_BE <- df_values[-1,]
rm("df_names", "df_values")

#combine num data
data_BE <- data_BE[ ,-c(1:3, 6:7, 9, 13, 15, 17:30, 33, 37:38, 41:75, 78, 82:87, 93:96)]
data_IT <- data_IT[ ,-c(8, 19, 20, 21:27, 36, 39)] #36=experience #8=cultivate #19=gmo why text #=why yes gmo
data_BE <- add_column(data_BE, "Q2.2+" = "", .after = 7)
data_BE <- add_column(data_BE, "Q6.3+" = "", .after = 16)
data_IT <- data_IT[ ,c(1:5, 8, 6:7, 10:11, 9, 12:25, 27, 26)]
x <- matrix(c(colnames(data_BE), colnames(data_IT)), ncol = 2)
colnames(data_BE) <- colnames(data_IT)
data_num <- rbind(data_BE, data_IT)
rm(data_BE, data_IT)

#combine text data
data_text_BE <- data_text_BE[ ,-c(1:3, 6:7, 9, 13, 15, 17:30, 33, 37:38, 41:75, 78, 82:87, 93:96)]
data_text_IT <- data_text_IT[ ,-c(8, 19, 20, 21:27, 36, 39)] #36=experience #8=cultivate #19=gmo why text #=why yes gmo
data_text_BE <- add_column(data_text_BE, "Q2.2+" = "", .after = 7)
data_text_BE <- add_column(data_text_BE, "Q6.3+" = "", .after = 16)
data_text_IT <- data_text_IT[ ,c(1:5, 8, 6:7, 10:11, 9, 12:25, 27, 26)]
colnames(data_text_BE) <- colnames(data_text_IT)
data_text <- rbind(data_text_BE, data_text_IT)
rm(data_text_BE, data_text_IT)

#combine num with text
data <- data_num
data[ ,c(c(6:11, 15:24))] <- data_text[ ,c(6:11, 15:24)]

#merge 4.2 into 4.3
data <- separate(data=data, col=Q4.2, into=c("Q4.2", "Q4.2B"), sep = ",", convert = FALSE)
data$Q4.3[(data$Q4.3=="")&!is.na(data$Q4.3)] <- data$Q4.2B[(data$Q4.3=="")&!is.na(data$Q4.3)]
data$Q4.2B <- NULL

data$Q4.3[data$Q4.2=="Prefer not to say"&!is.na(data$Q4.2)] <- "Prefer not to say"
data$Q4.2[data$Q4.2=="Prefer not to say"&!is.na(data$Q4.2)] <- NA

data$Q4.3 <- as.factor(data$Q4.3)
levels(data$Q4.3) <- c("Because I'm against the principle myself", "Because of the public opinion", "Because I'm against the principle myself", "Because of the public opinion", "Other", "Prefer not to say")

#setting data type
data$Sample <- as.factor(data$Sample)
data$Q3.4 <- as.factor(data$Q3.4)
data$Q3.1 <- as.factor(data$Q3.1)
data$Q3.1 <- relevel(data$Q3.1, 8) #Veneto
data$Q3.5 <- as.factor(data$Q3.5)
data$Q3.7 <- as.factor(data$Q3.7)
data$Q4.3 <- as.factor(data$Q4.3) 
data$Q8.1 <- as.factor(data$Q8.1)

data$Q3.8_1 <- as.numeric(data$Q3.8_1)
data$Q3.8_2 <- as.numeric(data$Q3.8_2)
data$Q3.8_3 <- as.numeric(data$Q3.8_3)

data$Q8.2 <- as.numeric(data$Q8.2)
data$Q8.4 <- as.numeric(data$Q8.4)
data$Q8.5 <- as.numeric(data$Q8.5)

data$Q3.5 <- as.logical(data$Q3.5=="Yes")
data$Q3.6 <- as.logical(data$Q3.6=="Yes")
data$Q4.2 <- as.logical(data$Q4.2=="Yes")
data$Q3.7B <- data$Q3.7
data$Q3.7B[data$Q3.7B=="No, but I may be willing to install one"] <- "Yes"
data$Q3.7B <- as.logical(data$Q3.7B=="Yes")

#cleaning up
data$ResponseId <- 1:length(data$ResponseId)
data <- data[data$UserLanguage!="EN", ]
data$UserLanguage[data$UserLanguage=="NL"] <- "BE"
colnames(data)[4] <- "Sample"
data <- data[data$Q1.2==1, ]
data$Q3.1[data$Q3.1=="Other"] <- "Netherlands"

data$Q8.1_3_TEXT <- NULL
data$Q7.1_5_TEXT <- NULL
data$Q7.2_13_TEXT <- NULL
data$Q3.2 <- NULL
#Amelia::missmap(data, col=c("blue", "red"), legend=T)

#split columns
long <- data
long1 <- separate_rows(long, Q7.1 , sep = ",", convert = FALSE)
long1$Q7.1 <- as.factor(long1$Q7.1)
levels(long1$Q7.1)[6:8] <- c("Industry reps (producers)", "Industry reps (producers)", "Other")

long2 <- separate_rows(long, Q7.2 , sep = ",", convert = FALSE)
long2$Q7.2 <- as.factor(long2$Q7.2)
levels(long2$Q7.2) <- c("IMP fit", "Effectiveness", "Regulation", "Time cost", "None", "Other", "Fin risk", "Soc/Eth", "Mode of action", "PricePerf", "How used", "Retail+auction", "Process sci", "Funding sci")

long3 <- separate_rows(long, Q7.3 , sep = ",", convert = FALSE)
long3$Q7.3 <- as.factor(long3$Q7.3)
levels(long3$Q7.3) <- c("less confident", "stop use", "trust gov", "welcome")
long3b <- long3
levels(long3b$Q7.3) <- c("2", "1", "3", "4") #fast track acceptance on a scale of 1:4
long3b$Q7.3 <- as.numeric(long3b$Q7.3)

long4 <- separate_rows(long, Q4.1 , sep = ", ", convert = FALSE)
long4$Q4.1 <- as.factor(long4$Q4.1)
levels(long4$Q4.1)[c(3, 5, 13)] <- c(NA, "Effectiveness", "Eou")
long4$Q4.1A <- long4$Q4.1
levels(long4$Q4.1A) <- c(T, F, T, F, NA, F, T, T, T, T) #acceptance of tech
long4$Q4.1A <- as.logical(long4$Q4.1A)
long4$Q4.1B <- long4$Q4.1
levels(long4$Q4.1B) <- c("Practical", "Buisiness", "Buisiness", "Practical", "Ethical", NA, "Buisiness", "None", "Buisiness", "Buisiness") #objection type
rm(long)

#discriptives
sort(table(data$Sample)) #country
sort(table(data$Q3.4)) #crop
sort(table(data$Q3.1)) #region
sort(table(data$Q3.5)) #infected

sort(table(data$Q3.6)) #heard of phage/virus

sort(table(long4$Q4.1)) #cons
sort(table(long4$Q4.1A)) #binary acceptance of tech
sort(table(long4$Q4.1B)) #con type

sort(table(data$Q3.7)) #fridge
sort(table(data$Q3.7B))#binary fridge

sort(table(data$Q4.2)) #gmo
sort(table(data$Q4.3)) #gmo+

sort(table(long1$Q7.1)) #info
sort(table(long2$Q7.2)) #needed

sort(table(long3$Q7.3)) #fast track
mean(long3b$Q7.3, na.rm = T) #con-pro 1-4

mean(data$Q3.8_1, na.rm = T) #confidence in control
mean(data$Q3.8_2, na.rm = T) #problem will grow
mean(data$Q3.8_3, na.rm = T) #looking for alt

sort(table(data$Q8.1)) #gender
mean(data$Q8.2, na.rm = T) #age
mean(data$Q8.4, na.rm = T) #education
mean(data$Q8.5, na.rm = T) #ses


#stats
#cons x sample
fit <- lm(as.logical(Sample=="BE")~Q4.1, long4)
summary(fit) #more eou and need in BE, more health in IT

#acceptance x infected
fit <- lm(Q4.1A~Q3.5, long4)
summary(fit)

#acceptance x statements
fit <- lm(Q4.1A~Q3.8_1+Q3.8_2+Q3.8_3, long4)
summary(fit)

#con type x socioeconomic
fit <- lm(Q4.1A~Q8.1+Q8.2+Q8.4+Q8.5, long4)
summary(fit)

#heard of, fridge, gmo x sample
fit <- lm(as.logical(Sample=="BE")~Q3.6+Q3.7+Q4.2, data) #estimate = chance of BE
summary(fit) #more heard of it in IT, more willing to install fridge in BE
gmodels::CrossTable(data$Sample, data$Q3.6, prop.t=F)

#info, needed, fast track x Sample
fit <- lm(as.logical(Sample=="BE")~Q7.1, long1)
summary(fit) #less research stations in BE (obviously IT exclusive options less in BE)
fit <- lm(as.logical(Sample=="BE")~Q7.2, long2)
summary(fit) #more retail/auction and financial risk in BE
fit <- lm(as.logical(Sample=="BE")~Q7.3, long3)
summary(fit)

#infected x sample, region
fit <- lm(Q3.5~Sample, data)
summary(fit) #more infections in IT
fit <- lm(Q3.5~Q3.1, data[data$Sample=="IT", ]) #intercept = veneto
summary(fit) #more infections in Apulia

#sample x statements ; IT situation is wors (less conf, more growth, more looking)
fit <- lm(Q3.8_1~Sample, data) #confidence in control
summary(fit)
fit <- lm(Q3.8_2~Sample, data) #problem will grow
summary(fit)
fit <- lm(Q3.8_3~Sample, data) #looking for alt
summary(fit)

#crop x statements
fit <- lm(Q3.8_1+Q3.8_2+Q3.8_3~Q3.4, data[data$Sample=="IT", ]) #conf
summary(fit) #intercept = both

#crop x infected
fit <- lm(Q3.5~Q3.4, data[data$Sample=="IT", ]) #conf
summary(fit) #more infections if people do both

#infected x statements
fit <- lm(Q3.5~Q3.8_1+Q3.8_2+Q3.8_3+Sample, data) #conf growth alts
summary(fit) #more looking for alts if infected
fit <- lm(Q3.5~Q3.8_1*Sample+Q3.8_2*Sample+Q3.8_3*Sample, data) #conf growth alts
summary(fit) #especially in IT (+if in fected in IT, more problem growth expected)

#fast track x infected, age, edu
fit <- lm(Q7.3~Q3.5+Q8.4+Q8.2, long3b)
summary(fit)

#fast track x gmo
fit <- lm(Q7.3~Q4.2, long3b)
summary(fit) #overlap between pro fast track and pro gmo people

#age x gmo, fridge
fit <- lm(Q8.2~Q4.2+Q3.7, data)
summary(fit) #pro gmo and willing to install are younger

#edu x gmo, fridge, heard of
fit <- lm(Q8.4~Q4.2+Q3.7+Q3.6, data) #willing to install are higher Edu
summary(fit)

#infected x gmo, fridge
fit <- lm(Q3.5~Q4.2+Q3.7B, data)
summary(fit)

#fridge x socio economics
fit <- lm(Q3.7B~Q8.2+Q8.4+Q8.5, data) #age edu ses
summary(fit)

#gender x info needed
fit <- lm(as.logical(Q8.1=="Male")~Q7.2, long2)
summary(fit)


#just for italy
#36=experience #8=cultivate #19=gmo why text #=why yes gmo
#cultivate x statements
#cultivate x gmo
#cultivate x 4.1

#need some extra analysis of the bigger BE data set
#Q6.1 - Which of the biocontrol technologies mentioned would you prefer?
#Q3.5 - Which of the following statements completes this sentence the best for you: "When a product promises a certain effectiveness..."
#Q6.4 - The new products can be very specific and disease causing bacteria can differ between farms. A service could produce a precision product for each farm but first an analysis of the present bacteria would be required.
#Q14.1 - Do you use any measure(s) to prevent and/or control crazy roots?
#Q14.2 - Which of these do you currently use?
#Q14.3/4 - cost/benifit (they have no idea)
#Q14.5 - Do you use hydrogen peroxide for any other purposes except control of crazy roots? If so, what do you use it for or against what other diseases do you use it?

#analyse with lm, if not possible try:
#agregate over users (freq) and test for differences in frequency between countries with chisq

#https://www.researchgate.net/post/Comparing_two_groups_of_percentages-is_a_t-test_ok
#http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r