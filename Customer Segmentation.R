library(visdat)
library(tidyr)
library(ggplot2)
library(psych)
library(plotly)
library(dplyr)
library(data.table)
library(clustMixType)
library(tidyverse)
library(cluster)
library(factoextra)
library(dplyr)

# Import Data
cust_seg <- read.csv(file.choose())
cust_seg = subset(cust_seg, select = -c(ID, Var_1) )
cust_seg[cust_seg==""] <- NA
cust_seg[1:8,]
head(cust_seg)


# Pre-processing Data
cust_seg$Gender <- as.factor(cust_seg$Gender)
cust_seg$Ever_Married <- as.factor(cust_seg$Ever_Married)
cust_seg$Age <- as.numeric(cust_seg$Age)
cust_seg$Graduated <- as.factor(cust_seg$Graduated)
cust_seg$Profession <- as.factor(cust_seg$Profession)
cust_seg$Work_Experience <- as.numeric(cust_seg$Work_Experience)
cust_seg$Spending_Score <- as.factor(cust_seg$Spending_Score)
cust_seg$Family_Size <- as.numeric(cust_seg$Family_Size)
str(cust_seg)
dim(cust_seg)

## check the missing value
sapply(cust_seg, function(x) sum(is.na(x)))
vis_miss(cust_seg)

## Handle the missing value
cust_seg$Work_Experience[is.na(cust_seg$Work_Experience)] = 0
m<-mean(cust_seg$Work_Experience)
cust_seg$Work_Experience[cust_seg$Work_Experience==NA]<-m

cust_seg$Family_Size[is.na(cust_seg$Family_Size)] = 0
fm<-mean(cust_seg$Family_Size)
cust_seg$Family_Size[cust_seg$Family_Size==NA]<-fm

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(cust_seg$Ever_Married)
find_mode(cust_seg$Graduated)
find_mode(cust_seg$Profession)
cust_seg=cust_seg %>% replace_na(list(Ever_Married = "Yes"))
cust_seg=cust_seg %>% replace_na(list(Graduated = "Yes"))
cust_seg=cust_seg %>% replace_na(list(Profession = "Artist"))
sapply(cust_seg, function(x) sum(is.na(x)))

## check the outlier of data
num_cols <- unlist(lapply(cust_seg, is.numeric)) #Memilih kolom bertipe numerik
df_num <- cust_seg[ , num_cols]  
boxplot(df_num)

## EDA
par(mfrow=c(2,2),mar=c(3,4,1,1))
hist(cust_seg$Age, main = "Hist of Age")
hist(cust_seg$Work_Experience, main = "Hist of Work Experience")
hist(cust_seg$Family_Size, main = "Hist of Family SIze")

par(mfrow=c(3,2),mar=c(3,4,1,1))
barplot(table(cust_seg$Gender), ylab = "Frequency", xlab = "Gender")
barplot(table(cust_seg$Ever_Married), ylab = "Frequency", xlab = "Ever Married")
barplot(table(cust_seg$Graduated), ylab = "Frequency", xlab = "Graduated")
barplot(table(cust_seg$Profession), ylab = "Frequency", xlab = "Profession")
barplot(table(cust_seg$Spending_Score), ylab = "Frequency", xlab = "Spending Score")


## Standardize the numeric variable
cust_seg <- cust_seg %>% mutate_at(c('Age', 
                                     'Work_Experience', 
                                     'Family_Size'), ~(scale(.) %>% as.vector))
pairs.panels(df_num, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


# k-prototypes clustering
head(cust_seg)
set.seed(123)
clust3 <- kproto(cust_seg, k = 3)
clust3
clust3$iter
clust3$centers
par(mfrow=c(2,2),mar=c(3,4,1,1))
clprofiles(clust3, cust_seg)
library(writexl)
write_xlsx(clust3$centers,"D:/Self Study/R Studio/center cluster.xlsx")
clustofclust3 <- data.frame(clust3$cluster)
write_xlsx(clustofclust3,"D:/Self Study/R Studio/d.xlsx")
