#Giancarlo Carino
#Basketball 2018 Analysis
#Centers

#Load libraries
library(tidyverse)
library(stringr)

#Import CSV
bk18perg <- read_csv("RawData/bk18perGstats.csv")

#Remove rank from data frame
bk18perg$Rk <- NULL

#Rename points per game variable
colnames(bk18perg)[29] <- "PPG"

#Remove after slash
bk18perg$Player <- sub("\\\\.*", "", bk18perg$Player)

#Create new tibble of just Point Guards
centers <- subset(bk18perg, Pos == "C")

#Remove Pos variable (all the same)
centers$Pos <- NULL

#Remove irrelevent variables for centers
centers$`3P%` <- NULL
centers$AST <- NULL 

#Find duplicates and store
rmDup <- which(duplicated(centers$Player))

#Update dataframe without duplicate Players
centers <- centers[-rmDup,]

glimpse(centers)

#Remove NA values
centers <- na.omit(centers)

#Create fantasy value for centers
centers$FV <- centers$`FG%`*.15 + centers$`3P`*.025 + centers$PPG*.2 + centers$`FT%`*.05 + centers$TRB*.25 - centers$TOV*.025 + centers$BLK*.25 + centers$STL*.05

#Check for distribution of PPG
ggplot(data = centers, mapping = aes(x = PPG)) +
  geom_histogram(binwidth = .5)

#Check distribution of TRB
ggplot(data = centers, aes(TRB)) +
  geom_histogram(binwidth = .5)

#Log transform variables to put in terms of % change
centers$lnFG <- log(centers$FG)
centers$ln2P <- log(centers$`2P`)
centers$lnDRB <- log(centers$DRB)
centers$lnORB <- log(centers$ORB)
centers$lnPF <- log(centers$PF)
centers$lnMP <- log(centers$MP)
centers$lneFG <- log(centers$`eFG%`)

#Remove rows with Inf values (non-significant players)
rmINF <- which(is.infinite(centers$lnORB))
centers <- centers[-rmINF,]

#Lin-Log Regression
centersLinLog <- lm(centers$FV ~ centers$lnFG + centers$ln2P + centers$lnDRB + centers$lnORB + centers$lnPF + centers$lnMP + centers$lneFG, data = centers)

summary(centersLinLog)
