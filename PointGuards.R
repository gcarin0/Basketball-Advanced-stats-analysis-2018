#Giancarlo Carino
#Basketball 2018 Analysis
#Point Guards

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
pointGuards <- subset(bk18perg, Pos == "PG")

#Remove Pos variable (all the same)
pointGuards$Pos <- NULL

#Find duplicates and store
rmDup <- which(duplicated(pointGuards$Player))

#Update dataframe without duplicate Players
pointGuards <- pointGuards[-rmDup,]

#Check atomic data types for variables
glimpse(pointGuards)

#Remove NA values
pointGuards <- na.omit(pointGuards)

#Test the relationship between Age and AST
#Scatterplot Age(Y) v AST(X)
ggplot(data = pointGuards, mapping = aes(x = Age, y = AST)) +
  geom_point(mapping = aes(color = Tm))

#Create Fantasy value variable based on specific stats
pointGuards$FV <- pointGuards$STL*.2 + pointGuards$PPG*.3 + pointGuards$TRB*.1 - pointGuards$TOV*.05 + pointGuards$AST*1.5 + pointGuards$`3P`*.2

#Generate natural log variables for linear regression
pointGuards$ln2P <- log(pointGuards$`2P`)
pointGuards$lnDRB <- log(pointGuards$DRB)
pointGuards$lnPF <- log(pointGuards$PF)
pointGuards$lnFT <- log(pointGuards$FT)
pointGuards$lnFG <- log(pointGuards$FG)
pointGuards$lneFG <- log(pointGuards$`eFG%`)

#Remove rows with Inf values (non-significant players)
rmINF <- which(is.infinite(pointGuards$lnDRB))
pointGuards <- pointGuards[-rmINF,]
rmINF <- which(is.infinite(pointGuards$lnPF))
pointGuards <- pointGuards[-rmINF,]

#Lin-Log Regression
pglreg <- lm(pointGuards$FV ~ pointGuards$ln2P + pointGuards$lnDRB + pointGuards$lnPF + pointGuards$lnFT + pointGuards$lnFG + pointGuards$lneFG, data = pointGuards)

