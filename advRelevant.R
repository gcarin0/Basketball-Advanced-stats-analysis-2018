#Giancarlo Carino
#Basketball advanced stats

#load libraries
library(tidyverse)
library(stringr)

#Load data
advbk18stats <- read_csv("RawData/advbk18stats.csv")

#Tidy the data

#Delete unnecessary columns
advbk18stats$X20 <- NULL
advbk18stats$X25 <- NULL
advbk18stats$Rk <- NULL
advbk18stats$WS <- NULL
advbk18stats$`WS/48` <- NULL

#Remove after slash
advbk18stats$Player <- sub("\\\\.*", "", advbk18stats$Player)

#Find duplicates and store
rmDup <- which(duplicated(advbk18stats$Player))

#Set Pos as factor variable
advbk18stats$Pos <- as.factor(advbk18stats$Pos)

#Update dataframe without duplicate Players
advbk18stats <- advbk18stats[-rmDup,]

glimpse(advbk18stats)

#Create a separate tibble for Games > 50
relevantAdv <- subset(advbk18stats, G > 50)

#Basic statistics MP
summary(relevantAdv$MP)

#Box plot by POS of PER
relevantAdv %>%
  ggplot(aes(Pos, PER)) +
  geom_boxplot()

#Outliers in PF
relevantAdv %>%
  filter(Pos == "PF" & PER > 20)

#Outliers in PG
relevantAdv %>%
  filter(Pos == "PG" & PER > 20)

#Outliers in SG
relevantAdv %>%
  filter(Pos == "SG" & PER > 22)

#Analyze usage rate
relevantAdv %>%
  ggplot(aes(`USG%`)) +
  geom_histogram(aes(y = ..density..), fill = "black") +
  geom_density(fill = "light blue", alpha = .2, color = "light blue") +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

scatter_

#Box plot of Usage rate by Pos
relevantAdv %>%
  ggplot(aes(Pos, `USG%`)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))

#Outliers in PG
relevantAdv %>%
  filter(Pos == "PG" & `USG%` > 32)

#Create Fantasy value of adv stats (Punt: Asts/TO, Strength: PTS, 3s, RBD, STL, BLK)
relevantAdv$fv <- relevantAdv$`3PAr`*.15 + relevantAdv$`USG%`*.15 + relevantAdv$`TRB%`*.15 + relevantAdv$`STL%`*.15 + relevantAdv$`BLK%`*.15 + relevantAdv$FTr*.1 - relevantAdv$`AST%`*.05 + relevantAdv$`TOV%`*.05 + relevantAdv$G*.025 + relevantAdv$MP*.025

#Box plot of fantasy by Pos
relevantAdv %>%
  ggplot(aes(Pos, fv)) +
  geom_boxplot() +
  theme(panel.background = element_rect(fill = "gray95"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "gray95"))
