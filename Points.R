#Giancarlo Carino
#Practice using tidyverse package
#Basketball analysis 2018 season

library(tidyverse)

bk18perg <- read_csv("RawData/bk18perGstats.csv")

glimpse(bk18perg)
#rename column
colnames(bk18perg)[30] <- "PPG"

#Minutes played vs FG% by Position filtered by Minutes played > 20 and Field Goal % > 45
minPLUSfgp <- filter(bk18perg, MP > 20 & `FG%` > .45)
minPLUSfgp %>%
  ggplot(aes(x = MP, y = `FG%`)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_line(mapping = aes(color = Pos)) +
  ggtitle("Minutes per Game > 20 vs FG% > .45")

#Points per game vs Fg% filtered by > 12 PPG and FG% > 42%
ppgVSfgp <- filter(bk18perg, PPG > 12 & `FG%` > .42)
ppgVSfgp %>%
  ggplot(aes(x = PPG, y = `FG%`)) +
  geom_point(mapping = aes(color = Pos)) +
  geom_smooth(mapping = aes(color = Pos), se = FALSE) +
  facet_wrap(~Pos, nrow = 5) +
  ggtitle("Points per game > 12 vs FG% > .42")

#List players from SG position and Field goal attempts greater than the average Field Goal attempts
mean(bk18perg$FG)
mean(bk18perg$FGA)
bk18perg %>%
  filter(Pos == "SG" & FGA > mean(FGA))

