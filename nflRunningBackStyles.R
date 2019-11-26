setwd("~/Documents/GitHub/nflRunningBacks")
nfl.data <- read.csv("data/train.csv",stringsAsFactors = FALSE)

## Age/Height/Weight
## DisplayName, Yardline, Quarter, Down, Distance, FieldPosition,OffenseFormation,OffensePersonnel, DefendersInTheBox, DefensePersonnel,PlayDirection, PlayerHeight, PLayerWeight,Position,Turf,

## 12,15,16,19,20,21,25,26,27,28,29,30,31,32,33,34,35,37,44

data <- nfl.data[,c(12,15,16,19,20,21,25,26,27,28,29,30,31,32,33,34,35,37,44)]

data.rb <- data[data$Position=="RB",]

## want to group data by downs so each RB will have 4 entries
