require(dplyr)
require(ggplot2)
require(corrplot)
setwd("~/Documents/GitHub/nflRunningBacks")
nfl.data <- read.csv("data/train.csv",stringsAsFactors = FALSE)


data <- nfl.data[,c(12,15,16,19,20,21,25,26,27,28,29,30,31,32,33,34,35,37,44,2,1,11)]

data.rb <- data[data$Position=="RB",]

## reorder columns

 data.rb <- data.rb[, c(22,1,15,16,17,18,21,20,19,3,4,6,2,5,7,8,9,10,11,12,13,14)]

 rb.attr <- data.rb[,c(1:6)]
 rb.gm.data <- data.rb[,c(1,7:22)]
 
 splitTime <- function(x){
   
   dHour <- as.numeric(substr(x,1,2))
   dMin <- as.numeric(substr(x,4,5))
   dSec <- as.numeric(substr(x,7,8))
   
   dTime <- 3600*dHour + 60*dMin + dSec
   
   dTime
   
 }
 
 handoffTime <- function(x,y){
   
   ans <- splitTime(x) - splitTime(y)
 
   if(ans<0)  ans <- ans + (24*60*60)
   
   ans
 }
 
 ## calculate delayed handooffs
 rb.gm.data$TimeHandoff <- substr(rb.gm.data$TimeHandoff,12,19)
 rb.gm.data$TimeSnap<- substr(rb.gm.data$TimeSnap,12,19)
 rb.gm.data$handoffTime <- handoffTime(rb.gm.data$TimeHandoff,rb.gm.data$TimeSnap)
 
 rb.gm.data <- rb.gm.data[rb.gm.data$handoffTime>=0,]

 ## Cleaning - removing N/A's - Will consider something else in the future
 rb.gm.data[!is.na(rb.gm.data$DefendersInTheBox),]
 
 rb.gm.data <- rb.gm.data[,-c(15,16)]
 
 agg.data.rb <- unique(rb.gm.data) %>%
                      group_by(NflId,GameId,Quarter,Down) %>%
                           mutate(firstHalf = if(Quarter<=2){as.numeric(1.0)}else{as.numeric(0)},
                                  playLeft = if(PlayDirection=="left"){as.numeric(1.0)}else{as.numeric(0)},
                                  firstDownRun = if(Distance>Yards){as.numeric(1.0)}else{as.numeric(0)},
                                  grassField = if(grepl("rass",Turf)){as.numeric(1.0)}else{as.numeric(0)},
                                  goodRun = if(Yards > 5){as.numeric(1)}else{as.numeric(0)})%>%
                           transmute(avgFieldType = mean(grassField),
                                     avgFirstHalf = mean(firstHalf),
                                     avgPlayLeft = mean(playLeft),
                                     avgFirstDownRun = mean(firstDownRun),
                                     avgDistancePerDefenders = Distance/DefendersInTheBox,
                                     avgYardsPerDefenders = Yards/DefendersInTheBox,
                                     totalYards = sum(Yards), 
                                     avgYards = mean(Yards), 
                                     avgHandOfTime = mean(handoffTime), 
                                     avgDistance=mean(Distance),
                                     avgDefendersInTheBox = mean(DefendersInTheBox),
                                     goodRun = goodRun)
  
## want to group data by downs so each RB will have 4 entries
corrplot(cor(agg.data.rb[,c(-1,-2)],method = "spearman",use="pairwise.complete.obs") )
 


### working on everything below this comment

g <- ggplot(data=agg.data.rb) 
 
# plots
g + geom_boxplot(aes(round(avgDefendersInTheBox),avgYards)) + facet_grid(.~Down)
g + geom_density(aes(avgYards), bins=11) + facet_grid(.~PlayDirection)


g + geom_line(aes(y=(avgYards),x=avgDefendersInTheBox))+facet_grid(Quarter~Down)



