require(dplyr)
require(ggplot2)
require(corrplot)
require(caret)
require(factoextra)
require(rpart)
setwd("~/Documents/GitHub/nflRunningBacks")

run <- function(){
nfl.data <- read.csv("data/train.csv",stringsAsFactors = FALSE)


data <- nfl.data[,c(12,15,16,19,20,21,25,26,27,28,29,30,31,32,33,34,35,37,44,2,1,11)]

data.rb <- data[data$Position=="RB",]



## reorder columns

 data.rb <- data.rb[, c(22,1,15,16,17,18,21,20,19,3,4,6,2,5,7,8,9,10,11,12,13,14)]

 data.rb <- data.rb %>%
    group_by(NflId) %>%
    arrange(PlayId) %>%
    mutate(prePlayYards=lag(Yards),
           cumYards=cumsum(Yards))
 
 
 rb.attr <- data.rb[,c(1:6)]
 rb.gm.data <- data.rb[,c(1,7:24)]
 
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
 rb.gm.data <- rb.gm.data[!is.na(rb.gm.data$DefendersInTheBox),]
 
 rb.gm.data <- rb.gm.data[,-c(15,16)]
 
 #rm(data,nfl.data)
 
 agg.data.rb <<- unique(rb.gm.data) %>%
                           group_by(NflId,GameId,PlayId, Quarter,Down) %>%
                           transmute(avgDistancePerDefenders = Distance/DefendersInTheBox,
                                     avgYardsPerDefenders = Yards/DefendersInTheBox,
                                     prePlayYards = prePlayYards, 
                                     cumYards = cumYards,
                                     avgYards = mean(Yards), 
                                     handoffTime = handoffTime, 
                                     Distance = Distance,
                                     avgDistance=mean(Distance),
                                     DefendersInTheBox = DefendersInTheBox,
                                     avgDefendersInTheBox = mean(DefendersInTheBox),
                                     Yards=Yards,
                                     goodRun = ifelse(Yards > 5,as.numeric(1),as.numeric(0)))
 
 agg.data.rb$prePlayYards[is.na(agg.data.rb$prePlayYards)] <- 0
 saveRDS(agg.data.rb,"rbData.rds")
}

bypass <- function(){
   agg.data.rb <<- readRDS("rbData.rds")
}
 ## graphs
 plots <- function(){
 g <<- ggplot(data=agg.data.rb) 
 }
 
 more.plots <- function(){
 # plots
 
 g + geom_boxplot(aes(round(avgDefendersInTheBox),avgYards)) + facet_grid(.~Down)
 g + geom_density(aes(avgYards)) + facet_grid(Quarter~Down)
 g + geom_jitter(aes(y=(avgYards),x=avgDefendersInTheBox))+facet_grid(Quarter~Down)
 
 g + geom_boxplot(aes(y=Distance,x=factor(Down)))
 g + geom_boxplot(aes(x=DefendersInTheBox,y=Distance))
 g + geom_boxplot(aes(y=handoffTime,x=factor(Down)))
 g + geom_jitter(aes(y=prePlayYards,x=DefendersInTheBox))
 g + geom_jitter(aes(y=handoffTime,x=DefendersInTheBox))
 
 g + geom_boxplot(aes(y=DefendersInTheBox, x=factor(goodRun)))
 g + geom_boxplot(aes(y=DefendersInTheBox, x=factor(Quarter)))
 g + geom_boxplot(aes(y=DefendersInTheBox, x=factor(Down)))
 }
 
 cor.plot <- function(){
## want to group data by downs so each RB will have 4 entries
 png(file="corr.png", res=300, width=4500, height=4500)
 corr <- cor(agg.data.rb[,c(-1,-2,-3)],method = "spearman",use="pairwise.complete.obs")
 corrplot(corr,method="color" ,addCoef.col="black",number.digits=2,number.cex = 1)
 dev.off()

 }
 
 createLinearModel <- function(){
 ##  Need to Split the data up
 
 train.index <- caret::createDataPartition(agg.data.rb$goodRun,p=.7,list=FALSE)
 train <- agg.data.rb[train.index,]
 test <- agg.data.rb[-train.index,]
 
### working on everything below this comment

 
 
 ## linear model
 linear.model2 <<- lm(Yards~DefendersInTheBox+Distance+cumYards+Down+Quarter+prePlayYards+handoffTime, data=train)
 linear.model <<- lm(Yards~DefendersInTheBox+Distance+cumYards+handoffTime, data=train)
 pred <- predict(linear.model,test)
 pred2 <- predict(linear.model2,test)
 actual_preds <<-data.frame(cbind(actuals=test$Yards,predicted=pred))
 actual_preds2 <<-data.frame(cbind(actuals=test$Yards,predicted=pred2))
 min_max_accuracy <<- mean(apply(actual_preds, 1, min) / apply(actual_preds, 1, max)) 
 min_max_accuracy2 <<- mean(apply(actual_preds2, 1, min) / apply(actual_preds2, 1, max)) 
 }
 
