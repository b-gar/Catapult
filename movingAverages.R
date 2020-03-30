library(TTR)
library(dplyr)
library(ggplot2)
library(tidyquant)
library(reshape2)

nEWMA <- function(values, n){
  lam <- 2/(n+1)
  temp <- c()
  
  for (i in 1:length(values)){
    if(i%%n==1){
      temp[1] <- values[1]
    }
    else{
      temp[i] <- (lam * values[i]) + ((1-lam)*temp[i-1])
    }
  }
  return(temp)
}
test$ewma <- nEWMA(test$playerLoadSum, 7)

df <- read.csv("catapultValues.csv")
df$X <- NULL

df %>% filter(Name == 'Logan Dobratz') %>% mutate(AcuteLoad = movavg(playerLoad, n = 7, type = "e"))

# EMA: lambda*valueToday + 1-lambda * EWMABefore
# lambda = 2/n+1

# Team EWMA #
test <- df %>% group_by(Date) %>% summarise(playerLoadSum = sum(playerLoad))
test$sma <- SMA(test$playerLoadSum, 7)
test$acute <- EMA(test$playerLoadSum, 7)
test$chronic <- EMA(test$playerLoadSum, 28)

testLong <- melt(test)
testLong$Date <- as.Date(testLong$Date)
ggplot(testLong, aes(Date, value, color = variable)) + geom_line(group = 1)

# Easier - Using Tidyqant
test$Date <- as.Date(test$Date)
ggplot(test, aes(Date, playerLoadSum)) + geom_line() + geom_ma(ma_fun = EMA, n = 7, color = 'blue') + 
  geom_ma(ma_fun = SMA, n = 7, color = 'red')
