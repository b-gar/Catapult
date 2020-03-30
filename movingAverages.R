library(TTR)
library(dplyr)
library(ggplot2)
library(plotly)

# Read Data
df <- read.csv("catapultValues.csv")
df$X <- NULL

# Team EWMA by Summation of Player Load Using TTR Package
test <- df %>% group_by(Date) %>% summarise(playerLoad = sum(playerLoad)) %>% 
  mutate(Acute = EMA(test$playerLoad, 7), Chronic = EMA(test$playerLoad, 28))

# EMA: lambda*valueToday + 1-lambda * EWMABefore
# lambda = 2/n+1

# Turn DF Long to Plot in ggplot2
library(reshape2)

testLong <- melt(test)
testLong$Date <- as.Date(testLong$Date)
colnames(testLong) <- c("Date", "Variable", "Value")
g <- ggplot(testLong, aes(Date, Value, color = Variable)) + geom_line(group = 1, alpha = 0.9) + 
  scale_color_manual(name = "", values = c("black", "#1b9e77", "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g)

# Easier - Using Tidyqant
# Can't Use Plotly with geom_ma so Above Would Need to be Used
library(tidyquant)

test$Date <- as.Date(test$Date)
ggplot(test, aes(Date, playerLoad)) + geom_line(aes(color = "Values")) + 
  geom_ma(aes(color = "Acute"), ma_fun = EMA, n = 7, show.legend = TRUE, linetype = 1, size = 1) + 
  geom_ma(aes(color = "Chronic"), ma_fun = EMA, n = 28, show.legend = TRUE, linetype = 1, size = 1) +
  scale_color_manual(name = "", breaks = c("Values", "Acute", "Chronic"), 
                     values = c("Values" = "black", "Acute" = "#1b9e77", "Chronic" = "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
