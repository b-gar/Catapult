library(TTR)
library(dplyr)
library(ggplot2)
library(plotly)

# EMA: lambda*valueToday + 1-lambda * EWMABefore
# lambda = 2/n+1

## EWMA by Team Sum ##

# Read Data
df <- read.csv("catapultValues.csv")
df$X <- NULL

# Team EWMA by Summation of Player Load Using TTR Package
test <- df %>% group_by(Date) %>% summarise(playerLoad = sum(playerLoad)) %>% 
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28))

# Turn DF Long to Plot in ggplot2
library(reshape2)

testLong <- test %>% melt() %>% transmute(Date= as.Date(Date), Variable = variable, Value = value)

# Plot
g <- ggplot(testLong, aes(Date, Value, color = Variable)) + geom_line(group = 1, alpha = 0.9) + 
  scale_color_manual(name = "", values = c("black", "#1b9e77", "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g) #%>% api_create(filename = "Acute vs. Chronic Player Load")

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

## EWMA for a Player ##
test <- df %>% filter(Name == Name[2]) %>% select(Date, playerLoad) %>%
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28))

testLong <- test %>% melt() %>% transmute(Date= as.Date(Date), Variable = variable, Value = value)

# Plot Including playerLoad
g <- ggplot(testLong, aes(Date, Value, color = Variable)) + geom_line(group = 1, alpha = 0.9) + 
  scale_color_manual(name = "", values = c("black", "#1b9e77", "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g) #%>% api_create(filename = "Acute vs. Chronic Player Load")

# ACWR w/o playerLoad
test <- df %>% filter(Name == Name[2]) %>% select(Date, playerLoad) %>%
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28), ACWR = Acute/Chronic) %>%
  transmute(Date = as.Date(Date), ACWR = ACWR)

# Plot ACWR Only
g <- ggplot(test, aes(Date, ACWR)) + geom_line(group=1) + scale_y_continuous(breaks = seq(0,2,0.5), limits = c(0,2)) +
  ggtitle("Player Acute/Chronic Workload Ratio") + xlab("") + ylab("ACWR") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g) #%>% api_create(filename = "ACWR")
