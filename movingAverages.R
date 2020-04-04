library(TTR)
library(reshape2)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyquant)

# EMA: lambda*valueToday + 1-lambda * EWMABefore
# lambda = 2/n+1

### EWMA - Team ###

# Read Data
df <- read.csv("catapultValues.csv")
df$X <- NULL

# Team EWMA by Summation of Player Load Using TTR Package
team <- df %>% group_by(Date) %>% summarise(playerLoad = sum(playerLoad)) %>% 
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28))

# Get All Players ACWR into DF
df$Name <- as.character(df$Name)
df$Date <- as.Date(df$Date)

allPlayers <- data.frame()

for (athlete in levels(df$Name)) {
  if (as.numeric(df %>% filter(Name==athlete) %>% tally() < 28)) {
    next
  }
  
  else({
    player <- df %>% filter(Name == athlete) %>% 
      transmute(Name = athlete, Date = as.Date(Date), Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28), ACWR = Acute/Chronic) %>%
      mutate_if(is.numeric, round, 2)
    allPlayers <- rbind(player, allPlayers)
  })
  
}

# Make a DF with Text Based on ACWR Status
msgs <- allPlayers %>% 
  mutate(warningMessage = case_when(ACWR < 0.8 ~ paste(Name, "has a low ACWR on", Date), 
                                    ACWR > 1.2 ~ paste(Name, "has a high ACWR on", Date))) %>%
  mutate(Warning = case_when(ACWR < 0.8 ~ "Low",
                             ACWR > 1.2 ~ "High"))

# Return List of Warning Text
msgs %>% filter(!is.na(warning)) %>% select(warning) %>% as.list()

# Turn DF Long to Plot in ggplot2
teamLong <- team %>% melt() %>% transmute(Date= as.Date(Date), Variable = variable, Value = value)

# Plot
g <- ggplot(teamLong, aes(Date, Value, color = Variable)) + geom_line(group = 1, alpha = 0.9) + 
  scale_color_manual(name = "", values = c("black", "#1b9e77", "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g) #%>% api_create(filename = "Acute vs. Chronic Player Load")

# Easier - Using Tidyqant
# Can't Use Plotly with geom_ma so Above Would Need to be Used

team$Date <- as.Date(team$Date)
ggplot(team, aes(Date, playerLoad)) + geom_line(aes(color = "Values")) + 
  geom_ma(aes(color = "Acute"), ma_fun = EMA, n = 7, show.legend = TRUE, linetype = 1, size = 1) + 
  geom_ma(aes(color = "Chronic"), ma_fun = EMA, n = 28, show.legend = TRUE, linetype = 1, size = 1) +
  scale_color_manual(name = "", breaks = c("Values", "Acute", "Chronic"), 
                     values = c("Values" = "black", "Acute" = "#1b9e77", "Chronic" = "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))

### EWMA - Player ###
player <- df %>% filter(Name == Name[2]) %>% select(Date, playerLoad) %>%
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28))

playerLong <- player %>% melt() %>% transmute(Date= as.Date(Date), Variable = variable, Value = value)

# Plot
g2 <- ggplot(playerLong, aes(Date, Value, color = Variable)) + geom_line(group = 1, alpha = 0.9) + 
  scale_color_manual(name = "", values = c("black", "#1b9e77", "#7570b3")) + 
  ggtitle("Player Load Acute/Chronic") + xlab("") + ylab("Player Load") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g2) #%>% api_create(filename = "Acute vs. Chronic Player Load")

# ACWR w/o playerLoad
acwr <- df %>% filter(Name == Name[2]) %>% select(Date, playerLoad) %>%
  mutate(Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28), ACWR = round(Acute/Chronic, 3)) %>%
  transmute(Date = as.Date(Date), ACWR = ACWR) %>% filter(!is.na(ACWR))

# Plot ACWR Only
g3 <- ggplot(acwr, aes(Date, ACWR)) + geom_line(group=1) + scale_y_continuous(breaks = seq(0,2,0.5), limits = c(0,2)) + 
  geom_hline(yintercept = 0.8, color = "orange", alpha = 0.5) + geom_hline(yintercept = 1.2, color = "red", alpha = 0.5) + 
  ggtitle("Player Acute/Chronic Workload Ratio") + xlab("") + ylab("ACWR") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggplotly(g3) #%>% api_create(filename = "ACWR")
