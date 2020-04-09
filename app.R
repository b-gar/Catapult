library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(stringr)
library(plotly)
library(ggplot2)
library(TTR)

# Custom CSS
CSS <- ".shiny-output-error-fileUpload {
  color: black;
}

.nav-tabs-custom .nav-tabs li.active a{
  background-color: #F5F5F5;
  border-top-color: #003366;
  font-weight: bold;
  
}
.nav-tabs-custom .nav-tabs li.active {
  border-top-color: #003366;
  
}

.navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
  width:500px;
  }

#numGame {
  text-align: center;}
#numPractice {
  text-align: center;}"

# UI #
ui <- dashboardPage(skin = "black", title = "Catapult",
                    
        ## HEADER ##
        dashboardHeader(title = strong("Catapult"), dropdownMenuOutput("notificationLow"), dropdownMenuOutput("notificationHigh")),

        ## SIDEBAR ##
        dashboardSidebar(
            sidebarMenu(
                menuItem("File Upload", tabName = "tab1", icon = icon("file")),
                menuItem("Team", tabName = "tab2", icon = icon("users")),
                menuItem("Player", tabName = "tab3", icon = icon("user")),
                menuItem("GitHub", icon = icon("github"), href = "https://github.com/blg-uwm/Catapult", newtab = TRUE)
            )
        ),
                    
        ## BODY ##
        dashboardBody(inlineCSS(CSS),
            tabItems(
                
                ## TAB 1 - Import File ##
                tabItem(tabName = "tab1",
            
                    ## ROW 1 ##
                    fluidRow(
                        column(2,
                            img(src="logo.png", height = "100%", width = "100%")       
                        ),
                        box(title = "Upload Catapult File(s)", width = 3, background = "black", align = "center",
                            fileInput("files", "Select CSV File(s)", multiple = TRUE, accept = c(".csv")),
                            tags$h5("Missing Catapult data and want to test out the app?"),
                            actionButton("demoFiles", "Get Files", onclick = "window.open('https://panthers-my.sharepoint.com/:f:/g/personal/blgarski_uwm_edu/EvDzPKJryPNNvuPbTdKUX2kBGsYM1hZytX1Ok4Ms6Kh0PQ?e=9OxkLe', '_blank')")
                        ),
                        gradientBox(width = 2, title = "Overview", footer = 
                            h3(id = "numPractice", textOutput("numPractice")),
                            br(),
                            h3(id = "numGame", textOutput("numGame"))
                        ),
                        tabBox(width = 5, title = "Variable Summary",
                            tabPanel("Games",
                                verbatimTextOutput("gameSummary")
                            ),
                            tabPanel("Practices",
                                verbatimTextOutput("practiceSummary")
                            )
                        )
                    ),
                    br(),
                    br(),
                    
                    ## ROW 2 ##
                    fluidRow(
                        column(12,
                               withSpinner(DTOutput("table"), type = 7, color = "#FFCC00", size = 2) 
                        )
                    )
                ),
                
                ## TAB 2 - Team ##
                tabItem(tabName = "tab2",
                        
                      ## ROW 1 ##
                      fluidRow(
                        tabBox(width = 12,
                               tabPanel("Player Load",
                                        withSpinner(plotlyOutput("TeamLoadChrono", height = "32vh"), type = 7, color = "#FFCC00", size = 2)
                               ),
                               tabPanel("Max Velocity",
                                        withSpinner(plotlyOutput("TeamVelocityChrono", height = "32vh"), type = 7, color = "#FFCC00", size = 2)
                               )
                        )
                      ),
                      
                      ## ROW 2 ##
                      fluidRow(
                        tabBox(width = 12,
                               tabPanel("Player Load",
                                        withSpinner(plotlyOutput("TeamLoadCode", height = "32vh"), type = 7, color = "#FFCC00", size = 2)
                               ),
                               tabPanel("Max Velocity",
                                        withSpinner(plotlyOutput("TeamVelocityCode", height = "32vh"), type = 7, color = "#FFCC00", size = 2)
                               )
                        )
                      )
                ),
                
                ## TAB 3 - Player ##
                tabItem(tabName = "tab3", useShinyjs(),
                     
                   ## ROW 1 ##
                   fluidRow(
                     column(3,
                            pickerInput("player", "Player", choices = NULL, options = pickerOptions(actionsBox = TRUE))
                     ),
                     column(3,
                            sliderInput("acwr", "ACWR Warnings", min = 0.6, max = 1.4, value = c(0.8, 1.2))
                     ),
                     column(1, style = "float: left;",
                            br(),
                       actionButton("reset", "Reset", width = "80%")
                     )
                   ),
                   
                   ## ROW 2 ##
                   fluidRow(
                     tabBox(width = 12,
                            tabPanel("Player Load",
                                     withSpinner(plotlyOutput("PlayerLoadChrono", height = "28vh"), type = 7, color = "#FFCC00", size = 2)
                            ),
                            tabPanel("Max Velocity",
                                     withSpinner(plotlyOutput("PlayerVelocityChrono", height = "28vh"), type = 7, color = "#FFCC00", size = 2)
                            ),
                            tabPanel("ACWR",
                                     withSpinner(plotlyOutput("PlayerEWMA", height = "28vh"), type = 7, color = "#FFCC00", size = 2)
                            )
                     )
                   ),
                   
                   ## ROW 3 ##
                   fluidRow(
                     tabBox(width = 12,
                            tabPanel("Player Load",
                                     withSpinner(plotlyOutput("PlayerLoadCode", height = "28vh"), type = 7, color = "#FFCC00", size = 2)
                            ),
                            tabPanel("Max Velocity",
                                     withSpinner(plotlyOutput("PlayerVelocityCode", height = "28vh"), type = 7, color = "#FFCC00", size = 2)
                            )
                     )
                   )
                )
            )
        )
)

# Server #
server <- function(input, output, session) {
  
    # Reactive expression to get DF from file input
    Data <- reactive({
      
      req(input$files)
        
        # Empty DF for Loop
        dfCombined <- data.frame()
        
        # Combine Files Inputted
        for (i in 1:length(input$files$name)) {
            
            df1 <- read.csv(input$files$datapath[i], skip = 9)
            
            # Get Date From File Name
            df1$date <- str_extract(readLines(input$files$datapath[i], n=1), "\\d+/\\d+/\\d+")
            df1$date <- as.Date(df1$date, "%m/%d/%Y")
            
            # Get Activity Type From File Name
            df1$activity <- str_extract(input$files$name[i], "practice|Practice|Activity|Game")
            df1$activity[df1$activity == "Activity"] <- "practice"
            df1$activity <- str_to_title(df1$activity)
            
            # Filter and Combine
            df1 <- df1 %>% filter(Period.Name == "Session") %>% select(1,4,7:9,16, date, activity)
            dfCombined <- rbind(df1, dfCombined)
            
        }
        
            # Add Game Day Coding
            dfCombined$nextdate <- lag(dfCombined$date)
            dfCombined$days_between <- dfCombined$nextdate - dfCombined$date 
            dfCombined$gdays <- NA
        
            for(i in 1:nrow(dfCombined)){
                if (dfCombined$activity[[i]] == "Game")
                    dfCombined$gdays[[i]] <- 0
                else {
                    if (i>1)
                        dfCombined$gdays[[i]] <- dfCombined$gdays[[i-1]] + dfCombined$days_between[[i]]
                    else dfCombined$gdays[[i]] <- NA
                
                }
            
            } 
        
            dfCombined$gcode <- ifelse(dfCombined$gdays > 0, paste0("G-", dfCombined$gdays), "G")
        
            dfCombined <- arrange(dfCombined, date)
            dfCombined <- dfCombined %>% select(1:8,12)
            colnames(dfCombined) <- c("Name", "Position", "Duration", "Distance", "playerLoad", "maxVelocity", "Date", 
                                      "Activity", "gameCode")
            dfCombined <- dfCombined %>% filter(Distance != 0 | playerLoad != 0) %>% mutate_if(is.numeric, round, 2)
         
        return(dfCombined)
    })
    
    ## DF with All Player's ACWR ##
    dataACWR <- reactive({
      
      allPlayers <- data.frame()
      
      for (athlete in levels(Data()$Name)) {
        if (as.numeric(Data() %>% filter(Name==athlete) %>% tally() < 28)) {
          next
        }
        
        else({
          player <- Data() %>% filter(Name == athlete) %>% 
            transmute(Name = athlete, Date = as.Date(Date), Acute = EMA(playerLoad, 7), Chronic = EMA(playerLoad, 28), ACWR = Acute/Chronic) %>%
            mutate_if(is.numeric, round, 2)
          allPlayers <- rbind(player, allPlayers)
        })
        
      }
      if (nrow(allPlayers) > 0) {
        msgs <- allPlayers %>% 
          mutate(WarningMessage = case_when(ACWR < input$acwr[1] ~ paste(Name, "has a low ACWR of", ACWR, "on", Date), 
                                            ACWR > input$acwr[2] ~ paste(Name, "has a high ACWR of", ACWR, "on", Date)
                                            )
          ) %>%
          mutate(Warning = case_when(ACWR < input$acwr[1] ~ "Low", 
                                     ACWR > input$acwr[2] ~ "High"
                                    )
          ) %>% arrange(desc(Date))
      }
      else(msgs <- data.frame())
      
      return(msgs)
    })
    
    ## Notification Menu for Low ACWR ##
    output$notificationLow <- renderMenu({
      
      nmLow <- dataACWR() %>% filter(Warning == "Low")
      
      if (nrow(nmLow) > 0) {
        notifMessageL <- apply(nmLow, 1, function(row) {
          notificationItem(text = row[["WarningMessage"]], icon = icon("exclamation-triangle"), status = "warning")
        })
        dropdownMenu(type = "notifications", .list = notifMessageL, icon = icon("long-arrow-alt-down fa-2x"), badgeStatus = "warning")
      }
      else(
        dropdownMenu(notificationItem(text = "There are no low ACWR values", icon = icon("info")), badgeStatus = "info")
      )
      
      
    })
    
    ## Notification Menu for High ACWR ##
    output$notificationHigh <- renderMenu({
      
      nmHigh <- dataACWR() %>% filter(Warning == "High")
      if (nrow(nmHigh) > 0) {
        notifMessageH <- apply(nmHigh, 1, function(row) {
          notificationItem(text = row[["WarningMessage"]], icon = icon("exclamation-triangle"), status = "danger")
        })
        dropdownMenu(type = "notifications", .list = notifMessageH, icon = icon("long-arrow-alt-up fa-2x"), badgeStatus = "danger")
      }
      else(
        dropdownMenu(notificationItem(text = "There are no high ACWR values", icon = icon("info")), badgeStatus = "info")
      )
      
      
    })
    ## HOME SCREEN SUMMARY ##
    
    # Number of Games
    numGame <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Game") %>% group_by(Date) %>% n_distinct()
    })
    output$numGame <- renderText({
      validate(
        need(input$files != "", "Please upload csv file(s)"), errorClass = "fileUpload"
      )
        paste(numGame(), "Games")
    })
    
    # Number of Practices
    numPractice <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Practice") %>% group_by(Date) %>% n_distinct()
    })
    output$numPractice <- renderText({
        paste(numPractice(), "Practices")
    })
    
    # Game Summary
    output$gameSummary <- renderPrint({
        gs <- Data() %>% filter(Activity == "Game") %>% select(Distance, playerLoad, maxVelocity) 
        summary(gs)
    })
    
    # Practice Summary
    output$practiceSummary <- renderPrint({
        ps <- Data() %>% filter(Activity == "Practice") %>% select(Distance, playerLoad, maxVelocity)  
        summary(ps)
    })
    
    # Data Table Output
    output$table <- renderDT(Data(), extensions = c('Buttons', 'FixedHeader', 'Responsive'), rownames = FALSE, filter = 'top',
                             options = list(dom = 'Brtip', fixedHeader = TRUE, scroller = TRUE, bPaginate = FALSE, 
                                            buttons = c('csv', 'excel')))
    ## Team Tab ##
    
    # Plotly Average Load Over Time
    output$TeamLoadChrono <- renderPlotly({
        p1 <- Data() %>% select(playerLoad, Date, Activity, gameCode) %>% group_by(Date) %>% 
        mutate(averagePlayerLoad = round(mean(playerLoad), 2)) %>% distinct(Date, .keep_all = TRUE) %>% select(-playerLoad) %>%
        ggplot(aes(x = Date, y = averagePlayerLoad, group = 1, 
                   text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "averagePlayerLoad: ", averagePlayerLoad))) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Player Load Over Time")
      ggplotly(p1, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Average Max Velocity Over Time
    output$TeamVelocityChrono <- renderPlotly({
      p2 <- Data() %>% select(maxVelocity, Date, Activity, gameCode) %>% group_by(Date) %>% filter(maxVelocity < 20) %>%
        mutate(averageMaxVelocity = round(mean(maxVelocity), 2)) %>% distinct(Date, .keep_all = TRUE) %>% select(-maxVelocity) %>%
        ggplot(aes(x = Date, y = averageMaxVelocity, group = 1, 
                   text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "averageMaxVelocity: ", averageMaxVelocity))) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Max Velocity Over Time")
      ggplotly(p2, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Player Load by Game Code
    output$TeamLoadCode <- renderPlotly({
      p3 <- Data() %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
        mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>%
        ggplot(aes(x=gameCode, y=playerLoad)) + geom_jitter(width = 0.1, alpha = 0.4, size = 3, color = "#003366") +
        stat_summary(fun.y=mean, colour="#FFCC00", size = 2, geom="line", aes(group = 1, shape = "mean")) + theme_minimal() + 
        ggtitle("Player Load by Game Code") + scale_shape_manual("", values=c("Mean"="x"))
      ggplotly(p3) %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Average Max Velocity by Game Code
    output$TeamVelocityCode <- renderPlotly({
      p4 <- Data() %>% select(maxVelocity, Activity, gameCode) %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
        mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>%
        group_by(gameCode) %>% mutate(averageMaxVelocity = round(mean(maxVelocity), 2)) %>% select(-maxVelocity) %>% 
        ggplot(aes(x = gameCode, y = averageMaxVelocity, group = 1)) + geom_point(aes(color = Activity), size = 4) + geom_line() + 
        theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Max Velocity by Game Code")
      ggplotly(p4) %>% config(displayModeBar = FALSE)
    })
    
    ## Player Tab ##
    
    # Update Player Selector Input
    observe({
        updatePickerInput(session, inputId = "player", choices = Data() %>% mutate(Name = as.character(Name)) %>% distinct(Name))
    })
    
    # Reset ACWR Limits to Default
    observeEvent(input$reset, {
      reset("acwr")
    })
    
    # Plotly Player Load Over Time/Player
    output$PlayerLoadChrono <- renderPlotly({
      p5 <- Data() %>% select(Name, playerLoad, Date, Activity, gameCode) %>% filter(Name == input$player) %>%
        ggplot(aes(x = Date, y = playerLoad, group = 1, 
                   text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "playerLoad: ", playerLoad))) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Player Load Over Time")
      ggplotly(p5, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Player Max Velocity Over Time/Player
    output$PlayerVelocityChrono <- renderPlotly({
      p6 <- Data() %>% select(Name, maxVelocity, Date, Activity, gameCode) %>% filter(Name == input$player, maxVelocity < 20) %>%
        ggplot(aes(x = Date, y = maxVelocity, group = 1, 
                   text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "maxVelocity: ", maxVelocity))) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Max Velocity Over Time")
      ggplotly(p6, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Player EWMA Over Time/Player with Validation for chronic
    output$PlayerEWMA <- renderPlotly({
      validate(
        need(as.numeric(Data() %>% filter(Name == input$player) %>% tally()) > 28, 
             "This player does not have enough data for a chronic player load of 28-days")
      )
      
      p7 <- Data() %>% filter(Name == input$player) %>%
        mutate(Acute = round(EMA(playerLoad, 7), 2), Chronic = round(EMA(playerLoad, 28), 2), ACWR = round(Acute/Chronic, 2)) %>%
        filter(!is.na(Chronic)) %>%
        ggplot(aes(Date, ACWR, text = paste0("Date: ", Date, "\n", "Acute: ", Acute, "\n", "Chronic: ", Chronic, "\n", "ACWR: ", ACWR))) + 
        geom_line(group = 1) + scale_y_continuous(breaks = seq(0.5, 1.5, 0.5), limits = c(0.5, 1.5)) + 
        geom_hline(aes(yintercept = input$acwr[1]), color = "#d95f02", alpha = 0.5) + 
        geom_hline(aes(yintercept = input$acwr[2]), color = "#f03b20", alpha = 0.5) + ggtitle("ACWR Using Exponentially Weighted Moving Average") + 
        xlab("") + ylab("ACWR") + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
      ggplotly(p7, tooltip = "text") %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Average Player Load/gameCode
    output$PlayerLoadCode <- renderPlotly({
      p8 <- Data() %>% select(Name, playerLoad, Activity, gameCode) %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
        mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>% filter(Name == input$player) %>%
        group_by(gameCode) %>% mutate(averagePlayerLoad = round(mean(playerLoad), 2)) %>% select(-playerLoad) %>% 
        ggplot(aes(x = gameCode, y = averagePlayerLoad, group = 1)) + geom_point(aes(color = Activity), size = 4) + geom_line() + 
        theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Player Load by Game Code")
      ggplotly(p8) %>% config(displayModeBar = FALSE)
    })
    
    # Plotly Average Max Velocity/gameCode
    output$PlayerVelocityCode <- renderPlotly({
      p9 <- Data() %>% select(Name, maxVelocity, Activity, gameCode) %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
        mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>% 
        filter(Name == input$player, maxVelocity < 20) %>% group_by(gameCode) %>% 
        mutate(averageMaxVelocity = round(mean(maxVelocity), 2)) %>% 
        select(-maxVelocity) %>% ggplot(aes(x = gameCode, y = averageMaxVelocity, group = 1)) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() + 
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Max Velocity by Game Code")
      ggplotly(p9) %>% config(displayModeBar = FALSE)
    })
}
# Run the application 
shinyApp(ui = ui, server = server)