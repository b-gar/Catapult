library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinycssloaders)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(dplyr)
library(data.table)
library(stringr)
library(plotly)
library(ggplot2)

# Specify CSS For Error Messsage
CSS <- ".shiny-output-error { visibility: hidden; }
.shiny-output-error:before {
    visibility: visible;
    content: 'Check that your uploaded file is formatted properly'; }
#numGame {
  text-align: center;}
#numPractice {
  text-align: center;}"

# UI #
ui <- dashboardPage(skin = "black", title = "Catapult",
                    
        ## HEADER ##
        dashboardHeader(title = strong("Catapult")),

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
                        column(3,
                            img(src="logo.png", height = "85%", width = "85%")       
                        ),
                        box(title = "Upload Catapult File(s)", width = 3, background = "black", align = "center",
                            fileInput("files", "Select CSV File(s)", multiple = TRUE, accept = c(".csv")),
                            tags$h5("Missing Catapult data and want to test out the app?"),
                            actionButton("demoFiles", "Get Files", onclick = "window.open('https://github.com/blg-uwm/Catapult/tree/master/Catapult%20Demo%20Files', '_blank')")
                        ),
                        gradientBox(width = 2, title = "Overview", footer = 
                            h2(id = "numPractice", textOutput("numPractice")),
                            br(),
                            h2(id = "numGame", textOutput("numGame"))
                        ),
                        tabBox(width = 4, title = "Player Load Statistics",
                            tabPanel("Games",
                                verbatimTextOutput("minGame"),
                                verbatimTextOutput("avgGame"),
                                verbatimTextOutput("maxGame")
                            ),
                            tabPanel("Practices",
                                verbatimTextOutput("minPractice"),
                                verbatimTextOutput("avgPractice"),
                                verbatimTextOutput("maxPractice")
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
                        column(5,
                            h3(strong(""))
                        )
                    ),
                        
                    ## ROW 2 ##
                    fluidRow(
                        column(12,
                               withSpinner(plotlyOutput("AverageTeamLoad"), type = 7, color = "#FFCC00", size = 2) 
                        )
                    ),
                    br(),
                        
                    ## ROW 3 ##
                    fluidRow(
                        column(12,
                               withSpinner(plotlyOutput("AverageGameCodeLoad"), type = 7, color = "#FFCC00", size = 2) 
                        )
                    )
                ),
                
                ## TAB 3 - Player ##
                tabItem(tabName = "tab3",
                        
                    ## ROW 1 ##
                    fluidRow(
                        column(3,
                            pickerInput("player", "Player", choices = NULL, options = pickerOptions(actionsBox = TRUE))
                        )
                    ),
                    
                    ## ROW 2 ##
                    fluidRow(
                      column(12,
                             withSpinner(plotlyOutput("playerLoad"), type = 7, color = "#FFCC00", size = 2) 
                      )
                    ),
                    br(),
                    
                    ## ROW 3 ##
                    fluidRow(
                      column(12,
                             withSpinner(plotlyOutput("AveragePlayerGameCodeLoad"), type = 7, color = "#FFCC00", size = 2) 
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
        
        for (i in 1:length(input$files$name)) {
            # Read File
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
        
            # Add Game-Day Coding
            dfCombined$nextdate <- lag(dfCombined$date)
            dfCombined$days_between <- dfCombined$nextdate -dfCombined$date 
            dfCombined$gdays <- NA
        
        
            for( i in 1:nrow(dfCombined)){
                if (dfCombined$activity[[i]] == "Game")
                    dfCombined$gdays[[i]] <- 0
                else {
                    if (i>1)
                        dfCombined$gdays[[i]] <- dfCombined$gdays[[i-1]] + dfCombined$days_between[[i]]
                    else dfCombined$gdays[[i]] = NA
                
            }
            
        } 
        
            dfCombined$gcode <- ifelse(dfCombined$gdays>0,paste0("G-",dfCombined$gdays),"G")
        
            dfCombined <- arrange(dfCombined,date)
            dfCombined <- dfCombined %>% select(1:8,12)
            colnames(dfCombined) <- c("Name", "Position", "Duration", "Distance", "playerLoad", "maxVelocity", "Date", "Activity", "gameCode")
            dfCombined <- dfCombined %>% filter(Duration != 0, Distance != 0, playerLoad != 0, maxVelocity != 0)
            
        return(dfCombined)
    })
    
    # Number of Games
    numGame <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Game") %>% group_by(Date) %>% n_distinct()
    })
    output$numGame <- renderText({
        req(input$files)
        paste(numGame(), "Games")
    })
    
    # Number of Practices
    numPractice <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Practice") %>% group_by(Date) %>% n_distinct()
    })
    output$numPractice <- renderText({
        req(input$files)
        paste(numPractice(), "Practices")
    })
    
    # Game Minimum
    minGame <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Game") %>% 
            summarise(minLoad = min(playerLoad)) %>% round(digits = 2)
    })
    output$minGame <- renderText({
        req(input$files)
        paste0("Min: ", minGame())
    })
    
    # Practice Minimum
    minPractice <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Practice") %>% 
            summarise(minLoad = min(playerLoad)) %>% round(digits = 2)
    })
    output$minPractice <- renderText({
        req(input$files)
        paste0("Min: ", minPractice())
    })
    
    # Game Average
    avgGame <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Game") %>% 
            summarise(avgLoad = mean(playerLoad)) %>% round(digits = 2)
    })
    output$avgGame <- renderText({
        req(input$files)
        paste0("Mean: ", avgGame())
    })
    
    # Practice Average
    avgPractice <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Practice") %>% 
            summarise(avgLoad = mean(playerLoad)) %>% round(digits = 2)
    })
    output$avgPractice <- renderText({
        req(input$files)
        paste0("Mean: ", avgPractice())
    })
    
    # Game Max
    maxGame <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Game") %>% 
            summarise(maxLoad = max(playerLoad)) %>% round(digits = 2)
    })
    output$maxGame <- renderText({
        req(input$files)
        paste0("Max: ", maxGame())
    })
    
    # Practice Max
    maxPractice <- reactive({
        Data() %>% select(Activity, playerLoad) %>% filter(Activity=="Practice") %>% 
            summarise(maxLoad = max(playerLoad)) %>% round(digits = 2)
    })
    output$maxPractice <- renderText({
        req(input$files)
        paste0("Max: ", maxPractice())
    })
    
    # Data Table Output
    output$table <- renderDT(Data(), extensions = c('Buttons', 'FixedHeader', 'Responsive'), rownames = FALSE, filter = 'top',
                             options = list(dom = 'Brtip', fixedHeader = TRUE, scroller = TRUE, bPaginate = FALSE, 
                                            buttons = c('csv', 'excel')))
    # Plotly Average Player Load Over Time
    output$AverageTeamLoad <- renderPlotly({
        p1 <- Data() %>% select(playerLoad, Date, Activity, gameCode) %>% group_by(Date) %>% 
            mutate(averagePlayerLoad = mean(playerLoad)) %>% distinct(Date, .keep_all = TRUE) %>% select(-playerLoad) %>%
            ggplot(aes(x = Date, y = averagePlayerLoad, group = 1, 
            text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "averagePlayerLoad: ", round(averagePlayerLoad, 2)))) + 
            geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
            scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Player Load Over Time")
        ggplotly(p1, tooltip = "text")
    })
    
    # Plotly Average Player Load/gameCode
    output$AverageGameCodeLoad <- renderPlotly({
        p2 <- Data() %>% select(playerLoad, Activity, gameCode) %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
            mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>%
            group_by(gameCode) %>% mutate(averagePlayerLoad = mean(playerLoad)) %>% select(-playerLoad) %>% 
            ggplot(aes(x = gameCode, y = averagePlayerLoad, group = 1)) + geom_point(aes(color = Activity), size = 4) + geom_line() + 
            theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
            scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Player Load by Game Code")
        ggplotly(p2)
    })
    
    # Update Player Selector Input
    observe({
        updatePickerInput(session, inputId = "player", choices = Data() %>% mutate(Name = as.character(Name)) %>% distinct(Name))
    })
    
    # Plotly Player Load Over Time/Player
    output$playerLoad <- renderPlotly({
      p3 <- Data() %>% select(Name, playerLoad, Date, Activity, gameCode) %>% filter(Name == input$player) %>%
        ggplot(aes(x = Date, y = playerLoad, group = 1, 
                   text = paste0("Date: ", Date, "\n", "gameCode: ", gameCode, "\n", "playerLoad: ", round(playerLoad, 2)))) + 
        geom_point(aes(color = Activity), size = 4) + geom_line() + theme_bw() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Player Load Over Time")
      ggplotly(p3, tooltip = "text")
    })
    
    # Plotly Average Player Load/gameCode
    output$AveragePlayerGameCodeLoad <- renderPlotly({
      p4 <- Data() %>% select(Name, playerLoad, Activity, gameCode) %>% filter(gameCode %in% c("G","G-1","G-2","G-3","G-4","G-5","G-6","G-7")) %>% 
        mutate(gameCode = factor(gameCode, levels = c("G-7","G-6","G-5","G-4","G-3","G-2","G-1","G"))) %>% filter(Name == input$player) %>%
        group_by(gameCode) %>% mutate(averagePlayerLoad = mean(playerLoad)) %>% select(-playerLoad) %>% 
        ggplot(aes(x = gameCode, y = averagePlayerLoad, group = 1)) + geom_point(aes(color = Activity), size = 4) + geom_line() + 
        theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5)) + 
        scale_color_manual(values = c("#FFCC00", "#003366")) + ggtitle("Average Player Load by Game Code")
      ggplotly(p4)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
