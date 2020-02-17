library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(data.table)
library(stringr)

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
        dashboardBody(
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
                        column(6,
                               valueBoxOutput("numGames"),
                               valueBoxOutput("numPractices")
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
                
                ## TAB 2 -  ##
                tabItem(tabName = "tab2",
                        
                    ## ROW 1 ##
                    fluidRow(
                        column(5,
                            h3(strong(""))
                        ),
                        
                    ## ROW 2 ##
                    fluidRow(
                        
                    )
                    )
                )
            )
        )
)

# Server #
server <- function(input, output) {
    
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
        
        return(dfCombined)
    })
    
    # Reactive Value for Value Box Ouput Game
    numGame <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Game") %>% group_by(Date) %>% n_distinct()
    })
    
    # Value Box Output Game
    output$numGames <- renderValueBox({
        req(input$files)
        valueBox(numGame(), "Games", color = "black")
    })
    # Reactive Value for Value Box Output Practice
    numPractice <- reactive({
        Data() %>% select(Date, Activity) %>% filter(Activity=="Practice") %>% group_by(Date) %>% n_distinct()
    })
    
    # Value Box Output Practice
    output$numPractices <- renderValueBox({
        req(input$files)
        valueBox(numPractice(), "Practices", color = "black")
    })
    
    # Data Table Output
    output$table <- renderDT(Data(), extensions = c('Buttons', 'FixedHeader', 'Responsive'), rownames = FALSE, filter = 'top',
                             options = list(dom = 'Brtip', fixedHeader = TRUE, scroller = TRUE, bPaginate = FALSE, 
                                            buttons = c('csv', 'excel')))
}

# Run the application 
shinyApp(ui = ui, server = server)
