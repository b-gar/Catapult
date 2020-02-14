library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(data.table)
library(stringr)

# UI #
ui <- dashboardPage(skin = "yellow", title = "Catapult",
                    
        ## HEADER ##
        dashboardHeader(title = strong("Catapalt")),

        ## SIDEBAR ##
        dashboardSidebar(
            sidebarMenu(
                menuItem("File Upload", tabName = "tab1", icon = icon("file")),
                menuItem("test", tabName = "tab2", icon = icon("chart-line")),
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
                        box(width = 3, status = "warning", align = "center",
                            fileInput("files", "Select CSV File(s)", multiple = TRUE, accept = c(".csv")),
                            checkboxInput("show", "Show Table", value = TRUE)
                        )
                    ),
                    ## ROW 2 ##
                    fluidRow(
                        conditionalPanel(condition = "input.show == true",
                            column(12,
                               withSpinner(DTOutput("table"), type = 7, color = "#FFCC00", size = 3) 
                            )
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
        
            dfCombined$gcode <- ifelse(dfCombined$gdays>0,paste0("G",dfCombined$gdays),"G")
        
            dfCombined <- arrange(dfCombined,date)
            dfCombined <- dfCombined %>% select(1:8,12)
        
        return(dfCombined)
    })
    
    # Data Table Output
    output$table <- renderDT(Data(), options = list(scroller = TRUE, bPaginate = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
