library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(data.table)
library(stringr)

# Custom function to get dates from file titles    

# UI #
ui <- dashboardPage(skin = "yellow", title = "Catapult",
                    
        ## HEADER ##
        dashboardHeader(title = strong("Catapalt")),

        ## SIDEBAR ##
        dashboardSidebar(
            sidebarMenu(
                menuItem("Import File", tabName = "tab1", icon = icon("file")),
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
                            fileInput("files", "Select CSV File(s)", multiple = TRUE, accept = c(".csv"))
                        ),
                        box(width = 3, status = "warning", align = "center",
                            checkboxInput("show", "Show Table")
                        )
                    ),
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
            df1 <- read.csv(input$files$datapath[i], skip = 9)
            df1$date <- str_extract(readLines(input$files$datapath[i], n=1), "\\d+/\\d+/\\d+")
            df1$activity <- str_extract(input$files$name[i], "practice|Practice|Activity|Game")
            df1$activity[df1$activity == "Activity"] <- "practice"
            df1$activity <- str_to_title(df1$activity)
            df1 <- df1 %>% filter(Period.Name == "Session") %>% select(1,4,7:9,16, date, activity)
            dfCombined <- rbind(df1, dfCombined)
            
        }
        return(dfCombined)
    })
    
    output$table <- renderDT(Data(), options = list(scroller = TRUE, bPaginate = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
