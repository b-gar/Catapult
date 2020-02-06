library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(data.table)

# Define UI for application that draws a histogram
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
                            fileInput("files", "Select CSV File", multiple = TRUE, accept = c(".csv"))
                        ),
                        box(width = 3, status = "warning", align = "center",
                            dateInput("date", label = "Date of File", value = "2020-01-01")
                        )
                    ),
                    ## ROW 2 ##
                    fluidRow(
                        column(12,
                              withSpinner(dataTableOutput("table"), type = 7, color = "#FFCC00", size = 2) 
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

# Server
server <- function(input, output) {
    
    df <- reactive({
            data <- as.data.frame(rbindlist(lapply(input$files$datapath, read.csv, skip = 9)))
            
    })
    
    
    output$table <- renderDataTable(df(), options = list(scroller = TRUE, bPaginate = FALSE))
}

# Run the application 
shinyApp(ui = ui, server = server)
