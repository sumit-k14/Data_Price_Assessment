library(shiny)
library(zoo) #used for converting date as yearmon
library(shinydashboard)
library(ggplot2)

# Define UI
ui <- fluidPage(
  titlePanel("Argus Index Calculator"),
    sidebarLayout(
      sidebarPanel(
        fileInput("data", "Select a file (Data.csv)"),
        actionButton("calculate", "Calculate Index"),
        downloadButton("download", "Save Output File")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Table", dataTableOutput("table"), icon = icon('table')),
        tabPanel("Chart", plotOutput("chart"), icon = icon('chart-line'))
      )
    )
  )
)



# Define server
server <- function(input, output) {

  # Load data
  data <- reactive({
    req(input$data)
    df <- read.csv(input$data$datapath)

    df$DEAL.DATE <- as.Date(df$DEAL.DATE, format = "%d/%m/%Y")

    #Adding a new column for delivery begin check
    df$delivery_begin_date <- as.yearmon(paste(df$DELIVERY.MONTH, df$DELIVERY.YEAR), "%b %Y")
    df$delivery_begin_date <- as.Date(df$delivery_begin_date, format = "%d/%m/%Y")
    df

  })

  # Calculate index table
  index_table <- eventReactive(input$calculate, {
    df <- data()

    # Filter relevant deals
    df <- df %>% filter(delivery_begin_date - DEAL.DATE <= 180)

    # Calculate VWAP and index price
    coal2_prices <- df %>% filter(DELIVERY.LOCATION %in% c("ARA", "AMS", "ROT", "ANT")) %>%
      group_by(DEAL.DATE) %>%
      summarise(VWAP = sum(PRICE * VOLUME) / sum(VOLUME))


    coal4_prices <- df %>% filter(COMMODITY.SOURCE.LOCATION == "South Africa") %>%
      group_by(DEAL.DATE) %>%
      summarise(VWAP = sum(PRICE * VOLUME) / sum(VOLUME))

    # Merge index prices into a single table
    index_table <- merge(coal2_prices, coal4_prices, by = "DEAL.DATE", all = TRUE)
    index_table[is.na(index_table)] <- 0  # Replace NA with 0
    colnames(index_table) <- c("DEAL.DATE", "COAL2_Index", "COAL4_Index")

    index_table
  })

  # Render index table
  output$table <- renderDataTable(options = list(scrollX = TRUE), {
    index_table()
  })

  # Calculate index chart
  index_chart <- eventReactive(input$calculate, {
    df <- index_table()

    # Create chart
    ggplot(df, aes(x = DEAL.DATE)) +
      geom_line(aes(y = COAL2_Index, color = "COAL2 Index")) +
      geom_line(aes(y = COAL4_Index, color = "COAL4 Index")) +
      labs(x = "Date", y = "Index Price", color = "Index") +
      theme_classic()
  })

  # Render index chart
  output$chart <- renderPlot({
    index_chart()
  })

  # Download index table as CSV
  output$download <- downloadHandler(
    filename = "index_table.csv",
    content = function(file) {
      write.csv(index_table(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)