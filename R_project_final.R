# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)

# Load the dataset (adjust the path as necessary)
data <- read.csv("D:/桌面/all_seasons.csv")

# Define the UI
ui <- fluidPage(
  titlePanel("NBA Player Stats: Points, Assists, Rebounds, and Draft Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Select Team:", choices = unique(data$team_abbreviation)),
      selectInput("season", "Select Season:", choices = unique(data$season)),
      textInput("player", "Search Player:", value = ""),
      selectInput("draft_year", "Select Draft Year:", choices = unique(data$draft_year)),
      sliderInput("ptsRange", "Points Range:", 
                  min = min(data$pts, na.rm = TRUE), max = max(data$pts, na.rm = TRUE), 
                  value = c(min(data$pts, na.rm = TRUE), max(data$pts, na.rm = TRUE))),
      sliderInput("astRange", "Assists Range:", 
                  min = min(data$ast, na.rm = TRUE), max = max(data$ast, na.rm = TRUE), 
                  value = c(min(data$ast, na.rm = TRUE), max(data$ast, na.rm = TRUE))),
      checkboxInput("show_avg", "Show Team Averages", value = FALSE),
      downloadButton("downloadData", "Download Player Data")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Points vs Assists", plotOutput("ptsAstPlot")),
        tabPanel("Rebounds vs Assists", plotOutput("rebAstPlot")),
        tabPanel("Points vs Rebounds", plotOutput("ptsRebPlot")),
        tabPanel("Assists vs Rebounds", plotOutput("astRebPlot")),
        tabPanel("Player Details", tableOutput("playerTable")),
        tabPanel("Countries by Draft Year", plotOutput("pieChart")),
        tabPanel("Age Distribution", plotOutput("ageDistPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Filtered data based on inputs
  filteredData <- reactive({
    data %>%
      filter(team_abbreviation == input$team, 
             season == input$season, 
             grepl(input$player, player_name, ignore.case = TRUE), 
             between(pts, input$ptsRange[1], input$ptsRange[2]),
             between(ast, input$astRange[1], input$astRange[2]))
  })
  
  # General function to create static ggplot2 plots without interactive functionality
  createPlot <- function(plot_data, x_var, y_var, color, title) {
    ggplot(plot_data, aes_string(x = x_var, y = y_var, size = "player_height")) +
      geom_point(color = color, alpha = 0.7) +
      theme_minimal() +
      ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
  }
  
  # Points vs Assists Plot
  output$ptsAstPlot <- renderPlot({
    createPlot(filteredData(), "ast", "pts", "blue", paste("Points vs Assists -", input$team, input$season))
  })
  
  # Rebounds vs Assists Plot
  output$rebAstPlot <- renderPlot({
    createPlot(filteredData(), "ast", "reb", "green", paste("Rebounds vs Assists -", input$team, input$season))
  })
  
  # Points vs Rebounds Plot
  output$ptsRebPlot <- renderPlot({
    createPlot(filteredData(), "reb", "pts", "orange", paste("Points vs Rebounds -", input$team, input$season))
  })
  
  # Assists vs Rebounds Plot
  output$astRebPlot <- renderPlot({
    createPlot(filteredData(), "reb", "ast", "purple", paste("Assists vs Rebounds -", input$team, input$season))
  })
  
  # Player details table
  output$playerTable <- renderTable({
    filteredData() %>%
      select(player_name, team_abbreviation, season, pts, reb, ast, player_height, player_weight)
  })
  
  # Draft Year Pie Chart
  output$pieChart <- renderPlot({
    pie_data <- data %>%
      filter(draft_year == input$draft_year) %>%
      group_by(country) %>%
      summarise(num_players = n()) %>%
      arrange(desc(num_players))
    
    if (nrow(pie_data) > 5) {
      pie_data <- rbind(pie_data[1:5, ], data.frame(country = "Others", num_players = sum(pie_data$num_players[6:nrow(pie_data)])))
    }
    
    ggplot(pie_data, aes(x = "", y = num_players, fill = country)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = paste("Top 5 Countries in", input$draft_year, "Draft Year"), fill = "Country")
  })
  
  # Age Distribution Plot
  output$ageDistPlot <- renderPlot({
    ggplot(data %>% filter(draft_year == input$draft_year), aes(x = age)) +
      geom_bar(fill = "blue", alpha = 0.7) +
      theme_minimal() +
      labs(title = paste("Age Distribution in", input$draft_year, "Draft Year"), x = "Age", y = "Count")
  })
  
  # Show team averages in a modal
  observe({
    if (input$show_avg) {
      avg_data <- filteredData() %>%
        summarise(Average_Points = mean(pts, na.rm = TRUE),
                  Average_Rebounds = mean(reb, na.rm = TRUE),
                  Average_Assists = mean(ast, na.rm = TRUE))
      
      showModal(modalDialog(title = "Team Average Stats", renderTable(avg_data)))
    }
  })
  
  # Download player data
  output$downloadData <- downloadHandler(
    filename = function() { paste("player_data_", input$team, "_", input$season, ".csv", sep = "") },
    content = function(file) { write.csv(filteredData(), file, row.names = FALSE) }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
