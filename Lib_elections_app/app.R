#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(scales)
library(rsconnect)
library(bslib)
#library(leaflet)


lib_elections <- read.csv("lib_election_clean.csv")
# lib_elections <- lib_elections %>%
#   filter(Candidate != "Valid Votes")
#lib_elections <- na.omit(lib_elections) # Remove 'NA' values


# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "quartz"), # change the theme
  titlePanel("Liberia Elections 2005 - 2017"),
  sidebarLayout(
    sidebarPanel(
      selectInput( "Elections_Year", "Select Year to explore votes per Candidate and County", choices = unique(lib_elections$Elections_Year), selected = unique(lib_elections$Elections_Year)[1])
    ),
    mainPanel(
      # Plot output for displaying the graph
      plotOutput("electionPlot", height = "400px"),
      br(),
      plotOutput("pie_chart", height = "400px"),
      plotOutput("countyVotes",  height = "400px"),
      plotOutput("pieChart",  height = "400px"),
      plotOutput("genderPlot"),
      plotOutput("partyCountPlot", height = "400px"),
      #leafletOutput("map",  height = "400px")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # Filter the dataset based on the selected year
  filtered_data <- reactive({
    data <- filter(lib_elections, Elections_Year == input$Elections_Year, Seat == 1)
    data$Gender <- factor(data$Gender, levels = c(1, 2), labels = c("Male", "Female"))
    return(data)
    
  })
  
  # Filter the top 10 candidates based on the selected year and graph
  top_candidates <- reactive({
    filtered_data() %>%
      group_by(Candidate) %>%
      summarise(Votes = sum(Precinct_TotalVotes)) %>%
      arrange(desc(Votes)) %>%
      slice_head(n = 10)
  })
  
  # Aggregate votes per county based on the year
  county_votes <- reactive({
    filtered_data() %>%
      group_by(County) %>%
      summarise(Votes = sum(Precinct_TotalVotes)) %>%
      arrange(desc(Votes))
  })
  
  # Aggregate by gender based on the year
  gender_count <- reactive({
    filtered_data() %>%
      group_by(Gender) %>%
      summarise(Count = n())
 
  })
  
    # Create a bar plot for the top 10 candidates
  output$electionPlot <- renderPlot({
    ggplot(top_candidates(), aes(x = Votes, y = reorder(Candidate, Votes), fill = Candidate)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = paste("Top 10 Candidates in", input$Elections_Year, "first round Election"),
           subtitle = "Data Source: https://liberiaelections.org/reports.html",
           x = "Votes",
           y = "Candidates") +
      scale_x_continuous(labels = scales::label_number(suffix = ""))
  }) 
  
  # Create a pie chart showing the percentage of votes for the top 10 candidates
  output$pie_chart <- renderPlot({
    # Calculate percentages for each candidate
    candidate_votes <- top_candidates() %>%
      mutate(percentage = Votes / sum(Votes) * 100)
    
    # Print the candidate_votes for debugging
  #print(candidate_votes)
    
    # Create the pie chart
    ggplot(candidate_votes, aes(x = "", y = percentage, fill = Candidate)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.5)) +  # Display percentage labels
      coord_polar("y", start = 0) +
      labs(title = paste("Percentage of Votes by Candidate in", input$Elections_Year, "first round Election"),
           x = NULL,
           y = "% of Votes") +
      theme_void() +
      theme(legend.title = element_blank())
  })
  
  # bar chart for county_votes
  
  output$countyVotes <- renderPlot({
    ggplot(county_votes(), aes(x = Votes, y = reorder(County, Votes), color = County)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +  # Remove grid lines
      labs(title = paste("Total votes per County in ", input$Elections_Year, "Election"),
           x = "Votes",
           y = "Counties") +
      scale_fill_continuous(labels = label_number(suffix = "")) +
      scale_x_continuous(labels = label_number(suffix = "")) +
      theme(legend.key.size = unit(0.8, "lines")) # Adjust the size of the color legend)
  }) 
  
  # Create a pie chart showing the percentage of votes per County
  output$pieChart <- renderPlot({
    # Calculate percentages for each candidate
    county_votes <- county_votes() %>%
      mutate(percentage = Votes / sum(Votes) * 100)
    
    # Print the candidate_votes for debugging
    #print(candidate_votes)
    
    # Create the pie chart
    ggplot(county_votes, aes(x = "", y = percentage, fill = Votes)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), position = position_stack(vjust = 0.2),
                color = "white", size = 4) +  # Display percentage labels
      coord_polar("y", start = 0) +
      labs(title = paste("Percentage of Votes per county in", input$Elections_Year, "first round Election"),
           x = NULL,
           y = "% of Votes") +
      theme_void() +
      scale_fill_continuous(labels = label_number(suffix = "")) +
      #scale_x_continuous(labels = scales::number_format(scale = 1e-3)) +
      theme(legend.title = element_blank()) +
      theme(legend.key.size = unit(0.8, "lines")) # Adjust the size of the color legend)
  })
  
  
  # Bar chart for gender proportion in each election
  output$genderPlot <- renderPlot({
    gender_count <- filtered_data() %>%
      group_by(Party, Gender) %>%
      summarise(Count = n(), .groups = 'keep')
    
    ggplot(gender_count, aes(x = Gender, y = Count, fill = Gender)) +
      geom_bar(stat = "identity") +
      theme_void() +
      labs(title = paste("Male and Female Proportion in", input$Elections_Year, "first round Election"),
           x = "Gender",
           y = "Count") 
  }) 
  
}

# Run the application 
shinyApp(ui = ui, server = server)

