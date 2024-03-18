library(shiny)
library(tidyverse)
library(rsconnect)
week8_tbl <- read_csv (file = 'week8_tbl_simple.csv')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("Relationship Between Q1-Q6 Mean and Q8-Q10 Mean"),

    # Sidebar
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu for selecting gender
      selectInput("sel_gender", "Select Gender", choices = c("All", "Male", "Female"), selected = "All"),
      
      # Dropdown menu for selecting error band display
      selectInput("sel_error_band", "Display Error Band?", choices = c("Display Error Band", "Suppress Error Band"), selected = "Display Error Band"),
      
      # Dropdown menu for selecting participant completion date filter
      selectInput("sel_filter_date", "Include participants before July 1, 2017?", choices = c("Include", "Do not include"), selected = "Include")
    ),

        # Show a plot 
        mainPanel(
           plotOutput("corr_plot")
        )
    )
)

# Define server logic required
server <- function(input, output) {

  #Filter data based on selections 1 and 3. This made the most sense in my brain because I'd used a similar format in the past for a Shiny app, but I'm not sure it's the most efficient.
  data_plot <- reactive({
    #gender = all and date filter = do not include
    if (input$sel_gender == "All" & input$sel_filter_date == "Do not include"){
      week8_tbl %>%
        filter(timeStart >= "2017-07-01 00:00:00")}
    #gender = male and date filter = include
    else if (input$sel_gender == "Male" & input$sel_filter_date == "Include"){
      week8_tbl|>
        filter(gender == "Male")}
    #gender = female and date filter = include
    else if (input$sel_gender == "Female" & input$sel_filter_date == "Include"){
      week8_tbl|>
        filter(gender == "Female")}
    #gender = male and date filter = do not include
    else if (input$sel_gender == "Male" & input$sel_filter_date == "Do not include"){
      week8_tbl|>
        filter(timeStart >= "2017-07-01 00:00:00" & gender == "Male")}
    #gender = female and date filter = do not include
    else if (input$sel_gender == "Female" & input$sel_filter_date == "Do not include"){
      week8_tbl>
        filter(timeStart >= "2017-07-01 00:00:00" & gender == "Female")}
    #gender = all and date filter = include
    else {
      week8_tbl}
  })
    output$corr_plot <- renderPlot({
      #plot including error band based on selection 2. Again, using the "if" and "else" language made the most sense to me.
      if(input$sel_error_band == "Display Error Band") {
        data_plot() %>%
        ggplot(aes(x = rowMeans(select(., q1:q6)), y = rowMeans(select(., q8:q10)))) +
        geom_point() +
        geom_smooth(method = "lm", color = "purple") +
        xlab("Mean Scores for Questions 1-6") +
        ylab("Mean Scores for Questions 8-10")}
      else{
        #plot without error band if selected
        data_plot() %>%
          ggplot(aes(x = rowMeans(select(., q1:q6)), y = rowMeans(select(., q8:q10)))) +
          geom_point() +
          geom_smooth(method = "lm", color = "purple", se = FALSE) +
          xlab("Mean Scores for Questions 1-6") +
          ylab("Mean Scores for Questions 8-10")}
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
