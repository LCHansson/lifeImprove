library(shiny)
library(ggplot2)

options("shiny.launch.browser" = TRUE)

source("analysis.R")

shinyServer(function(input, output, session) {
  ## Data ----
  lifedata <- reactive({
    # Summary of data transformation:
    # - Select only the variables that are to be analysed as "date" and "output"
    # - Group the data by a given date interval and calculate mean for each such group
    data <- analysdata %>%
      select(date, output = matches(input$var)) %>%
      filter(date >= (today() - as.numeric(input$timefilter))) %>%
      arrange(desc(row_number())) %>%
      mutate(timegrp = (row_number() - 1) %/% as.integer(input$progressInterval)) %>%
      group_by(timegrp) %>%
      mutate(avgval = mean(output))
    
    data
  })
  
  output$controls <- renderUI({
    selectInput(
      'var',
      'Variabel',
      choices = sort(unique(ticks_db$name)),
      width = 200
    )
  })
  
  output$time <- renderUI({
    selectInput(
      'timefilter',
      'Tid',
      choices = c(
        # "Vecka" = 7,
        "Månad" = 30,
        "Alltid" = 10000
      ),
      width = 200
    )
  })
  
  output$showProgress <- renderUI({
    selectInput(
      'progressInterval',
      'Visa utveckling',
      choices = c(
        'Inte alls' = 1,
        'Vecka' = 7,
        'Två veckor' = 14,
        'Månad' = 30
      ),
      width = 200
    )
  })
  
  output$chart <- renderPlot({
    p <- ggplot(lifedata(), aes(x = date, y = output)) + 
      geom_point()
    
    if (input$progressInterval > 1) {
      p <- p + geom_line(aes(y = avgval))
    }
    
    p
  })
})