library(shiny)

fluidPage(
  title = "LifeImprove",
  responsive = TRUE,
  
  fluidRow(
    column(3, offset = 2, uiOutput('controls')),
    column(3, uiOutput('time')),
    column(3, uiOutput('showProgress')),
    hr(),
    column(8, offset = 2,
           plotOutput('chart')
    )
  )
)
