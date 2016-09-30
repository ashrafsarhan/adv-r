library(shiny)
library(httr)
library(spotifyr)
library(jsonlite)

clientID = 'a223e7590d9043f1849bffa7ef86b102'
secret = 'aa351a255a8c49e7a4d69a8c2f893175'
res <- auth(clientID, secret)
token <- as.character(content(res)$access_token)

ui <- fluidPage(

  title = 'Spotify track feature analyzer',

  h1('Tracks: '),

  fluidRow(
    column(6, DT::dataTableOutput('table')),
    column(6, plotOutput('plot', height = 500))
  )

)

server <- function(input, output, session) {

  # server-side processing
  se_tracks <- read.csv2(
    "regional-se-daily-latest.csv", sep=",", header=TRUE)
  output$table = DT::renderDataTable(se_tracks, server = TRUE)

  streams <- se_tracks[,c('Position', 'Streams')]

  # highlight selected rows in the scatterplot
  output$plot = renderPlot({
    s = input$x1_rows_selected
    plot(streams)
  })

}

shinyApp(ui = ui, server = server)
