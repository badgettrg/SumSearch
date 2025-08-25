# app.R — SumSearch

library(shiny)

ui <- fluidPage(
  titlePanel("SumSearch: searching for money influences on national politics"),
  sidebarLayout(
    sidebarPanel(helpText("This is a minimal Shiny app.")),
    mainPanel(h2("SumSearch"))
  )
)

server <- function(input, output) {}

# On shinyapps.io, just call shinyApp — no install.packages(), no runApp(), no browser options
shinyApp(ui = ui, server = server)
