# app.R — VoteView and FEC - Shiny -------------------------------------------
# Author: bob.badgett@gmail.com
# License: Code GPL-3.0; Images CC BY-NC-SA 4.0
# Last edited: 2025-08-23

# ---- Libraries/packages (comment out for locked environments) ----
need <- c("shiny","data.table","DT","htmltools","bslib","crayon")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) {
  install.packages(p, repos = "https://cloud.r-project.org")
}

library(shiny)
library(data.table)
library(DT)
library(htmltools)
library(bslib)
library(crayon)

options(shiny.launch.browser = TRUE)

# Functions and Helpers: app directory + null-coalescing operator --------
`%||%` <- function(x, y) if (is.null(x)) y else x
APP_DIR <- shiny::getShinyOption("appdir") %||% normalizePath(getwd(), winslash = "/")

#___________________________________________________________-----
# Functions (no reactivity, no session state) ----------
function_load_csv <- function(
    app_dir    = APP_DIR,
    local_rel  = file.path("", "HS119_members.csv"),
    remote_url = "https://voteview.com/static/data/out/members/HS119_members.csv"
){
  status <- NULL
  dt <- NULL
  local_path <- file.path(app_dir, local_rel)
  
  if (file.exists(local_path)) {
    dt <- tryCatch(fread(local_path), error = function(e) e)
    if (inherits(dt, "error")) {
      status <- paste0("Error reading local file: ", local_path, " — ", dt$message)
      dt <- NULL
    } else {
      status <- paste0("Loaded local file: ", normalizePath(local_path, winslash = "/"))
    }
  } else {
    status <- paste0("Local file not found: ",
                     normalizePath(local_path, winslash = "/", mustWork = FALSE),
                     ". Attempting to load from VoteView URL…")
  }
  
  if (is.null(dt)) {
    dt <- tryCatch(fread(remote_url), error = function(e) e)
    if (inherits(dt, "error")) {
      stop(paste0(status, "\nAlso failed to load remote URL: ",
                  remote_url, " — ", dt$message))
    } else {
      status <- paste0(status, "\nLoaded from URL: ", remote_url)
    }
  }
  
  list(data = dt, status = status)
}

# UI **********************-----------------------------------------------------------
ui <- page_sidebar(
  titlePanel("SumSearch: searching for money influences on national politics"),
 ## sidebar -----
  sidebar = sidebar(
    open  = "open",   # "open" (user can close it) | "closed" (starts closed)
    width = 320,      # adjust to taste
    
    helpText("..."),
    selectInput("chamber", "Chamber:", c("House","Senate","All"), selected = "House"),
    actionButton("refresh", "Reload data"),
    tags$hr(),
  ),
  
  ## Main content ------
  ##* Tab 1: DW-NOMINATE scatterplot -----
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    selected = "DW-NOMINATE scatterplot",
    tabPanel(
      title = "DW-NOMINATE scatterplot",
      h3("DW-NOMINATE scatterplot"),
      # plotOutput("dwplot", height = 600)
    ),
    ##* Tab 2: Receipts by DW-NOMINATE plot -----
    tabPanel(
      title = "Receipts by DW-NOMINATE plot",
      # plotOutput("table2_preview", height = 600)
    ),
    ##* Tab 3: Recipients, sorted table -----
    tabPanel(
      title = "Recipients, sorted",
      h3("Recipients, sorted by the product of DW-NOMINATE1*receipts"),
      # DTOutput("recipients_table"),
      tags$br(),
      div(style="color:#666;",
          "Shows candidates with Received ($) > 0. Default page size is 50. ",
          "Party labels colored: Democrat (blue), Republican (red), Other (gray). ",
          strong("Outliers are highlighted in light pink."))
    ),
    ##* Tab 4: About / Sources / Help -----
    tabPanel(
      title = "About / Sources / Help",
      h3("About / Sources / Help"),
      h4("Data sources:"),
        tags$li("FEC.gov",
                tags$ul(
                  tags$li(
                    tags$a(href = "https://api.open.fec.gov/developers/", "open.FEC developers", target = "_blank")
                  ),
                    tags$li(
                      tags$a(href = "https://voteview.com/data", "VoteView.com/data", target = "_blank")
                    ),
                  tags$ul(
                    tags$li(
                      tags$a(href = "https://www.fec.gov/data/receipts/", "Receipts", target = "_blank")
                  )),
                  tags$li(
                    tags$a(href = "https://www.fec.gov/files/bulk-downloads/2026/ccl26.zip", "Crosswalk: candidate-committee", target = "_blank")
                )
        ),
        tags$li("VoteView.gov",
                tags$ul(
                  tags$li(
                    tags$a(href = "https://voteview.com/data", "Data", target = "_blank")
                  )
                )
        ),
        tags$li("opensecrets.org",
                tags$ul(
                  tags$li(
                    tags$a(href = "https://www.opensecrets.org/bulk-data", "Data", target = "_blank")
                  )
                )
        ),
        tags$li("GOVTRACK.us",
                tags$ul(
                  tags$li(
                    tags$a(href = "https://www.govtrack.us/congress/members/current", "Congressional members list", target = "_blank")
                    tags$a(href = "https://www.govtrack.us/accounts/lists", "Create trackers", target = "_blank")
                  )
                )
        ),
      )
    )
  )
)

server <- function(input, output) {}

# On shinyapps.io, just call shinyApp — no install.packages(), no runApp(), no browser options
shinyApp(ui = ui, server = server)
