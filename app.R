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

attach(htmltools::tags)

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
    open  = "closed",   # "open" (user can close it) | "closed" (starts closed)
    width = 320,      # adjust to taste
    
    helpText("..."),
    selectInput("chamber", "Chamber:", c("House","Senate","All"), selected = "House"),
    actionButton("refresh", "Reload data"),
    tags$hr(),
  ),
  
  ## Main content ------
  ##* Tab 1: Menu  -----
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    selected = "Welcome",
    tabPanel(
      title = "Welcome",
      h3("Choose a scenario to investigate"),
      ul(
        li("A political issue has surfaced and you want to know which congressional members are receiving donations from key political action committees (PAC). Enter the key text that is included in one of more PAC names, such as \"National Rifle Association\" or \"Planned Parenthood\" or \"Lockheed Martin\" (without quotes).", 
           textInput(inputId = "scenario1_search_string", value = "Lockheed Martin", label= NULL)),
        li("(Not implemented) You encountered a new political action committee (PAC) with a vague name. What is the weighted political leaning of recipients of donations from this PAC?"),
        li("(Not implemented) A member of congress makes new statements that do not align with prior stated views. What PAC money has this member received and have any of PACs increased their donations to this member?")
      )
      # plotOutput("dwplot", height = 600)
    ),
    ##* Tab 2: Results -----
    tabPanel(
      title = "Results",
      h3("Receipts by DW-NOMINATE scatterplot"),
      # plotOutput("table2_preview", height = 600)
    ),
    ##* Tab 3: Results - details -----
    tabPanel(
      title = "Results - details",
      h3("Recipients, sorted by the total values of receipts"),
      # DTOutput("recipients_table"),
      tags$br(),
      div(style="color:#666;",
          "Shows candidates with Received ($) > 0. Default page size is 50. ",
          "Party labels colored: Democrat (blue), Republican (red), Other (gray). ",
          strong("Outliers are highlighted in light pink."))
    ),
    ### Tab 4: About / Sources / Help -----
    tabPanel(
      title = "About / Sources / Help",
      h3("About / Sources / Help"),
      ###* Data sources ----
      h4("Data sources:"),
      tags$div("FEC.gov"),
      tags$ul(
        tags$li(
          tags$a(href = "https://api.open.fec.gov/developers/", "open.FEC developers", target = "_blank")
        ),
        tags$li(
          tags$a(href = "https://www.fec.gov/data/", "Data", target = "_blank")
        ),
        tags$ul(tags$li(
          tags$a(href = "https://www.fec.gov/data/", "Candidates", target = "_blank"), "(used for the candidate's 'fec_candidate_id')",
        ),
        tags$ul(tags$li(
          tags$a(href = "https://www.fec.gov/data/browse-data/?tab=bulk-data", "Candidate-committee linkages", target = "_blank"), " (used for current current year's linkages of fec_candidate_id to the  political action committee(s) (PACs) that the candidate uses to receive campaing contributions.", tags$a(href = "https://www.fec.gov/files/bulk-downloads/2026/ccl26.zip", "Direct link for this year's linkages", target = "_blank"), ")",
        ),
        ),
        ),
        tags$ul(tags$li(
            tags$a(href = "https://www.fec.gov/data/receipts/", "Receipts", target = "_blank"), " (search using the campaign committees aboves)",
          )
          ),
        tags$li(
          tags$a(href = "https://fecnotify.fec.gov/fecnotify/register/", "Create tracker", target = "_blank")
        )
      ),
      tags$div("VoteView.gov"),
      tags$ul(
        tags$li(
          tags$a(href = "https://voteview.com/data", "Data", target = "_blank")
        )
      ),
      tags$div("GOVTRACK.us"),
      tags$ul(
        tags$li(
          tags$a(href = "https://www.govtrack.us/congress/members/current", "Congressional members list", target = "_blank")
        ),
        tags$li(
          tags$a(href = "https://www.govtrack.us/accounts/lists", "Create tracker", target = "_blank")
        )
      ),
      tags$div("opensecrets.org"),
      tags$ul(
        tags$li(
          tags$a(href = "https://www.opensecrets.org/members-of-congress/members-list", "Congressional members list", target = "_blank")
        ),
        tags$li(
          tags$a(href = "https://www.opensecrets.org/bulk-data", "Data", target = "_blank")
        )
      ),
      ###* Finance sources *not* used ----
              h4("Data sources not used:"),
              tags$div("These entities cannot earmark/designate money from companies to candidates."),
              tags$ul(
                tags$li("Party national committees. The two major parties each have three national parties: their National Committee, Senatorial Campaign Committee, and Congressional Campaign Committee (",
                        tags$a(href = "https://ballotpedia.org/Party_committee_fundraising,_2025-2026", "Ballotopedia", target = "_blank"), ") However national committees do not earmark money from sources to candidates."
                ),
                tags$li(tags$a(href = "https://www.fec.gov/help-candidates-and-committees/joint-fundraising-candidates-political-committees/", "Conduit Committees", target = "_blank"),"do earmark.designate money to candidates, but only from contributions by individual citizens.",
                        # Conduit committees: ActBlue and WinRed
                        tags$ul(
                          tags$li(tags$a(href = "https://www.fec.gov/help-candidates-and-committees/joint-fundraising-candidates-political-committees/", "ActBlue", target = "_blank")),
                          tags$li(tags$a(href = "https://www.fec.gov/help-candidates-and-committees/joint-fundraising-candidates-political-committees/", "WinRed", target = "_blank"))
                        )
                ),
                tags$li( "Joint fund raising committees (JFCs) (",
                         tags$a(href = "https://www.fec.gov/help-candidates-and-committees/joint-fundraising-candidates-political-committees/", "FEC.gov", target = "_blank"), ", ",
                         tags$a(href = "https://www.opensecrets.org/joint-fundraising-committees-jfcs", "OpenSecrets.org", target = "_blank",")")
                )
              ),
      tags$h4("Help:"),
                      tags$ul(
                        tags$li(
                          tags$a(href = "https://github.com/badgettrg/SumSearch/issues", "Submit an issue at GitHub (requires a Google or GitHub account)", target = "_blank")
                        )
              ),
              ###* About ----
              h4("About:"),
              div(HTML(
                'The first SUMSearch was a medical meta-search engine started as "Medical SmartSearch" in October, 1998 
   (Wayback archive <a href="https://web.archive.org/web/20010608194559/http://sumsearch.uthscsa.edu/searchform5.htm">2000</a> 
   and <a href="https://web.archive.org/web/20100916215413/http://sumsearch.org/">2010</a>.)')),
              div(HTML("It was well-reviewed in its day (PMID <a href=\'https://pubmed.gov/\'>17603909</a>; <a href=\'https://pmc.ncbi.nlm.nih.gov/articles/PMC2000788/\'>PMC2000788</a>) but as continously and evidence-linked resources for healthcare practioners emerged like UpToDate and Dynamed, SUMSearch was retired in 2024. Hopefully, tracking with money in politics will also improve.")
              )
      )
    )
)

server <- function(input, output) {}

# On shinyapps.io, just call shinyApp — no install.packages(), no runApp(), no browser options
shinyApp(ui = ui, server = server)
