# app.R — VoteView and FEC - Shiny -------------------------------------------
# Author: bob.badgett@gmail.com
# License: Code GPL-3.0; Images CC BY-NC-SA 4.0
# Last edited: 2025-08-23

# Notes: -----
# Consider
# SHARED_CACHE_ENABLED=true and S3 config,

# setwd ----
if (Sys.getenv("RSTUDIO") == "1") {
  # Running inside RStudio IDE
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  }
} else {
  # Running from R console or command line (e.g., Rscript, shiny::runApp)
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    script_path <- dirname(sub("^--file=", "", file_arg))
    setwd(script_path)
  }
}
getwd()

# Libraries/packages (comment out for locked environments) ----
need <- c("shiny","data.table","DT","htmltools","bslib","crayon","memoise","cachem","httr","pins")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) {
  install.packages(p, repos = "https://cloud.r-project.org")
}

library(shiny)
library(data.table)
library(DT)
library(htmltools)
library(bslib)
library(crayon)

# For caching:
library(memoise)
library(cachem)
library(pins)
library(digest)

#___________________________________________________________-----
# Helpers: app directory + null-coalescing operator --------
`%||%` <- function(x, y) if (is.null(x)) y else x

APP_DIR <- shiny::getShinyOption("appdir") %||% normalizePath(getwd(), winslash = "/")

# Functions (no reactivity, no session state) ----------
function_display_df_in_viewer <- function(df, caption = NULL, highlight_row = NULL, title = NULL) {
  df <- utils::head(as.data.frame(df), 10)  # <-- only send 10 rows
  
  cap <- if (!is.null(caption)) htmltools::HTML(
    paste0("<div style='text-align:left;'>", caption, "</div>")
  ) else NULL
  
  widget <- DT::datatable(
    df,
    caption = cap,
    rownames = FALSE,
    options = list(
      pageLength   = 10,
      scrollX      = TRUE,
      autoWidth    = TRUE,
      searching    = FALSE,
      lengthChange = FALSE,
      dom          = "t"
    )
  )
  
  # Show in RStudio Viewer pane only
  if (Sys.getenv("RSTUDIO") == "1" && interactive() &&
      requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() &&
      requireNamespace("htmlwidgets", quietly = TRUE)) {
    tmp <- tempfile(fileext = ".html")
    htmlwidgets::saveWidget(widget, tmp, selfcontained = TRUE,
                            title = title %||% "Preview")
    rstudioapi::viewer(tmp)
    return(invisible(TRUE))
  }
  
  widget
}


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

function_fec_get <- function(url, query, retries = 6, pause = 0.5) {
  # Helpful: identify your app/email in UA for API operators
  # with 429-aware backoff
  ua <- httr::user_agent("VoteView-Shiny (contact: youremail@example.com)")
  accept_json <- httr::accept_json()
  
  for (i in seq_len(retries)) {
    resp <- httr::GET(url, query = query, ua, accept_json, httr::timeout(60))
    status <- httr::status_code(resp)
    
    # Success fast-path
    if (status >= 200 && status < 300) return(resp)
    
    # Handle rate limiting & transient server errors with backoff
    if (status %in% c(429, 502, 503, 504)) {
      # Honor Retry-After if present; else exponential backoff
      ra <- httr::headers(resp)[["retry-after"]]
      if (!is.null(ra)) {
        wait_s <- suppressWarnings(as.numeric(ra))
        if (!is.na(wait_s) && wait_s > 0) {
          message(sprintf("Got %d. Respecting Retry-After: %s sec.", status, wait_s))
          Sys.sleep(wait_s)
        } else {
          # Sometimes Retry-After is HTTP-date; fall back to backoff
          backoff <- pause * (2^(i - 1))
          message(sprintf("Got %d. Backing off ~%.1f sec.", status, backoff))
          Sys.sleep(backoff)
        }
      } else {
        backoff <- pause * (2^(i - 1))
        message(sprintf("Got %d. Backing off ~%.1f sec.", status, backoff))
        Sys.sleep(backoff)
      }
      next
    }
    
    # Other HTTP errors: stop with details
    txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
    stop(sprintf("FEC API request failed (HTTP %d): %s", status, substr(txt, 1, 500)))
  }
  
  stop("FEC API request failed after retries (rate-limited or transient errors persisted).")
}

function_fetch_fec_schedule_a <- function(
    contributor_name               = NULL,
    committee_id                   = NULL,
    min_load_date                  = NULL,   # e.g., "2020-01-01"
    two_year_transaction_period    = NULL,   # e.g., 2026
    per_page                       = 100,    # API max
    sort                           = "-contribution_receipt_date",
    sort_hide_null                 = FALSE,
    sort_null_only                 = FALSE,
    api_key                        = Sys.getenv("FEC_API_KEY"),
    max_pages                      = Inf,    # fetch all unless capped here
    quiet                          = FALSE,
    progress_cb                    = NULL    # function(page, total_pages, n_so_far)
){
  if (!nzchar(api_key)) {
    warning("No API key in Sys.getenv('FEC_API_KEY'); using DEMO_KEY (very low rate limits).")
    api_key <- "DEMO_KEY"
  }
  base_url <- "https://api.open.fec.gov/v1/schedules/schedule_a/"
  build_query <- function(page) {
    q <- list(
      api_key         = api_key,
      per_page        = per_page,
      page            = page,
      sort            = sort,
      sort_hide_null  = tolower(as.character(sort_hide_null)),
      sort_null_only  = tolower(as.character(sort_null_only))
    )
    if (!is.null(contributor_name))            q$contributor_name <- contributor_name
    if (!is.null(committee_id))                q$committee_id <- committee_id
    if (!is.null(min_load_date))               q$min_load_date <- min_load_date
    if (!is.null(two_year_transaction_period)) q$two_year_transaction_period <- two_year_transaction_period
    q
  }
  
  # First page: get total pages & count ----
  resp1 <- function_fec_get(base_url, build_query(1L))
  txt1  <- httr::content(resp1, as = "text", encoding = "UTF-8")
  dat1  <- jsonlite::fromJSON(txt1, flatten = TRUE, bigint_as_char = TRUE)
  res1  <- dat1$results
  if (length(res1) == 0) return(data.table::data.table())
  
  total_pages <- suppressWarnings(as.integer(dat1$pagination$pages %||% 1L))
  total_count <- suppressWarnings(as.integer(dat1$pagination$count %||% NA_integer_))
  if (!is.finite(total_pages) || is.na(total_pages) || total_pages < 1L) total_pages <- 1L
  
  out_list <- list(data.table::as.data.table(res1))
  n_so_far <- nrow(out_list[[1]])
  if (!quiet) {
    base_msg <- sprintf("Fetched page 1 of %d (n=%d)", total_pages, n_so_far)
    if (!is.na(total_count)) base_msg <- sprintf("%s; API reports total_count=%s", base_msg, format(total_count, big.mark=","))
    message(base_msg)
  }
  if (!is.na(total_count) && total_count >= 5000L && !quiet) {
    message("⚠ FEC may cap paginated results at ~5,000 rows. Consider refining the query or using bulk downloads.")
  }
  if (is.function(progress_cb)) progress_cb(1L, total_pages, n_so_far)
  
  # Remaining pages (2..N, capped by max_pages) ----
  last_page <- max(1L, min(total_pages, if (is.finite(max_pages)) as.integer(max_pages) else total_pages))
  if (last_page >= 2L) {
    for (pg in 2:last_page) {
      resp <- function_fec_get(base_url, build_query(pg))
      txt  <- httr::content(resp, as = "text", encoding = "UTF-8")
      dat  <- jsonlite::fromJSON(txt, flatten = TRUE, bigint_as_char = TRUE)
      res  <- dat$results
      if (length(res) == 0) break
      out_list[[length(out_list) + 1L]] <- data.table::as.data.table(res)
      n_so_far <- n_so_far + nrow(out_list[[length(out_list)]])
      if (!quiet) {
        message(sprintf("Fetched page %d of %d (n=%d)", pg, total_pages, nrow(out_list[[length(out_list)]])))
      }
      if (is.function(progress_cb)) progress_cb(pg, total_pages, n_so_far)
      Sys.sleep(0.1)
    }
  }
  
  ans <- data.table::rbindlist(out_list, fill = TRUE)
  attr(ans, "fec_total_pages") <- total_pages
  attr(ans, "fec_total_count") <- total_count
  ans
}

## Policy: for the House, show donations from the two years PRIOR to the last House election up to now. -----
# Example: if last election year was 2024, start at 2022-01-01 through today.
last_house_election_even <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  if (y %% 2 == 0) y else y - 1
}
house_window_start <- function(today = Sys.Date()) {
  even <- last_house_election_even(today)
  as.Date(sprintf("%d-01-01", even - 2))
}

norm_str <- function(x) {
  x <- tolower(trimws(x %||% ""))
  gsub("\\s+", " ", x)
}

# Globals ------
APP_SCHEMA_VER <- "fec-v2"   # bump whenever schema/logic changes

## secrets ----
FEC_API_KEY <- Sys.getenv("FEC_API_KEY")

if (!nzchar(FEC_API_KEY)) {
  # Optional: project-local fallback (kept out of git)
  local_env <- file.path(getwd(), ".Renviron")
  if (file.exists(local_env)) {
    readRenviron(local_env)
    FEC_API_KEY <- Sys.getenv("FEC_API_KEY")
  }
}

if (!nzchar(FEC_API_KEY)) {
  stop("FEC_API_KEY not set. Set it as a user environment variable (Windows) or in shinyapps.io app settings, or add it to a project-local .Renviron.")
}

## Caching -----
# Tier A: per-instance disk cache (24h TTL)
fec_cache <- cache_disk(dir = file.path(tempdir(), "fec_cache"),
                        max_age = 24 * 60 * 60)

fec_key <- function(args) digest(list(
  contributor_name = tolower(trimws(args$contributor_name %||% "")),
  min_load_date    = as.character(args$min_load_date %||% ""),
  two_year_transaction_period = args$two_year_transaction_period %||% NULL,
  per_page         = as.integer(args$per_page %||% 100L),
  sort             = args$sort %||% "-contribution_receipt_date",
  sort_hide_null   = isTRUE(args$sort_hide_null),
  sort_null_only   = isTRUE(args$sort_null_only),
  max_pages        = if (is.infinite(args$max_pages)) "Inf" else as.integer(args$max_pages %||% 1L)
), algo = "xxhash64")

# IMPORTANT: keep the signature to only stable, value-type args — no progress/session/closures
# low-level raw fetcher (unchanged)
fec_fetch_raw <- function(contributor_name,
                          min_load_date,
                          two_year_transaction_period = NULL,
                          per_page = 100,
                          sort = "-contribution_receipt_date",
                          sort_hide_null = FALSE,
                          sort_null_only = FALSE,
                          api_key = Sys.getenv("FEC_API_KEY"),
                          max_pages = Inf) {
  function_fetch_fec_schedule_a(
    contributor_name = contributor_name,
    min_load_date    = min_load_date,
    two_year_transaction_period = two_year_transaction_period,
    per_page         = per_page,
    sort             = sort,
    sort_hide_null   = sort_hide_null,
    sort_null_only   = sort_null_only,
    api_key          = api_key,
    max_pages        = max_pages
  )
}

fec_fetch_cached <- function(
    # Cached wrapper with TTL, force refresh, quiet, and progress support.
  # Keeps cache key stable (no session/progress args in key).
  # Sources: per-instance disk (cachem) + optional shared pins board.
  contributor_name,
  min_load_date,
  two_year_transaction_period = NULL,
  per_page = 100,
  sort = "-contribution_receipt_date",
  sort_hide_null = FALSE,
  sort_null_only = FALSE,
  api_key = Sys.getenv("FEC_API_KEY"),
  max_pages = Inf,
  ttl_secs = 24 * 60 * 60,
  force_refresh = FALSE,
  quiet = TRUE,
  progress_cb = NULL
) {
  # ---- build stable cache key (no progress/session args) ----
  args_for_key <- list(
    contributor_name = tolower(trimws(contributor_name %||% "")),
    min_load_date    = as.character(min_load_date %||% ""),
    two_year_transaction_period = two_year_transaction_period %||% NULL,
    per_page         = as.integer(per_page %||% 100L),
    sort             = sort %||% "-contribution_receipt_date",
    sort_hide_null   = isTRUE(sort_hide_null),
    sort_null_only   = isTRUE(sort_null_only),
    max_pages        = if (is.infinite(max_pages)) "Inf" else as.integer(max_pages %||% 1L)
  )
  key <- paste0("fec_", fec_key(args_for_key))
  
  now <- Sys.time()
  is_fresh <- function(created) {
    isTRUE(!is.na(created)) && as.numeric(difftime(now, created, units = "secs")) < ttl_secs
  }
  
  # ---- 1) Try shared pins (if enabled) ----
  if (isTRUE(shared_enabled) && !force_refresh) {
    ok <- FALSE
    created_at <- NA
    if (pins::pin_exists(board, key)) {
      # meta$created may be NULL/NA on some boards; we persist one ourselves below.
      meta <- tryCatch(pins::pin_meta(board, key), error = function(e) NULL)
      created_at <- suppressWarnings(as.POSIXct(meta$user$created %||% meta$created, tz = "UTC"))
      if (!is.null(meta) && identical(meta$user$schema, APP_SCHEMA_VER) && is_fresh(created_at)) {
        dt <- pins::pin_read(board, key)
        if (data.table::is.data.table(dt)) {
          attr(dt, "fec_cache_source")  <- "shared"
          attr(dt, "fec_cache_created") <- created_at
          return(dt)
        }
      }
    }
  }
  
  # ---- 2) Try local cachem disk (per-instance) ----
  if (!force_refresh) {
    entry <- tryCatch(fec_cache$get(key), error = function(e) NULL)
    if (data.table::is.data.table(entry)) {
      created_at <- attr(entry, "fec_cache_created")
      # Backward-compat: if missing, treat as fresh (or mark now) to avoid false misses
      if (is.null(created_at)) created_at <- now
      if (is_fresh(created_at)) {
        attr(entry, "fec_cache_source")  <- "memory"
        attr(entry, "fec_cache_created") <- created_at
        return(entry)
      }
    }
  }
  
  # ---- 3) Miss or forced: fetch from API (show progress if provided) ----
  if (!quiet) message("Cache miss or refresh; fetching from FEC API…")
  dt <- function_fetch_fec_schedule_a(
    contributor_name = contributor_name,
    min_load_date    = min_load_date,
    two_year_transaction_period = two_year_transaction_period,
    per_page         = per_page,
    sort             = sort,
    sort_hide_null   = sort_hide_null,
    sort_null_only   = sort_null_only,
    api_key          = api_key,
    max_pages        = max_pages,
    quiet            = quiet,
    progress_cb      = progress_cb
  )
  created_at <- now
  
  # ---- 4) Save to local cachem ----
  # Keep attributes for future reads
  attr(dt, "fec_cache_source")  <- "memory"
  attr(dt, "fec_cache_created") <- created_at
  suppressWarnings({
    fec_cache$set(key, dt)
  })
  
  # ---- 5) Save to shared pins (if enabled) ----
  if (isTRUE(shared_enabled)) {
    meta <- list(schema = APP_SCHEMA_VER, created = format(created_at, tz = "UTC", usetz = TRUE))
    suppressWarnings({
      pins::pin_write(board, key, dt, type = "rds", metadata = list(user = meta))
    })
  }
  
  return(dt)
}

# Optional shared board: enable by setting SHARED_CACHE_ENABLED=true and AWS_* vars
shared_enabled <- identical(tolower(Sys.getenv("SHARED_CACHE_ENABLED", "false")), "true")
board <- if (shared_enabled) {
  # e.g., S3 in shinyapps.io; set AWS_* env vars in the app settings
  board_s3(bucket = Sys.getenv("AWS_S3_BUCKET"), prefix = "fec-cache/")
} else {
  board_temp()  # per-instance only; safe local fallback
}

# FEC static data:
# --- CCL mapping loaded once (app startup) ---
CCL_PATH <- file.path(APP_DIR, "ccl.txt")
if (!file.exists(CCL_PATH)) {
  stop(sprintf("Missing CCL file at %s", CCL_PATH))
}

dt_ccl <- data.table::fread(
  CCL_PATH, sep = "|", header = FALSE, showProgress = FALSE,
  col.names = c("candidate_id", "cand_election_yr", "fec_election_yr",
                "committee_id", "committee_type", "committee_designation",
                "linkage_id")
)

# Ensure join key types are character
dt_ccl[, candidate_id := as.character(candidate_id)]
dt_ccl[, committee_id := as.character(committee_id)]
data.table::setkey(dt_ccl, committee_id)  # speeds joins


# UI **********************-----------------------------------------------------------
ui <- page_sidebar(
  titlePanel("SumSearch: searching for money in national politics"),
  
  # --- Insert global CSS here ---
  tags$head(
    tags$style(HTML("
      .li-flex { display: flex; align-items: center; gap: 10px; flex-wrap: wrap; }
      .li-flex .shiny-input-container { margin-bottom: 0; }
      .li-flex .input-wrap { flex: 0 0 320px; }  /* fixed width, keeps button close */
      "))
  ),
  
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
      tags$ol(
        tags$li(
          div(
            # Description sits on its own line
            span("(under construction) A political issue has surfaced and you want to know which congressional members are receiving donations from key political action committees (PAC). Enter the key text that is included in one of more PAC names, such as \"National Rifle Association\" or \"Planned Parenthood\" or \"Lockheed Martin\" (without quotes).")
          ),
          div(class = "li-flex",
              div(class = "input-wrap",
                  textInput("scenario1_search_string", label = NULL, value = "Lockheed Martin")
              ),
              actionButton("scenario1_go", "Fetch FEC Receipts")
          )
        ),
        tags$li("(Not implemented) You encountered a new political action committee (PAC) with a vague name. What is the weighted political leaning of recipients of donations from this PAC?"),
        tags$li("(Not implemented) A member of congress makes new statements that do not align with prior stated views. What PAC money has this member received and have any of PACs increased their donations to this member?")
      )
      # plotOutput("dwplot", height = 600)
    ),
    
    ##* Tab 2: Results -----
    tabPanel(
      title = "Results",
      h3("Receipts by DW-NOMINATE scatterplot"),
      h4(textOutput("results_nrows"))
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

# SERVER ****************************------
server <- function(input, output, session) {
  
  # FEC receipts
  dt_fec <- reactiveVal(data.table::data.table())
  
  ## scenario1_go() -----
  observeEvent(input$scenario1_go, {
    # --- inputs & normalization ---
    scenario1_search_string <- input$scenario1_search_string
    scenario1_search_string <- if (is.null(scenario1_search_string)) "" else trimws(scenario1_search_string)
    if (identical(scenario1_search_string, "")) {
      showNotification("Please enter a PAC or organization name.", type = "warning")
      return(invisible(NULL))
    }
    
    # Date policy: House window start to now
    min_dt <- as.character(house_window_start(Sys.Date()))
    per_page <- as.integer(input$scenario1_per_page %||% 100L)
    sort_val <- "-contribution_receipt_date"
    sort_hide_null <- FALSE
    sort_null_only <- FALSE
    FEC_API_KEY <- Sys.getenv("FEC_API_KEY")
    
    # quick total_count probe (1-row request) to inform user ---
    total_count <- NA_integer_
    try({
      base_url <- "https://api.open.fec.gov/v1/schedules/schedule_a/"
      q1 <- list(
        api_key          = FEC_API_KEY,
        per_page         = 1L,
        page             = 1L,
        sort             = sort_val,
        sort_hide_null   = tolower(as.character(sort_hide_null)),
        sort_null_only   = tolower(as.character(sort_null_only)),
        contributor_name = scenario1_search_string,
        min_load_date    = min_dt
      )
      resp1 <- function_fec_get(base_url, q1)
      dat1  <- jsonlite::fromJSON(httr::content(resp1, as = "text", encoding = "UTF-8"), flatten = TRUE)
      total_count <- suppressWarnings(as.integer(dat1$pagination$count %||% NA_integer_))
    }, silent = TRUE)
    
    {
      n_txt <- if (!is.na(total_count)) format(total_count, big.mark = ",") else "an unknown number of"
      msg1  <- sprintf('The FEC reports %s receipts by PACs containing "%s" in the name since %s.',
                       n_txt, scenario1_search_string, min_dt)
      showNotification(msg1, type = "message", duration = 6)
    }
    
    # --- build EXACT key used by your shared cache (must match fetch args) ---
    args_for_key <- list(
      contributor_name = tolower(trimws(scenario1_search_string)),
      min_load_date    = min_dt,
      two_year_transaction_period = NULL,
      sort             = sort_val,
      sort_hide_null   = sort_hide_null,
      sort_null_only   = sort_null_only,
      per_page         = per_page,
      max_pages        = Inf
    )
    key <- paste0("fec_", fec_key(args_for_key))
    
    shared_fresh_hit <- FALSE
    if (isTRUE(shared_enabled) && pins::pin_exists(board, key)) {
      meta <- pins::pin_meta(board, key)
      age  <- suppressWarnings(as.numeric(difftime(Sys.time(), meta$created, units = "secs")))
      shared_fresh_hit <- !is.na(age) && age < 24 * 60 * 60 && identical(meta$user$schema, APP_SCHEMA_VER)
    }
    
    # --- SINGLE FETCH PATH ---
    # Only call the fetcher once. Pass progress_cb ONLY when we expect a fresh remote pull.
    if (!shared_fresh_hit) {
      withProgress(message = "Please be patient...", value = 0, {
        prog_cb <- function(pg, total, n_so_far) {
          v <- if (total > 0) pg / total else 0
          prev <- get(".__fec_last_v", envir = .GlobalEnv, inherits = FALSE) %||% 0
          incProgress(amount = max(0, v - prev),
                      detail  = sprintf("%s receipts received so far", format(n_so_far, big.mark=",")))
          assign(".__fec_last_v", v, envir = .GlobalEnv)
        }
        assign(".__fec_last_v", 0, envir = .GlobalEnv)
        
        dt <- fec_fetch_cached(
          contributor_name = scenario1_search_string,
          min_load_date    = min_dt,
          two_year_transaction_period = NULL,
          per_page         = per_page,
          sort             = sort_val,
          sort_hide_null   = sort_hide_null,
          sort_null_only   = sort_null_only,
          api_key          = FEC_API_KEY,
          max_pages        = Inf,
          ttl_secs         = 24 * 60 * 60,
          force_refresh    = FALSE,
          quiet            = FALSE,
          progress_cb      = prog_cb    # <- progress only when not shared-cached
        )
        remove(".__fec_last_v", envir = .GlobalEnv)
        dt_fec(dt)
      })
    } else {
      dt <- fec_fetch_cached(
        contributor_name = scenario1_search_string,
        min_load_date    = min_dt,
        two_year_transaction_period = NULL,
        per_page         = per_page,
        sort             = sort_val,
        sort_hide_null   = sort_hide_null,
        sort_null_only   = sort_null_only,
        api_key          = FEC_API_KEY,
        max_pages        = Inf,
        ttl_secs         = 24 * 60 * 60,
        force_refresh    = FALSE,
        quiet            = TRUE,
        progress_cb      = NULL        # <- ensure not part of key / no progress on cache hit
      )
      dt_fec(dt)
    }
    
    # WORK HERE post-fetch notifications ---
    dt <- dt_fec()
    Z  <- nrow(dt)
    showNotification(sprintf("%s receipts ready for analysis", format(Z, big.mark=",")),
                     type = "message", duration = 6)
    
    cache_src     <- attr(dt, "fec_cache_source")
    cache_created <- attr(dt, "fec_cache_created")
    if (!is.null(cache_src) && cache_src %in% c("shared","memory")) {
      if (identical(cache_src, "shared") && !is.null(cache_created)) {
        showNotification(sprintf("Using cached data from shared store (created %s, ≤24h).", cache_created),
                         type = "message", duration = 6)
      } else {
        showNotification("Using cached data (in-memory, ≤24h).", type = "message", duration = 6)
      }
    }
    
    total_count_attr <- attr(dt, "fec_total_count")
    if (!is.null(total_count_attr) && !is.na(total_count_attr) && total_count_attr >= 5000L) {
      showNotification("⚠ FEC may cap paginated results at ~5,000 rows. Try narrowing the date window or query.",
                       type = "warning", duration = 8)
    }
    
    updateTabsetPanel(session, inputId = "main_tabs", selected = "Results")
  })
  
  output$results_nrows <- renderText({
    dt <- dt_fec()
    function_display_df_in_viewer(dt_fec())
    total_count <- attr(dt, "fec_total_count")
    base <- paste0("Rows received from FEC: ", nrow(dt))
    if (!is.null(total_count) && !is.na(total_count)) {
      paste0(base, " (API reported total: ", format(total_count, big.mark=","), ")")
    } else {
      base
    }
  })


}



# Launch shinyApp *****************-----
if (Sys.getenv("RSTUDIO") == "1") {
  cat("~ expands to: ", path.expand("~"), "\n", sep = "")
  cat("R_ENVIRON_USER: ", Sys.getenv("R_ENVIRON_USER"), "\n", sep = "")
  cat("Renviron-like files in ~:\n")
  print(list.files(path.expand("~"), all.files = TRUE, pattern = "Renviron"))
  
  options(shiny.fullstacktrace = TRUE)
  options(shiny.launch.browser = TRUE)
  cat("Options set: shiny.fullstacktrace, shiny.launch.browser\n")
}

shinyApp(ui = ui, server = server)
