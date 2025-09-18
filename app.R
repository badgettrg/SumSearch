# app.R — VoteView and FEC - Shiny -------------------------------------------
# Author: bob.badgett@gmail.com
# License: Code GPL-3.0; Images CC BY-NC-SA 4.0
# Last edited: 2025-08-23

# Notes: -----
# Consider
# SHARED_CACHE_ENABLED=true and S3 config,
# Troubleshooting
# dt_voteview <- reactiveVal(data.table::data.table())
# if (interactive()) utils::View(utils::head(dt_voteview, 10))


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
need <- c("shiny","shinyjs","data.table","openxlsx","DT","htmltools","bslib","crayon","memoise","cachem","httr","pins")
for (p in need) if (!requireNamespace(p, quietly = TRUE)) {
  install.packages(p, repos = "https://cloud.r-project.org")
}

library(shiny)
library(shinyjs)
library(data.table)
library(openxlsx)
library(DT)
library(htmltools)
library(bslib)
library(crayon)
library(stringr)

# For caching:
library(memoise)
library(cachem)
library(pins)
library(digest)

#___________________________________________________________-----
# Helpers: app directory + null-coalescing operator --------

APP_DIR <- shiny::getShinyOption("appdir") %||% normalizePath(getwd(), winslash = "/")

`%||%` <- function(x, y) if (is.null(x)) y else x

norm_str <- function(x) {
  x <- tolower(trimws(x %||% ""))
  gsub("\\s+", " ", x)
}

# Functions (no reactivity, no session state) ----------
# Example: if last election year was 2024, start at 2022-01-01 through today.
function_last_house_election_even_year <- function(today = Sys.Date()) {
  y <- as.integer(format(today, "%Y"))
  if (y %% 2 == 0) y else y - 1
}
function_house_window_start <- function(today = Sys.Date()) {
  even <- function_last_house_election_even_year(today)
  as.Date(sprintf("%d-01-01", even - 2))
}

function_notify_ui_and_console <- function(text, type = c("error","warning","message"), duration = 3) {
  # txt, type ("error","warning","message") ----
  # duration = 3 or ID----
  type <- match.arg(type)
  # Console (red/green/orange bold)
  if (type == "error")   cat(crayon::red$bold  ("\n[FEC API] ", text, "\n", sep=""))
  if (type == "warning") cat(crayon::yellow$bold("\n[FEC API] ", text, "\n", sep=""))
  if (type == "message") cat(crayon::green$bold ("\n[FEC API] ", text, "\n", sep=""))
  
  # UI toast (if we’re inside a Shiny session)
  # Resolve id/duration based on the class of `duration` ----
  id_val       <- if (is.character(duration) && length(duration) == 1 && !is.na(duration) && nzchar(duration)) duration else NULL
  duration_val <- if (is.numeric(duration)   && length(duration) == 1 && is.finite(duration)) duration else NULL
  
  dom <- shiny::getDefaultReactiveDomain()
  if (!is.null(dom)) shiny::showNotification(text, type = type, id = id_val, duration = duration_val, session = dom)
}

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

function_fec_current_download <- function(search_status, tracking_pattern = tracking_pattern, scenario1_search_string = NULL, dt_temp = dt_fec_receipts){
  # Usage (click here to see the call) -----
  # function_fec_current_download ("1. Before cleaning", tracking_pattern = tracking_pattern, scenario1_search_string = scenario1_search_string, dt_temp = dt_fec_receipts)
  if (!(interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()))
    return(invisible(NULL))
  
  cat(red$bgYellow$bold('\nsearch_status: ', search_status, '\n'))
  
  if (is.null(tracking_pattern)) tracking_pattern <- get0("tracking_pattern", inherits = TRUE)
  if (is.null(dt_temp))          dt_temp          <- get0("dt_fec_receipts", inherits = TRUE)
  if (is.null(scenario1_search_string)) {
    dom <- shiny::getDefaultReactiveDomain()
    if (!is.null(dom)) scenario1_search_string <- shiny::isolate(dom$input$scenario1_search_string)
  }
  if (is.null(scenario1_search_string) || !nzchar(trimws(as.character(scenario1_search_string))))
    stop("scenario1_search_string is required (pass it explicitly or call within a Shiny session).")
  
  wb <- createWorkbook()
  
  #* Conditional styling HERE 2025-09-16
  header_style     <- createStyle(textDecoration = "bold", fgFill = "#f2f2f2")
  col_red_style    <- createStyle(fgFill = "#fde0e0")
  col_green_style  <- createStyle(fgFill = "#e6f4ea")
  match_cell_style <- createStyle(fgFill = "#ffd6d6")  # light red for matched cells
  col_blue_style <- createStyle(fgFill = "#dbe9ff")
  write_styled_sheet <- function(wb, sheet_name, data, tracking_pattern) {
    addWorksheet(wb, sheet_name)
    # Ensure columns exist even if data has 0 rows
    if (nrow(data) == 0L) data <- data[0]  # zero-row with same columns
    writeData(
      wb, sheet = sheet_name, x = data,
      startCol = 1, startRow = 1, headerStyle = header_style
    )
    freezePane(wb, sheet = sheet_name, firstRow = TRUE)
    column_committee <- match("committee_id", names(data))
    column_amount    <- match("contribution_receipt_amount", names(data))
    column_contribution_receipt_date    <- match("contribution_receipt_date", names(data))
    column_transaction_id <- match("transaction_id", names(data))
    column_amendment_indicator_desc <- match("amendment_indicator_desc", names(data))
    # Append Summary row
    sum_val <- if (!is.na(column_amount)) {
      suppressWarnings(sum(as.numeric(data[[column_amount]]), na.rm = TRUE))
    } else NA_real_
    # Build a one-row data.table with NA defaults
    summary_row <- as.list(rep(NA, ncol(data)))
    names(summary_row) <- names(data)
    if (ncol(data) > 0) {
      summary_row[[1]] <- "Summary"
      if (!is.na(column_amount)) summary_row[["contribution_receipt_amount"]] <- sum_val
    }
    summary_dt <- as.data.table(summary_row)
    
    # Where to put the Summary row
    n <- nrow(data)
    summary_row_index <- n + 2  # header=1, data starts at 2
    if (ncol(data) > 0) {
      writeData(wb, sheet = sheet_name, x = summary_dt, startCol = 1, startRow = summary_row_index, colNames = FALSE)
    }
    # Style full columns (data rows + summary row only; not the header)
    last_row <- max(2, summary_row_index)  # at least row 2 if only summary exists
    if (!is.na(column_committee)) {
      addStyle(wb, sheet_name, style = col_red_style,
               rows = 2:last_row, cols = c(column_committee, column_transaction_id, column_contribution_receipt_date, column_amendment_indicator_desc), gridExpand = TRUE, stack = TRUE)
    }
    if (!is.na(column_amount)) {
      addStyle(wb, sheet_name, style = col_green_style,
               rows = 2:last_row, cols = column_amount, gridExpand = TRUE, stack = TRUE)
    }
    # Conditional highlight for tracking_pattern - matching cells (data rows only)
    if (n > 0 && ncol(data) > 0) {
      for (j in seq_along(data)) {
        # Convert to character safely for grepl
        chr <- as.character(data[[j]])
        hits <- which(grepl(tracking_pattern, chr, ignore.case = TRUE))
        if (length(hits)) {
          addStyle(
            wb, sheet_name, style = match_cell_style,
            rows = hits + 1,  # +1 for header offset
            cols = j, gridExpand = TRUE, stack = TRUE
          )
        }
      }
    }
    invisible(NULL)
  }
  # Sheet 1: all
  cat(green$bold("\nAll records, rows: "),black(nrow(dt_temp)))
  write_styled_sheet(wb, "Receipts - all", dt_temp, tracking_pattern)
  # Sheet 2: Conditional
  # using tracking_pattern
  any_match <- dt_temp[
    , Reduce(`|`, lapply(.SD, function(x) grepl(tracking_pattern, x, ignore.case = TRUE))),
    .SDcols = names(dt_temp)]
  dt_selected <- dt_temp[any_match]
  cat(green$bold("\ntracking_pattern, rows: "),black(nrow(dt_selected)))
  write_styled_sheet(wb, "Receipts - selected",  dt_selected, tracking_pattern)
  # Sheet 3: Conditional and unique
  dt_norm <- copy(dt_selected)
  is_list <- vapply(dt_norm, is.list, logical(1))
  if (any(is_list)) {
    for (nm in names(dt_norm)[is_list]) {
      set(dt_norm, j = nm, value = vapply(dt_norm[[nm]], function(x) paste(capture.output(dput(x)), collapse=""), character(1)))
    }
  }
  uniq_idx <- !duplicated(dt_norm)
  dt_unique_selected <- dt_selected[uniq_idx]
  # Option A — plain R
  cat(green$bold("\ntracking_pattern and unique, rows: "),
      black(format(nrow(dt_unique_selected), big.mark=",")), "\n")
  
  cat(green$bold("\ntracking_pattern and contribution_receipt_amount (total value): "),
      black(format(sum(dt_unique_selected[["contribution_receipt_amount"]], na.rm=TRUE), big.mark=",")), "\n")
  write_styled_sheet(wb, "Receipts - unique (selected)", dt_unique_selected, tracking_pattern)
  #* Sheet 3: highlight conflicted cells
  conflict_cols <- character(0)
  dt_cmp <- copy(dt_selected)
  is_list2 <- vapply(dt_cmp, is.list, logical(1))
  if (any(is_list2)) {
    for (nm in names(dt_cmp)[is_list2]) {
      set(dt_cmp, j = nm, value = vapply(dt_cmp[[nm]], function(x) paste(capture.output(dput(x)), collapse=""), character(1)))
    }
  }
  if (nrow(dt_cmp) > 1L && ncol(dt_cmp) > 1L) {
    cn <- names(dt_cmp)
    for (j in cn) {
      by_cols <- setdiff(cn, j)
      if (length(by_cols) == 0L) next
      tmp <- dt_cmp[, .(n_unique = uniqueN(get(j))), by = by_cols]
      if (any(tmp$n_unique > 1L, na.rm = TRUE)) conflict_cols <- c(conflict_cols, j)
    }
    conflict_cols <- unique(conflict_cols)
  }
  
  if (length(conflict_cols)) {
    n <- nrow(dt_unique_selected)
    summary_row_index <- n + 2
    last_row <- max(2, summary_row_index)
    addStyle(
      wb, "Receipts - unique (selected)",
      style = col_blue_style,
      rows = 2:last_row,
      cols = match(conflict_cols, names(dt_unique_selected)),
      gridExpand = TRUE, stack = TRUE
    )
  }
  cat(green(
    "\nmin date:", min(dt_temp$contribution_receipt_date, na.rm = TRUE),
    " max date:", max(dt_temp$contribution_receipt_date, na.rm = TRUE),
    "\n"
  ))

  # Save workbook
  file_name <- paste0("dt_fec_receipts - ", scenario1_search_string, " - ", search_status, " - ", Sys.Date(), ".xlsx")
  saveWorkbook(wb, file_name, overwrite = TRUE)
  cat(green("\nFile "), black("\"", file_name,"\" downloaded to ", getwd(),".\n"))
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
  ua <- httr::user_agent("VoteView-Shiny (contact: youremail@example.com)")
  accept_json <- httr::accept_json()
  
  last_err <- NULL
  informed_first_failure <- FALSE
  
  for (i in seq_len(retries)) {
    to <- 60 + 15 * (i - 1)
    
    resp <- tryCatch(
      httr::GET(url, query = query, ua, accept_json, httr::timeout(to)),
      error = function(e) e
    )
    
    if (inherits(resp, "error")) {
      last_err <- resp
      if (!informed_first_failure) {
        function_notify_ui_and_console(
          sprintf("Network timeout/error; retrying up to %d times...", retries),
          type = "warning", duration = 8
        )
        informed_first_failure <- TRUE
      }
      backoff <- pause * (2^(i - 1))
      cat(crayon::red$bold(sprintf("\n[FEC API] %s. Retrying in ~%.1fs (attempt %d/%d)\n",
                                   conditionMessage(resp), backoff, i, retries)))
      Sys.sleep(backoff)
      next
    }
    
    status <- httr::status_code(resp)
    if (status >= 200 && status < 300) return(resp)
    
    if (status %in% c(408, 429, 502, 503, 504)) {
      if (!informed_first_failure) {
        function_notify_ui_and_console(
          sprintf("FEC.gov temporary issue (HTTP %d); retrying...", status),
          type = "warning", duration = 8
        )
        informed_first_failure <- TRUE
      }
      ra <- httr::headers(resp)[["retry-after"]]
      if (!is.null(ra)) {
        wait_s <- suppressWarnings(as.numeric(ra))
        if (!is.na(wait_s) && wait_s > 0) {
          Sys.sleep(wait_s); next
        }
      }
      Sys.sleep(pause * (2^(i - 1)))
      next
    }
    
    # Non-retriable HTTP error → notify + return NULL
    txt <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
    function_notify_ui_and_console(sprintf("FEC API request failed, will try again (HTTP %d).", status), type = "error", duration = 10)
    return(NULL)
  }
  
  function_notify_ui_and_console(
    sprintf("FEC.gov request failed after %d attempts. Please try again.", retries),
    type = "error", duration = 10
  )
  return(NULL)
}

function_fec_count_unique_link_id_count <- function(base_url, base_query, sort = "-contribution_receipt_date",
                                     per_page = 100L, max_pages = Inf) {
  seen <- new.env(parent = emptyenv())
  n_unique <- 0L
  last_index <- NULL
  pages <- 0L
  repeat {
    q <- modifyList(base_query, list(
      per_page   = per_page,
      sort       = sort,
      last_index = last_index,
      fields     = "link_id,transaction_id,sub_id,file_number,load_date"
    ))
    cat(green$bold("Initial URL sending to FEC: ", base_url,q))
    resp <- function_fec_get(base_url, q)
    if (is.null(resp)) break
    js <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"), flatten = TRUE)
    res <- data.table::as.data.table(js$results)
    if (!nrow(res)) break
    
    keycol <- if ("link_id" %in% names(res)) "link_id"
    else if ("transaction_id" %in% names(res)) "transaction_id"
    else "sub_id"
    keys <- as.character(res[[keycol]])
    if (length(keys)) {
      new <- vapply(keys, function(k) {
        if (!is.na(k) && !exists(k, envir = seen, inherits = FALSE)) {
          assign(k, TRUE, envir = seen); TRUE
        } else FALSE
      }, logical(1))
      n_unique <- n_unique + sum(new)
    }
    
    li <- js$pagination$last_indexes$last_index
    if (is.null(li) || identical(li, last_index)) break
    last_index <- li
    
    pages <- pages + 1L
    if (pages >= max_pages) break
  }
  n_unique
}


function_fec_fetch_schedule_a_keyset <- function(
    contributor_name,
    min_load_date  = "2022-01-01",
    per_page       = 100L,
    sort           = "-contribution_receipt_date",  # DESC for keyset
    sort_hide_null = FALSE,
    sort_null_only = FALSE,                          # starting preference; may be toggled by server
    api_key        = Sys.getenv("FEC_API_KEY"),
    max_pages      = Inf,
    quiet          = FALSE,
    progress_cb    = NULL                            # function(pg, total_guess, n_rows_so_far)
) {
  stopifnot(length(per_page) == 1L, per_page %in% 1:100)
  if (!nzchar(api_key)) api_key <- "DEMO_KEY"
  
  base_url <- "https://api.open.fec.gov/v1/schedules/schedule_a/"
  q_base <- list(
    api_key          = api_key,
    contributor_name = contributor_name,
    min_load_date    = min_load_date,
    per_page         = per_page,
    sort             = sort,
    sort_hide_null   = tolower(as.character(sort_hide_null))
  )
  
  library(httr); library(jsonlite); library(data.table)
  
  out <- list()
  total_count <- NA_integer_
  n_rows <- 0L
  pg <- 0L
  
  # keyset cursor we send back to the server:
  cursor_last_index <- NULL
  cursor_last_date  <- NULL
  sort_null_only_flag <- isTRUE(sort_null_only)
  
  # simple repeated-page guard:
  seen_sigs <- character()
  
  repeat {
    pg <- pg + 1L
    if (pg > max_pages) break
    
    q <- q_base
    q$sort_null_only <- tolower(as.character(sort_null_only_flag))
    
    if (!is.null(cursor_last_index)) {
      q$last_index <- cursor_last_index
      # only send last_contribution_receipt_date while we're in the non-null segment
      if (!sort_null_only_flag && !is.null(cursor_last_date)) {
        q$last_contribution_receipt_date <- cursor_last_date
      }
    }
    
    resp <- httr::GET(base_url, query = q)
    if (httr::http_error(resp)) {
      stop("FEC error: ", httr::status_code(resp), " - ",
           httr::content(resp, as = "text", encoding = "UTF-8"))
    }
    
    js <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    if (is.na(total_count)) {
      total_count <- js$pagination$count %||% NA_integer_
    }
    
    res <- as.data.table(js$results)
    if (nrow(res) == 0L) break
    
    # repeated-page guard using first+last sub_id (or transaction_id fallback)
    first_id <- if ("sub_id" %in% names(res)) res$sub_id[1] else res$transaction_id[1]
    last_id  <- if ("sub_id" %in% names(res)) res$sub_id[nrow(res)] else res$transaction_id[nrow(res)]
    page_sig <- paste(first_id, last_id, nrow(res), sep = "::")
    if (page_sig %in% seen_sigs) {
      stop(sprintf("Detected repeated page at logical page %d; cursor is not advancing.", pg))
    }
    seen_sigs <- c(seen_sigs, page_sig)
    
    out[[length(out) + 1L]] <- res
    n_rows <- n_rows + nrow(res)
    
    # progress
    if (is.function(progress_cb)) {
      total_guess <- if (!is.na(total_count)) ceiling(total_count / per_page) else pg + 1L
      progress_cb(pg, total_guess, n_rows)
    } else if (!quiet) {
      message(sprintf("Fetched keyset page %d (n=%d). Rows so far: %s",
                      pg, nrow(res), format(n_rows, big.mark=",")))
    }
    
    # pull next cursor from pagination.last_indexes (NOT from rows)
    li <- js$pagination$last_indexes
    if (is.null(li) || is.null(li$last_index)) {
      # no cursor provided means we're done
      break
    }
    cursor_last_index <- li$last_index
    
    # Two cases per FEC docs:
    # 1) Normal segment: server gives last_contribution_receipt_date => keep paging with it.
    # 2) Null-date segment: server gives sort_null_only = TRUE => toggle and continue with only last_index.
    if (!is.null(li$last_contribution_receipt_date)) {
      cursor_last_date <- li$last_contribution_receipt_date
      # do not change sort_null_only_flag
    } else if (!is.null(li$sort_null_only) && isTRUE(li$sort_null_only)) {
      sort_null_only_flag <- TRUE
      cursor_last_date <- NULL
    } else {
      # Defensive: if neither field is present, stop to avoid looping
      stop("Server did not supply a usable keyset cursor (no last_contribution_receipt_date and sort_null_only is not TRUE).")
    }
  }
  
  dt <- rbindlist(out, use.names = TRUE, fill = TRUE)
  
  # Attach helpers for the UI
  attr(dt, "fec_total_count") <- total_count
  if ("contribution_receipt_date" %in% names(dt)) {
    suppressWarnings(dt[, contribution_receipt_date := as.POSIXct(contribution_receipt_date, tz = "UTC")])
  }
  if ("load_date" %in% names(dt)) {
    suppressWarnings(dt[, load_date := as.POSIXct(load_date, tz = "UTC")])
  }
  dt
}


function_fetch_fec_schedule_a <- function(
    contributor_name               = NULL,
    committee_id                   = NULL,
    min_load_date                  = min_load_date,   # e.g., "2020-01-01"
    two_year_transaction_period    = NULL,   # e.g., 2026
    #two_year_transaction_period = c(2024,2026),
    #min_election_date=2024,
    #max_election_date=2026,
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
  cat(blue$bold("\nBase url: ",base_url,"\n"))
  # First page: get total pages & count ----
  resp1 <- function_fec_get(base_url, build_query(1L))
  txt1  <- httr::content(resp1, as = "text", encoding = "UTF-8")
  dat1  <- jsonlite::fromJSON(txt1, flatten = TRUE, bigint_as_char = TRUE)
  res1  <- dat1$results
  if (length(res1) == 0) return(data.table::data.table())
  
  total_pages <- suppressWarnings(as.integer(dat1$pagination$pages %||% 1L))
  fec_total_count <- suppressWarnings(as.integer(dat1$pagination$count %||% NA_integer_))
  
  q_count <- build_query(1L); q_count$page <- NULL; q_count$per_page <- NULL
  n_unique <- function_fec_count_unique_link_id_count(base_url, q_count, sort = sort, per_page = 100L)
  dom <- shiny::getDefaultReactiveDomain()
  msg_unique <- sprintf("FEC reports %s unique receipts.",
                        format(n_unique, big.mark = ","))
  #if (!is.null(dom)) shiny::showNotification(msg_unique, type = "message")
  cat(green$bold("In function_fetch_fec_schedule_a: ",msg_unique, "\n"))
  
  
  if (!is.finite(total_pages) || is.na(total_pages) || total_pages < 1L) total_pages <- 1L
  
  out_list <- list(data.table::as.data.table(res1))
  n_so_far <- nrow(out_list[[1]])
  if (!quiet) {
    base_msg <- sprintf("Fetched page 1 of %d (n=%d)", total_pages, n_so_far)
    if (!is.na(fec_total_count)) base_msg <- sprintf("%s; API reports fec_total_count=%s", base_msg, format(fec_total_count, big.mark=","))
    message(base_msg)
  }
  cat(blue$bold("\nBase url: ",base_url,"\n"))
  
  if (!is.na(fec_total_count) && fec_total_count >= 5000L && !quiet) {
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
  attr(ans, "fec_total_count") <- fec_total_count
  attr(ans, "fec_unique_link_id") <- n_unique
  ans
}

# Globals ------
## Notes: -----
# Global objects (outside server()): created once per worker, shared by all users on that worker.
APP_SCHEMA_VER <- "fec-v2"   # bump whenever schema/logic changes

## Secrets ----
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
##* 1) Try shared pins (if enabled) ----
shared_enabled <- tolower(Sys.getenv("SHARED_CACHE_ENABLED", "false")) == "true"
board <- if (shared_enabled) {
  # S3 example: set AWS_S3_BUCKET and AWS_DEFAULT_REGION as env vars
  pins::board_s3(
    bucket    = Sys.getenv("AWS_S3_BUCKET"),
    region    = Sys.getenv("AWS_DEFAULT_REGION"),
    prefix    = "fec-cache/",
    versioned = FALSE
  )
} else {
  # Persistent local folder cache for dev (avoid board_temp() which is ephemeral)
  cache_dir <- Sys.getenv("FEC_CACHE_DIR", unset = tools::R_user_dir("sumsearch", "cache"))
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  pins::board_folder(cache_dir)
}

##* 2) Configure per-instance disk cache (cachem, ~24h TTL handled in wrapper) ----
fec_cache <- cache_disk(dir = file.path(tempdir(), "fec_cache"),
                        max_age = 24 * 60 * 60)

##*3) Helpers: null-coalescing and canonical cache key ----
`%||%` <- function(a, b) if (!is.null(a)) a else b

function_fec_cache_key <- function(args) {
  canon <- list(
    contributor_name = tolower(trimws(args$contributor_name %||% "")),
    min_load_date    = as.character(as.Date(args$min_load_date)),
    per_page         = as.integer(args$per_page %||% 100L),
    sort             = as.character(args$sort %||% "-contribution_receipt_date"),
    sort_hide_null   = as.logical(args$sort_hide_null %||% FALSE),
    sort_null_only   = as.logical(args$sort_null_only %||% FALSE),
    two_year_transaction_period = args$two_year_transaction_period %||% NULL,
    max_pages        = as.integer(ifelse(is.finite(args$max_pages %||% Inf), args$max_pages, -1L))
  )
  paste0("fec_", digest::digest(canon, algo = "xxhash64"))
}

##* 4) Network fetcher (keyset pagination for Schedule A) ----
function_fec_fetch_schedule_a_keyset <- function(
    contributor_name,
    min_load_date             = "2022-01-01",
    two_year_transaction_period = NULL,     # optional passthrough
    per_page                  = 100L,
    sort                      = "-contribution_receipt_date",
    sort_hide_null            = FALSE,
    sort_null_only            = FALSE,
    api_key                   = Sys.getenv("FEC_API_KEY"),
    max_pages                 = Inf,
    quiet                     = FALSE,
    progress_cb               = NULL        # function(pg, total_guess, n_rows_so_far)
) {
  stopifnot(length(per_page) == 1L, per_page %in% 1:100)
  if (!nzchar(api_key)) api_key <- "DEMO_KEY"
  
  base_url <- "https://api.open.fec.gov/v1/schedules/schedule_a/"
  q_base <- list(
    api_key          = api_key,
    contributor_name = contributor_name,
    min_load_date    = min_load_date,
    per_page         = per_page,
    sort             = sort,
    sort_hide_null   = tolower(as.character(sort_hide_null))
  )
  if (!is.null(two_year_transaction_period)) {
    q_base$two_year_transaction_period <- two_year_transaction_period
  }
  
  out <- list()
  total_count <- NA_integer_
  n_rows <- 0L
  pg <- 0L
  
  cursor_last_index <- NULL
  cursor_last_date  <- NULL
  sort_null_only_flag <- isTRUE(sort_null_only)
  seen_sigs <- character()
  
  repeat {
    pg <- pg + 1L
    if (pg > max_pages) break
    
    q <- q_base
    q$sort_null_only <- tolower(as.character(sort_null_only_flag))
    if (!is.null(cursor_last_index)) {
      q$last_index <- cursor_last_index
      if (!sort_null_only_flag && !is.null(cursor_last_date)) {
        q$last_contribution_receipt_date <- cursor_last_date
      }
    }
    
    resp <- httr::GET(base_url, query = q)
    if (httr::http_error(resp)) {
      stop("FEC error: ", httr::status_code(resp), " - ",
           httr::content(resp, as = "text", encoding = "UTF-8"))
    }
    js <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"))
    
    if (is.na(total_count)) total_count <- js$pagination$count %||% NA_integer_
    
    res <- as.data.table(js$results)
    if (nrow(res) == 0L) break
    
    # Repeated-page guard (first+last sub_id and n)
    first_id <- if ("sub_id" %in% names(res)) res$sub_id[1] else res$transaction_id[1]
    last_id  <- if ("sub_id" %in% names(res)) res$sub_id[nrow(res)] else res$transaction_id[nrow(res)]
    sig <- paste(first_id, last_id, nrow(res), sep = "::")
    if (sig %in% seen_sigs) stop(sprintf("Detected repeated page at logical page %d.", pg))
    seen_sigs <- c(seen_sigs, sig)
    
    out[[length(out) + 1L]] <- res
    n_rows <- n_rows + nrow(res)
    
    if (is.function(progress_cb)) {
      total_guess <- if (!is.na(total_count)) ceiling(total_count / per_page) else pg + 1L
      progress_cb(pg, total_guess, n_rows)
    } else if (!quiet) {
      message(sprintf("Fetched keyset page %d (n=%d). Rows so far: %s",
                      pg, nrow(res), format(n_rows, big.mark=",")))
    }
    
    li <- js$pagination$last_indexes
    if (is.null(li) || is.null(li$last_index)) break
    cursor_last_index <- li$last_index
    if (!is.null(li$last_contribution_receipt_date)) {
      cursor_last_date <- li$last_contribution_receipt_date
    } else if (!is.null(li$sort_null_only) && isTRUE(li$sort_null_only)) {
      sort_null_only_flag <- TRUE
      cursor_last_date <- NULL
    } else {
      stop("Server did not supply a usable keyset cursor.")
    }
  }
  
  dt <- rbindlist(out, use.names = TRUE, fill = TRUE)
  if ("contribution_receipt_date" %in% names(dt)) {
    suppressWarnings(dt[, contribution_receipt_date := as.POSIXct(contribution_receipt_date, tz = "UTC")])
  }
  if ("load_date" %in% names(dt)) {
    suppressWarnings(dt[, load_date := as.POSIXct(load_date, tz = "UTC")])
  }
  attr(dt, "fec_total_count") <- total_count
  dt
}

##* 5) Raw fetch wrapper (stable signature; calls keyset fetcher) ----
function_fec_fetch_raw <- function(
    contributor_name,
    min_load_date,
    two_year_transaction_period = NULL,
    per_page = 100,
    sort = "-contribution_receipt_date",
    sort_hide_null = FALSE,
    sort_null_only = FALSE,
    api_key = Sys.getenv("FEC_API_KEY"),
    max_pages = Inf,
    quiet = TRUE,
    progress_cb = NULL
) {
  function_fec_fetch_schedule_a_keyset(
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
}

##* 6) Cached fetch wrapper (shared pins -> local cache -> fetch+write) ----
function_fec_fetch_cached <- function(
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
  # build stable cache key (use canonical helper) ----
  args_for_key <- list(
    contributor_name = contributor_name,
    min_load_date    = min_load_date,
    two_year_transaction_period = two_year_transaction_period,
    per_page         = per_page,
    sort             = sort,
    sort_hide_null   = sort_hide_null,
    sort_null_only   = sort_null_only,
    max_pages        = max_pages
  )
  key <- function_fec_cache_key(args_for_key)
  
  now <- Sys.time()
  is_fresh <- function(created) isTRUE(!is.na(created)) &&
    as.numeric(difftime(now, created, units = "secs")) < ttl_secs
  
  # shared pins read ----
  if (isTRUE(shared_enabled) && !force_refresh && pins::pin_exists(board, key)) {
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
  
  # local cachem read ----
  if (!force_refresh) {
    entry <- tryCatch(fec_cache$get(key), error = function(e) NULL)
    if (data.table::is.data.table(entry)) {
      created_at <- attr(entry, "fec_cache_created")
      if (is.null(created_at)) created_at <- now
      if (is_fresh(created_at)) {
        attr(entry, "fec_cache_source")  <- "memory"
        attr(entry, "fec_cache_created") <- created_at
        attr(entry, "fec_cache_event") <- "local_cache_hit"
        return(entry)
      }
    }
  }
  
  # fetch from API (progress optional) ----
  if (!quiet) message("Cache miss or refresh; fetching from FEC API…")
  dt <- function_fec_fetch_raw(
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
  
  # write to local cachem ----
  attr(dt, "fec_cache_source")  <- "memory"
  attr(dt, "fec_cache_created") <- created_at
  suppressWarnings(fec_cache$set(key, dt))
  
  # write to shared pins (if enabled) ----
  if (isTRUE(shared_enabled)) {
    meta_user <- list(schema = APP_SCHEMA_VER,
                      created = format(created_at, tz = "UTC", usetz = TRUE))
    suppressWarnings(
      pins::pin_write(board, name = key, x = dt, type = "rds", metadata = list(user = meta_user))
    )
  }
  attr(dt, "fec_cache_event") <- "network_fetch"
  dt
}

# User UI **********************-----------------------------------------------------------
ui <- page_sidebar(
  
  useShinyjs(),
  
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
    open  = "open",   # "open" (user can close it) | "closed" (starts closed)
    width = 320,      # adjust to taste
    
    helpText("..."),
    selectInput("chamber", "Chamber to analyze:", c("House","Senate"), selected = "House"),
    tags$hr(),
  ),
  
  ## Main content ------
  ##* Tab 1: Menu  -----
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    selected = "Welcome - search",
    tabPanel(
      title = "Welcome - search",
      h3("Choose a scenario to investigate"),
      tags$ol(
        # Scenario 1
        tags$li(
          div("(under construction) A political issue has surfaced and you want to know which congressional members are receiving donations from key political action committees (PAC). Enter the key text that is included in one of more PAC names. Consider:"),
                 tags$ul(
                   tags$li("Search terms that retrieve more than one PAC: \"National Rifle Association\" or \"Planned Parenthood\" or \"Lockheed Martin\" (without quotes)."),
                   tags$li("Donors listed within relevant industries at ",tags$a(href="https://www.opensecrets.org/federal-lobbying/industries", "OpenSecret's list of industries", target = "_blank")),
                   tags$li("Testing search terms at the FEC's ",
                     tags$a(href="https://www.fec.gov/data/committees/", "Committee Names", target = "_blank"),
                     " and chose terms that may retrieve more than one relevant PACs as in the examples above")
          ),
          div(class = "li-flex",
              div(class = "input-wrap",
                  textInput("scenario1_search_string", label = NULL, value = "Lockheed Martin")
              ),
              actionButton("scenario1_go", "Fetch current FEC Receipts")
          )
        ),
        # Scenario 2
        tags$li("(Not implemented) You encountered a new political action committee (PAC) with a vague name. What is the weighted political leaning of recipients of donations from this PAC?"),
       # Scenario 3
      tags$li("(Not implemented) A member of congress makes new statements that do not align with prior stated views. What PAC money has this member received and have any of PACs increased their donations to this member?")
      )
      # plotOutput("dwplot", height = 600)
    ),
    
    ##* Tab 2: Results -----
    tabPanel(
      title = "Results",
      h3("DW-NOMINATE* scatterplot"),
      h4(textOutput("results_nrows")),
      plotOutput("dwplot", height = 600)
    ),
    ##* Tab 3: Results - details -----
    tabPanel(
      title = "Results - details",
      h3("Recipients, sorted by the total values of receipts at the Federal Elections COmmission (FEC)"),
      h4(textOutput("results_nrows")),
      div(tags$span(style="font-weight:bold;", "Warning: "),
          " data extracton from the FEC is still being optimized. Click links to receipts at the FDA to confirm sums."
      ),
      tags$div("Tools: ", style = "font-weight: bold;"),
               tags$ul(tags$li(
                       tags$a(href = "https://fecnotify.fec.gov/fecnotify/login/", "Get notified by the FEC of a new filing by a specific committee or candidate", target = "_blank")
                              ),
                       tags$li("Select row(s) in the table for candidates to have their initials next to their points on the DW-NOMINATE plot (not yet implemented).")
      ),
      DTOutput("dt_voteview_chamber_temp_candidates_with_topic"),
      tags$br()
    ),
    ### Tab 4: About / Sources / Help -----
    tabPanel(
      title = "About / Sources / Help",
      h3("About / Sources / Help"),
      ###* Data sources ----
      h4("Data sources:"),
      tags$div("This is an open source project with code available for suggestions or copying at", tags$a(href="https://github.com/badgettrg/SumSearch", "GitHub", target = "_blank")),
      tags$h3("FEC.gov"),
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
          tags$a(href = "https://www.fec.gov/data/receipts/", "Receipts", target = "_blank"), " (search using the campaign committees above)",
        )
        ),
        tags$li(
          tags$a(href = "https://fecnotify.fec.gov/fecnotify/register/", "Create tracker", target = "_blank")
        )
      ),
      tags$h3("VoteView.gov"),
      tags$ul(
        tags$li(
          tags$a(href = "https://voteview.com/data", "Data", target = "_blank")
        )
      ),
      tags$h3("GOVTRACK.us"),
      tags$ul(
        tags$li(
          tags$a(href = "https://www.govtrack.us/congress/members/current", "Congressional members list", target = "_blank")
        ),
        tags$li(
          tags$a(href = "https://www.govtrack.us/accounts/lists", "Create tracker", target = "_blank")
        )
      ),
      tags$h3("opensecrets.org"),
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
      ###* Help ----
      tags$h4("Help:"),
      tags$ul(
        tags$li(
          tags$a(href = "https://github.com/badgettrg/SumSearch/issues", "Submit an issue at GitHub (requires a Google or GitHub account)", target = "_blank")
        ),
        tags$li(
          tags$a(href = "https://github.com/badgettrg/SumSearch", "Source code at GitHub", target = "_blank")
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
# Notes:
# Session objects (inside server()): created once per user session, safe to mutate since they’re isolated.
server <- function(input, output, session) {
  
  # Reactive values for later display on tabs -----
  rv <- reactiveValues(min_receipts_date = NULL, max_date = NULL, topic = NULL)
  rv <- reactiveValues(dwplot_data = NULL)
  
  # Load data -----
  ##* VoteView.com data -----
  if (1 ==2){
    dt_voteview <- tryCatch(
      {
        data.table::fread("https://voteview.com/static/data/out/members/HS119_members.csv")
      },
      error = function(e) {
        stop("❌ Error: Could not load file from VoteView (HS119_members.csv). Please check the URL or your internet connection.")
      }
    )
  } else{
    dt_voteview <- fread("VoteView.com/HS119_members.csv")
  }

  cat(
    crayon::green$bold(
      "dt_voteview: total rows = ", nrow(dt_voteview),
      "; rows with chamber = House = ",
      sum(dt_voteview$chamber == "House", na.rm = TRUE),
      "\n"
    )
  )
  
  if (interactive()) utils::View(utils::head(dt_voteview, 100))
  

  ##* CCL.txt file from the FEC -----
  dt_FEC_candidate_committee_linkage <- fread("FEC.gov/ccl.txt", sep = "|", header = FALSE,
                                              col.names = c("candidate_id", "cand_election_yr", "fec_election_yr",
                                                            "committee_id", "committee_type", "committee_designation",
                                                            "linkage_id"))
  
  

  dt_FEC_candidate_committee_linkage[, candidate_id := as.character(candidate_id)]
  dt_FEC_candidate_committee_linkage[, committee_id := as.character(committee_id)]

  if (interactive()) utils::View(utils::head(dt_FEC_candidate_committee_linkage, 1000))
  
  cat(green$bold("\nccl.txt: ", nrow(dt_FEC_candidate_committee_linkage), " records\n"))

  # FEC receipts
  dtr_fec_receipts <- reactiveVal(NULL)
  
  # _____________ -----

  
  # run_scenario1() -----
  run_scenario1 <- function() {
    shinyjs::disable("scenario1_go")
    on.exit(shinyjs::enable("scenario1_go"), add = TRUE)

    # Parameters for searching: ------
    rv$topic <- topic <- input$scenario1_search_string

    cat(green$bold("\nParameter: input$scenario1_search_string: "), black(input$scenario1_search_string))
    cat(green$bold("\nParameter: input$chamber: "), black(input$chamber))
    
    # Date policy: House window start to now
    min_receipts_date <- as.character(function_house_window_start(Sys.Date()))
    rv$min_receipts_date <- min_receipts_date
    cat(green$bold("\nParameter: min_receipts_date: "), black(min_receipts_date,"\n"))
    
    # Parameters for troubleshooting -----
    troubleshoot_candidate_id <- "H0LA01087"
    troubleshoot_bioname      <- "SCALISE, Steve"
    troubleshoot_committees   <- c('C00394957','C00568162')
    # OR #
    troubleshoot_candidate_id <- "H8NY06048"
    troubleshoot_bioname      <- "MEEKS, Gregory W."
    troubleshoot_committees   <- c('C00430991')

    if (input$scenario1_search_string == "National Rifle Association"){
      troubleshoot_candidate_id <- "H0LA01087"
      troubleshoot_bioname      <- "SCALISE, Steve"
      troubleshoot_committees   <- c('C00394957','C00568162')
    }
    if (input$scenario1_search_string == "Planned Parenthood"){
      troubleshoot_candidate_id <- "H0LA01087"
      troubleshoot_bioname      <- "SCALISE, Steve"
      troubleshoot_committees   <- c('C00394957','C00568162')
    }      
    # Build "(A|B|C)" from troubleshoot_committees + troubleshoot_bioname ----
    vals <- c(unlist(troubleshoot_committees, use.names = FALSE), troubleshoot_bioname)
    vals <- unique(vals[!is.na(vals) & nzchar(trimws(as.character(vals)))])
    tracking_pattern <- if (length(vals) > 0) paste0("(", paste(vals, collapse = "|"), ")") else NULL
    
    cat(green$bold("\ntracking_pattern: "), tracking_pattern, "\n")
    
    ## VoteView.com chamber restriction -----
    chamber_input <- input$chamber
    ch_letter <- switch(chamber_input,
                        "House"  = "H",
                        "Senate" = "S",
                        "All"    = NA_character_
    )

    dt_voteview_chamber_temp <- dt_voteview[chamber == input$chamber]
    
    cat(green$bold("\ndt_voteview_chamber_temp rows: ", nrow(dt_voteview_chamber_temp),"\n"))
    
    # 1. FEC receipts data ----------------------
    cat(green$bold("\nFEC search start:\n"))
    # --- inputs & normalization ---
    scenario1_search_string <- input$scenario1_search_string
    scenario1_search_string <- if (is.null(scenario1_search_string)) "" else trimws(scenario1_search_string)
    if (identical(scenario1_search_string, "")) {
      showNotification("Please enter a PAC or organization name.", type = "warning")
      return(invisible(NULL))
    }
    
    per_page <- as.integer(input$scenario1_per_page %||% 100L)
    sort_val <- "-contribution_receipt_date"
    sort_hide_null <- FALSE
    sort_null_only <- FALSE
    FEC_API_KEY <- Sys.getenv("FEC_API_KEY")
    
    ###* quick fec_total_count (1-row request) to inform user -----
    fec_total_count <- NA_integer_
    tryCatch({
      base_url <- "https://api.open.fec.gov/v1/schedules/schedule_a/"
      q1 <- list(
        api_key          = FEC_API_KEY,
        per_page         = 1L,
        page             = 1L,
        sort             = sort_val,
        sort_hide_null   = tolower(as.character(sort_hide_null)),
        sort_null_only   = tolower(as.character(sort_null_only)),
        contributor_name = scenario1_search_string,
        min_load_date    = min_receipts_date,
        two_year_transaction_period = NULL
      )
      
      resp1 <- function_fec_get(base_url, q1)
      full_url <- httr::modify_url(base_url, query = q1)
      
      # ✅ redaction without regex foot-guns
      q1_redacted <- q1; q1_redacted$api_key <- "DEMO_KEY"
      full_url_redacted <- httr::modify_url(base_url, query = q1_redacted)
      
      cat(green$bold("\nFEC url: "), underline(full_url_redacted), "\n")
      
      dat1  <- jsonlite::fromJSON(httr::content(resp1, as = "text", encoding = "UTF-8"), flatten = TRUE)
      fec_total_count <- suppressWarnings(as.integer(dat1$pagination$count %||% NA_integer_))
    }, error = function(e) {
      # show a visible clue in your console and UI
      cat(crayon::red$bold("\nError while preparing preview request: ", conditionMessage(e), "\n"))
      n_txt <- "an unknown number of"
      #msg1  <- sprintf('The FEC reports %s candidate receipts by PACs containing "%s" in the name since %s.',
      #                 n_txt, scenario1_search_string, min_receipts_date)
      #showNotification(msg1, type = "error", duration = NULL)
    })
    
    msg1  <- sprintf('The FEC reports %s candidate receipts by PACs containing "%s" in the name since %s.',
                     fec_total_count, scenario1_search_string, min_receipts_date)
    showNotification(msg1, id = "msg_fec_total_count", type = "message")

    ##* Cache available? ------
    # build EXACT key used by your shared cache (must match fetch args) ---
    args_for_key <- list(
      contributor_name = tolower(trimws(scenario1_search_string)),
      min_load_date    = min_receipts_date,
      #two_year_transaction_period = NULL,
      #two_year_transaction_period = c(2024,2026),
      #min_election_date=2024,
      #max_election_date=2026,
      sort             = sort_val,
      sort_hide_null   = sort_hide_null,
      sort_null_only   = sort_null_only,
      per_page         = per_page,
      max_pages        = Inf
    )
    key <- function_fec_cache_key(args_for_key)
    
    shared_fresh_hit <- FALSE

    ###* Checking cache using pins-package {pins} -----
    if (isTRUE(shared_enabled) && pins::pin_exists(board, key)) {
      meta <- tryCatch(pins::pin_meta(board, key), error = function(e) NULL)
      created_at <- suppressWarnings(as.POSIXct(meta$user$created %||% meta$created, tz = "UTC"))
      if (!is.null(meta) && identical(meta$user$schema, APP_SCHEMA_VER)) {
        age <- suppressWarnings(as.numeric(difftime(Sys.time(), created_at, units = "secs")))
        shared_fresh_hit <- isTRUE(!is.na(age) && age < 24 * 60 * 60)
      }
    }
    
    ###* SINGLE FETCH PATH  ----
    # Only call the network if we don't have a fresh shared pin; otherwise read the pin.
    if (isTRUE(shared_enabled) && isTRUE(shared_fresh_hit)) {
      # hit: read from shared pins (no network)
      dt_fec_receipts <- pins::pin_read(board, key)
      # function_notify_ui_and_console("Loaded from shared cache (pins).","message")
    } else {
      # miss (or shared disabled): use cached wrapper (local cachem + writes to pins if enabled)

      withProgress(message = "Loading receipts...", value = 0, {
        prog_cb <- function(pg, total, n_so_far) {
          v <- if (total > 0) pg / total else 0
          prev <- get0(".__fec_last_v", ifnotfound = 0)
          incProgress(amount = max(0, v - prev),
                      detail  = sprintf("%s receipts received so far", format(n_so_far, big.mark=",")))
          assign(".__fec_last_v", v, envir = .GlobalEnv)
        }
        assign(".__fec_last_v", 0, envir = .GlobalEnv)
        
        dt_fec_receipts <- function_fec_fetch_cached(
          contributor_name = scenario1_search_string,
          min_load_date    = min_receipts_date,
          #two_year_transaction_period = NULL,
          per_page         = per_page,
          sort             = sort_val,
          sort_hide_null   = sort_hide_null,
          sort_null_only   = sort_null_only,
          api_key          = FEC_API_KEY,
          max_pages        = Inf,
          ttl_secs         = 24 * 60 * 60,
          force_refresh    = FALSE,
          quiet            = FALSE,
          progress_cb      = prog_cb
        )
        
        if (exists(".__fec_last_v", envir = .GlobalEnv)) {
          remove(".__fec_last_v", envir = .GlobalEnv)
        }
      })
    }
    

    # Create message after we know FEC receipts source (cache?) ----
    ev <- attr(dt_fec_receipts, "fec_cache_event") %||% "unknown"
    if (identical(ev, "local_cache_hit")) {
      function_notify_ui_and_console("Loaded from local cache (cachem).", "message")
    } else {
      function_notify_ui_and_console("Downloaded from FEC.gov API.", "message")
    }    
    
    cat(
      blue$bold(
        "\n[Cache] source: ", attr(dt_fec_receipts, "fec_cache_source") %||% "none",
        "; created (UTC): ",
        format(
          as.POSIXct(attr(dt_fec_receipts, "fec_cache_created"), tz = "UTC"),
          "%Y-%m-%d %H:%M:%S %Z"
        ),
        " (now: ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC"),
        ")",
        "; rows: ", nrow(dt_fec_receipts),
        "\n"
      )
    )
    
    rv$max_date <- max(dt_fec_receipts$load_date)
    
    cat(crayon::green$bold("\n",scenario1_search_string, "has",  
                           nrow(dt_fec_receipts), 
                           "rows with the most recent entry", 
                           rv$max_date,".\n"))
    
    showNotification(sprintf("%s receipts ready for analysis and will be stored for quick retreival during your current session.", 
                             format(nrow(dt_fec_receipts), big.mark=",")),
                     type = "message")

    ###*!! xlsx to local drive if interactive (before) -----
    function_fec_current_download ("1. Before cleaning", tracking_pattern = tracking_pattern, scenario1_search_string = scenario1_search_string, dt_temp = dt_fec_receipts)

    # Remove rows with receipt dates before min_receipts_date
    # Normalize to a simple character vector once
    s <- gsub("\\s+", " ", as.character(dt_fec_receipts$contribution_receipt_date))
    
    # Detect column format from the first non-empty value
    first <- s[which(nzchar(s))[1L]]
    
    if (grepl("^\\d{4}-\\d{2}-\\d{2}$", first)) {
      # ISO date, no time: 2025-07-28
      dt_fec_receipts[, receipt_idate := as.IDate(s)]
    } else if (grepl("^\\d{4}/\\d{2}/\\d{2}\\s+\\d{2}:\\d{2}:\\d{2}$", first)) {
      # Y/m/d HH:MM:SS: 2025/07/28 00:00:00
      dt_fec_receipts[, receipt_idate := as.IDate(
        as.POSIXct(s, format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
      )]
    } else {
      # m/d/Y h:m:s AM/PM: 7/28/2025 12:00:00 AM
      dt_fec_receipts[, receipt_idate := as.IDate(
        as.POSIXct(s, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
      )]
    }
    
    # Parse your threshold (works if you pass "YYYY-MM-DD" or the AM/PM form)
    min_idate <- tryCatch(
      as.IDate(min_receipts_date),
      error = function(...) as.IDate(as.POSIXct(min_receipts_date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC"))
    )
    
    # Filter
    dt_fec_receipts <- dt_fec_receipts[receipt_idate >= min_idate]
    
    # (Optional) sanity check
    cat("Receipt date range after parse:", min(dt_fec_receipts$receipt_idate),
        "to", max(dt_fec_receipts$receipt_idate), "\n")
    
    # Notify counts
    function_notify_ui_and_console(
      paste0("Receipts after removing earliest date criterion: ",
             format(nrow(dt_fec_receipts), big.mark=",")),
      type = "message"
    )

    ###*!! xlsx to local drive if interactive (old receipts)-----
    function_fec_current_download ("2. After min_receipts_date", tracking_pattern = tracking_pattern, scenario1_search_string = scenario1_search_string, dt_temp = dt_fec_receipts)
    
    ###* Remove national committees, WinRed/ActBlue ---------------------------
    # These committees do not accurately tag the receiving candidate
    remove_committeesby_ID <- c('C00003418','C00010603', # National
                                'C00027466','C00042366', # Senate
                                'C00075820','C00000935', # House
                                'C00694323','C00401224' # WinRed, ActBlue
    )
    
    dt_fec_receipts <- dt_fec_receipts[!(committee_id %in% remove_committeesby_ID)]
    
    function_notify_ui_and_console(
      paste0("Receipts after removing national committees: ",
             format(nrow(dt_fec_receipts), big.mark=",")),
      type = "message"
    )

    # WORK HERE post-fetch notifications ---
    cache_src     <- attr(dt_fec_receipts, "fec_cache_source")
    cache_created <- attr(dt_fec_receipts, "fec_cache_created")
    #if (!is.null(cache_src) && cache_src %in% c("shared","memory")) {
    #  if (identical(cache_src, "shared") && !is.null(cache_created)) {
    #    showNotification(sprintf("Using cached data from shared store (created %s, ≤24h).", cache_created),
    #                     type = "message")
    #  } else {
    #    showNotification("Using cached data (in-memory, ≤24h).", type = "message")
    #  }
    #}
    
    total_count_attr <- attr(dt_fec_receipts, "fec_total_count")
    if (!is.null(total_count_attr) && !is.na(total_count_attr) && total_count_attr >= 5000L) {
      showNotification("⚠ FEC may cap results at ~5,000 rows. Try narrowing the date window or query.",
                       type = "warning")
    }

    val_raw <- attr(dt_fec_receipts, "fec_unique_link_id", exact = TRUE)
    
    if (is.null(val_raw)) {
      cat(crayon::red$bold("\nReceipts with unique link_id: unavailable\n"))
    } else {
      val_num <- suppressWarnings(as.integer(val_raw))
      if (is.na(val_num)) {
        cat(crayon::red$bold("\nReceipts with unique link_id: unavailable\n"))
      } else {
        cat(crayon::green$bold("\nReceipts with unique link_id: ",
                               format(val_num, big.mark=","), "\n"))
      }
    }
    
    ###*!! xlsx to local drive if interactive -----
    function_fec_current_download ("4. After removal of nationals", tracking_pattern = tracking_pattern, scenario1_search_string = scenario1_search_string, dt_temp = dt_fec_receipts)

    #2. Merge data sources ----
    cat(red$bgYellow$bold("\nMerge prep FEC and selected voteview chamber (dt_voteview_chamber_temp)\n"))

    ###* unitedstates.io for crosswalk dt_crosswalk_flat.csv ----- 
    `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
    .get_char_vec <- function(x) as.character(unlist(x %||% character(0)))
    
    dt_crosswalk_flat <- read.csv('unitedstates.io/cw_flat.csv')
    setDT(dt_crosswalk_flat)
    
    cw_icpsr    <- dt_crosswalk_flat[!is.na(icpsr) & !duplicated(icpsr)]
    cw_bioguide <- dt_crosswalk_flat[!is.na(bioguide_id) & !duplicated(bioguide_id),
                                     .(bioguide_id, fec_candidate_id)]

    ###* Merge FEC IDs into VoteView ------------------
    # Make sure ICPSR is an integer in both tables
    dt_voteview_chamber_temp[, icpsr := as.integer(icpsr)]
    cw_icpsr[, icpsr := as.integer(icpsr)]
    
    # 1) Primary merge by ICPSR
    dt_voteview_chamber_temp_candidates <- merge(
      dt_voteview_chamber_temp,
      cw_icpsr[, .(icpsr, fec_candidate_id)],  # only keep needed cols
      by = "icpsr",
      all.x = TRUE
    )
    
    cat(
      crayon::green$bold(
        "dt_voteview_chamber_temp_candidates: total rows = ", nrow(dt_voteview_chamber_temp_candidates),
        "; rows with chamber = House = ",
        sum(dt_voteview_chamber_temp_candidates$chamber == "House", na.rm = TRUE),
        "\n"
      )
    )

        # 2) Secondary fill by Bioguide (if column exists in dt_voteview)
    if ("bioguide_id" %in% names(dt_voteview_chamber_temp_candidates)) {
      dt_voteview_chamber_temp_candidates <- merge(
        dt_voteview_chamber_temp_candidates,
        cw_bioguide,    # bioguide_id → fec_candidate_id
        by = "bioguide_id",
        all.x = TRUE,
        suffixes = c("", "_bioguide")
      )
      # Fill missing fec_candidate_id with bioguide match
      dt_voteview_chamber_temp_candidates[, fec_candidate_id := fifelse(
        is.na(fec_candidate_id),
        fec_candidate_id_bioguide,
        fec_candidate_id
      )]
      dt_voteview_chamber_temp_candidates[, fec_candidate_id_bioguide := NULL]  # clean up
    }
    
    ## Data checks -----
    if (sum(!is.na(dt_voteview_chamber_temp_candidates$fec_candidate_id)) != nrow(dt_voteview_chamber_temp_candidates)){
      cat(red$bold("Matched FEC IDs for", sum(!is.na(dt_voteview_chamber_temp_candidates$fec_candidate_id)),
                   "of", nrow(dt_voteview_chamber_temp_candidates), "VoteView rows\n"))
      cat(red$bold("Explanation: Marco Rubio and J.D. Vance left the senate\n"))
    }else{
      cat(green$bold("Matched FEC IDs for", sum(!is.na(dt_voteview_chamber_temp_candidates$fec_candidate_id)),
                     "of", nrow(dt_voteview_chamber_temp_candidates), "VoteView rows\n"))
    }
    

    # Robust topic flag across possible org-name fields
    topic_pattern <- str_replace_all(input$scenario1_search_string, " ", "\\\\s+")
    topic_flag <- 
      grepl(topic_pattern, dt_fec_receipts[["donor_committee_name"]], ignore.case = TRUE) |
      grepl(topic_pattern, dt_fec_receipts[["contributor_name"]], ignore.case = TRUE) |
      grepl(topic_pattern, dt_fec_receipts[["conduit_committee_name"]], ignore.case = TRUE)
    
    cat(green$bold("\ntopic_pattern: ",topic_pattern,"\n"))

    dt_fec_receipts_topic <- dt_fec_receipts[
      !is.na(contribution_receipt_date) &
        contribution_receipt_date >= as.IDate(min_receipts_date) &
        topic_flag == TRUE
    ]

    cat(green("\nAfter date criterion: nrow(dt_fec_receipts_topic): ", nrow(dt_fec_receipts_topic),"\n"))

    tracking_n_rows <- dt_fec_receipts_topic[`committee_id` %chin% as.character(unlist(troubleshoot_committees, use.names = FALSE)), .N]
    cat(green("\nAfter date AND tracking pattern: nrow(dt_fec_receipts_topic): ", tracking_n_rows,"\n"))

    cat(green$bold("\nAggregate topic receipts by the candidates' committee_id\n"))
    
    dt_fec_receipts_topic_by_cmte <- dt_fec_receipts_topic[
      !is.na(committee_id) & nzchar(committee_id),
      .(
        topic_receipts_count = .N,
        topic_receipts = sum(contribution_receipt_amount, na.rm = TRUE)
      ),
      by = .(committee_id)
    ]
    
    cat(green("\nAfter date criterion: nrow(dt_fec_receipts_topic_by_cmte): ", nrow(dt_fec_receipts_topic_by_cmte),"\n"))

    tracking_n_rows <- dt_fec_receipts_topic_by_cmte[`committee_id` %chin% as.character(unlist(troubleshoot_committees, use.names = FALSE)), .N]
    cat(green("\nAfter date AND tracking pattern: nrow(dt_fec_receipts_topic_by_cmte): ", tracking_n_rows,"\n"))
    
    dt_fec_receipts_topic_by_cand <-
      merge(
        dt_fec_receipts_topic_by_cmte,
        unique(dt_FEC_candidate_committee_linkage[, .(committee_id, candidate_id)]),
        by = "committee_id",
        all.x = TRUE
      )[
        !is.na(candidate_id),
        .(
          topic_receipts       = sum(topic_receipts,       na.rm = TRUE),
          topic_receipts_count = sum(topic_receipts_count, na.rm = TRUE),
          committee_count      = .N   # use uniqueN(committee_id) if you want distinct committees
        ),
        by = candidate_id
      ]
    
    ###*!! xlsx to local drive if interactive -----
    function_fec_current_download ("5. All done", tracking_pattern = tracking_pattern, scenario1_search_string = scenario1_search_string, dt_temp = dt_fec_receipts_topic_by_cand)

        cat(green$bold("\nAfter date criterion: nrow(dt_fec_receipts_topic_by_cand): ", nrow(dt_fec_receipts_topic_by_cand),"\n"))
      
    #tracking_n_rows <- dt_fec_receipts_topic_by_cand[`committee_id` %chin% as.character(unlist(dt_fec_receipts_topic_by_cand, use.names = FALSE)), .N]
    #cat(green("\nAfter date AND tracking pattern: nrow(dt_fec_receipts_topic_by_cand): ", tracking_n_rows,"\n"))

    rhs <- if (input$chamber == "All") {
      dt_fec_receipts_topic_by_cand[
        !is.na(candidate_id),
        .(candidate_id, topic_receipts)
      ]
    } else {
      dt_fec_receipts_topic_by_cand[
        substr(candidate_id, 1, 1) == ch_letter & !is.na(candidate_id),
        .(candidate_id, topic_receipts)
      ]
    }
    
    # Defensive de-dupe by candidate_id (no aggregation, just ensure one row)
    rhs <- unique(rhs, by = "candidate_id")
    
    # Quick checks (console)
    dup_right <- rhs[, .N, by = candidate_id][N > 1]
    dup_left  <- dt_voteview_chamber_temp_candidates[
      !is.na(fec_candidate_id), .N, by = fec_candidate_id][N > 1]
    
    if (nrow(dup_right)) cat(red$bold("Right side (RHS( still has duplicate candidate_id (unexpected).\n"))
    if (nrow(dup_left))  cat(red$bold("Left side (LHS) has duplicate fec_candidate_id; that will multiply rows.\n"))
    
    ## ***Final quality check *****-----
    cat(red$bgYellow$bold("\n\n\nFinal quality check\n"))
    nrow(dt_voteview_chamber_temp_candidates[dt_voteview_chamber_temp_candidates$chamber == "Senate"])
    # Both chambers, receiving money for topic
    length(unique(dt_fec_receipts_topic_by_cand$candidate_id))

    # Selected chamber, receiving money for topic
    n_temp <- nrow(dt_fec_receipts_topic_by_cand[substr(candidate_id, 1, 1) == ch_letter])
    cat(green$bold("dt_fec_receipts_topic_by_cand rows: "), black(n_temp,"\n"))
    
    dt_voteview_chamber_temp_candidates_with_topic <- merge(
      dt_voteview_chamber_temp_candidates,
      dt_fec_receipts_topic_by_cand[
        substr(candidate_id, 1, 1) == ch_letter,
        .(candidate_id, topic_receipts, topic_receipts_count)
      ],
      by.x = "fec_candidate_id",
      by.y = "candidate_id",
      all.x = TRUE
    )
    
    # Replace missing receipts with 0
    dt_voteview_chamber_temp_candidates_with_topic[
      is.na(topic_receipts), topic_receipts := 0
    ]
    
    ## dt_voteview_chamber_temp_candidates_with_topic ----
    dt_voteview_chamber_temp_candidates_with_topic <- 
      dt_voteview_chamber_temp_candidates_with_topic[
        , topic_receipts := fifelse(is.na(topic_receipts), 0, topic_receipts)
      ][
        topic_receipts > 0
      ][
        , `:=`(
          product      = as.numeric(nominate_dim1) * as.numeric(topic_receipts),
          chamber_name = fifelse(chamber == "H", "House",
                                 fifelse(chamber == "S", "Senate", "Other")),
          party_html   = fifelse(
            party_code == 200, "<span style='color:#cc0000;'>Republican</span>",
            fifelse(party_code == 100, "<span style='color:#0066cc;'>Democrat</span>",
                    "<span style='color:#777777;'>Other</span>"))
        )
      ][
        order(-product)
      ]

    # Append URLs to details -----
    # FEC_candidate_link
    dt_voteview_chamber_temp_candidates_with_topic[
      , FEC_candidate_link := paste0("https://www.fec.gov/data/candidate/", fec_candidate_id)
    ]    
    output$dt_voteview_chamber_temp_candidates_with_topic <- DT::renderDT({
      d <- copy(dt)  # your prepared table (with the anchor-tag column already)
      
      d[, Select := ""]
      data.table::setcolorder(d, c("Select", setdiff(names(d), "Select")))
      html_cols <- c("FEC_candidate_link")# columns that contain HTML
      
      DT::datatable(
        d,
        rownames   = FALSE,
        escape     = setdiff(seq_along(d), which(names(d) %in% html_cols)),
        selection  = list(style = "multi", selector = "td:first-child"),
        options    = list(
          dom = "tip",
          columnDefs = list(
            list(orderable = FALSE, className = "select-checkbox", targets = 0)
          )
        )
      )
    })
    
    # Use selected rows to drive your plot:
    observeEvent(input$dt_voteview_chamber_temp_candidates_with_topic_rows_selected, {
      sel <- input$dt_voteview_chamber_temp_candidates_with_topic_rows_selected %||% integer()
      #dt_sel <- dt[sel]
      # ... plot dt_sel ...
    })
    
    # FEC_receipts_link
    dt_comm_params <- dt_FEC_candidate_committee_linkage[
      !is.na(candidate_id) & !is.na(committee_id),
      .(committee_params = paste0("&committee_id=", unique(committee_id), collapse = "")),
      by = candidate_id
    ]
    
    dt_voteview_chamber_temp_candidates_with_topic <- merge(
      dt_voteview_chamber_temp_candidates_with_topic,
      dt_comm_params,
      by.x = "fec_candidate_id",
      by.y = "candidate_id",
      all.x = TRUE
    )
    
    #topic_pattern <- str_replace_all(input$scenario1_search_string, "\\s+", "+")
    
    cat(green$bold("\ninput$scenario1_search_string: "), black(input$scenario1_search_string, "\n"))
    cat(green$bold("\nrv$min_receipts_date: "), black(rv$min_receipts_date, "\n"))
    
    dt_voteview_chamber_temp_candidates_with_topic[
      , FEC_receipts_link := paste0( 
        "https://www.fec.gov/data/receipts/?data_type=processed", 
        fifelse(is.na(committee_params), "", committee_params), 
        "&min_date=", format(as.Date(rv$min_receipts_date), "%m/%d/%Y"), "&contributor_name=", 
        str_replace_all(input$scenario1_search_string, " ", "+")) ]    

    ## Check for outliers ----
    x <- dt_voteview_chamber_temp_candidates_with_topic$topic_receipts
    mu <- mean(x)
    sigma <- sd(x)
    z_scores <- (x - mu) / sigma
    # Flag outliers: |z| > 2 (common cutoff), |z| > 3 (strict)
    dt_voteview_chamber_temp_candidates_with_topic[, outlier_normal := abs(z_scores) > 2]
    
    if (interactive()) utils::View(utils::head(dt_voteview_chamber_temp_candidates_with_topic, 10))
    
    output$dt_voteview_chamber_temp_candidates_with_topic <- DT::renderDT({
      # If your table is created earlier as a plain data.table:
      req(exists("dt_voteview_chamber_temp_candidates_with_topic"))
      req(data.table::is.data.table(dt_voteview_chamber_temp_candidates_with_topic))
      shiny::validate(shiny::need(nrow(dt_voteview_chamber_temp_candidates_with_topic) > 0,
                    "No rows to display."))
      
      keep_cols <- c("bioname", "chamber", "party_html", "state_abbrev", "topic_receipts_count", "topic_receipts", "FEC_candidate_link", "FEC_receipts_link") #, "outlier_normal")
      dt_voteview_chamber_temp_candidates_with_topic <- dt_voteview_chamber_temp_candidates_with_topic[, ..keep_cols]

      dt_voteview_chamber_temp_candidates_with_topic[
        , FEC_candidate_link := sprintf(
          "<a href='%s' target='_blank'>Candidate at FEC</a>", FEC_candidate_link
        )
      ]
      
      dt_voteview_chamber_temp_candidates_with_topic[
        , FEC_receipts_link := sprintf(
          "<a href='%s' target='_blank'>Receipts at FEC</a>", FEC_receipts_link
        )
      ]

      data.table::setorder(dt_voteview_chamber_temp_candidates_with_topic, -topic_receipts)
      
      DT::datatable(
        dt_voteview_chamber_temp_candidates_with_topic,
        colnames = c("Name", "Chamber", "Party", "State", "Receipts (#)", "Receipts ($)", "Candidate overview<br>at FEC","Candidate receipts<br>at FEC<br>(if no receipts at FEC, refresh the FEC page in your browser)"), #,"Outlier ($)"),
        options = list(
          pageLength = 50,
          scrollX    = TRUE,
          autoWidth  = TRUE
        ),
        rownames = FALSE,
        escape   = FALSE
      )
    })
    
    #dt_voteview_chamber_temp_candidates_with_topic   
    
    
    #* Plot ----
    rv$dwplot_data <- list(
      all   = data.table::copy(dt_voteview_chamber_temp_candidates),
      topic = data.table::copy(dt_voteview_chamber_temp_candidates_with_topic)
    )
    
    #* Wrap up ----
    if (interactive())if (interactive()) utils::View(utils::head(dt_fec_receipts, 10))
    dtr_fec_receipts(dt_fec_receipts)
    
      updateActionButton(
        session = getDefaultReactiveDomain(),
        "scenario1_go",
        disabled = FALSE
      )
      #** Close notifications-----
      removeNotification("msg_fec_total_count", session = getDefaultReactiveDomain())
      #** Switch panel tab -----
      updateTabsetPanel(session, inputId = "main_tabs", selected = "Results")
  } # end of run_scenario1() -----

  ## _____________ -----

  ## Outputs ----------------------
  ##* output$dwplot-----
  output$dwplot <- renderPlot({
    plot_data <- req(rv$dwplot_data)
    all   <- data.table::as.data.table(plot_data$all)
    topic <- data.table::as.data.table(plot_data$topic)

    cat(green$bold("nrow(all): "), black(nrow(all)))
    shiny::validate(shiny::need(nrow(all) > 0, "No data available for the selected filters."))
    
    need_cols <- c("nominate_dim1","nominate_dim2","party_code")
    missing <- setdiff(need_cols, names(all))
    shiny::validate(shiny::need(length(missing) == 0, paste("Missing columns:", paste(missing, collapse = ", "))))
    
    cols <- ifelse(all$party_code == 200, "red",
                   ifelse(all$party_code == 100, "blue", "gray"))
    
    op <- par(no.readonly = TRUE); on.exit(par(op))
    par(oma = c(3, 0, 0, 0))
    
    plot(all$nominate_dim1, all$nominate_dim2,
         col = cols, 
         pch = 1, # open circle
         xlab = "DW-NOMINATE Dimension 1: Economic/Redistributive",
         ylab = "DW-NOMINATE Dimension 2: Other Votes",
         main = paste0(input$scenario1_search_string, ": DW-NOMINATE Plot for ", input$chamber))
    
    # Members all
    points(all$nominate_dim1,
           all$nominate_dim2,
           col = cols,
           pch = 1)

    mtext("Liberal",      side = 1, line = 2, adj = 0, font = 2, col = "gray")
    mtext("Conservative", side = 1, line = 2, adj = 1, font = 2, col = "gray")
    mtext("Liberal",      side = 2, line = 2, adj = 0, font = 2, col = "gray")
    mtext("Conservative", side = 2, line = 2, adj = 1, font = 2, col = "gray")
    
    # solid points for congress members with receipts
    cols <- ifelse(topic$party_code == 200, "red",
                   ifelse(topic$party_code == 100, "blue", "gray"))
    
    # Members with receipts
    points(topic$nominate_dim1,
           topic$nominate_dim2,
           col = cols,
           pch = 19)
    
    # Outliers
    # Only plot outliers
    with(
      topic[outlier_normal == TRUE],
      points(
        nominate_dim1,
        nominate_dim2,
        col = "black",   # override party colors
        pch = 19,
        cex = 0.5        # 50% size
      )
    )
    
    
    mtext("Notes:", side = 1, line = -1, adj = 0, outer = TRUE, cex = 1.2, font = 2)
    mtext("* Plot made with data from https://voteview.com/data and other sources by bob.badgett@gmail.com",
          side = 1, line = 0, adj = 0, outer = TRUE, cex = 1.2)
    mtext("* Plot design is from https://voteview.com/congress/senate",
          side = 1, line = 1, adj = 0, outer = TRUE, cex = 1.2)
    mtext(paste("Printed:", Sys.Date()),
          side = 1, line = 2, adj = 1, outer = TRUE, cex = 1.2)
  })
  
  ##* output$results_min_date -----
  output$results_min_date <- renderText({
    req(rv$min_receipts_date)
  })
  
  ##* output$results_nrows -----
  output$results_nrows <- renderText({
    fec_total_count <- attr(dtr_fec_receipts(), "fec_total_count")
    
    if (!is.null(fec_total_count) && !is.na(fec_total_count)) {
      paste0(
        rv$topic, ": receipts received from FEC:",
        " (total: ", format(as.numeric(fec_total_count), big.mark=","), ")",
        " since ", rv$min_receipts_date, 
        " through ", strftime(rv$max_date, tz = "America/New_York", format = "%Y-%m-%d %Z")
      )
    } else {
      paste0("Receipts received from FEC for \"", input$scenario1_search_string, "\" since ", rv$min_receipts_date)
    }
  })

  ## _____________ -----
  # ObserveEvents -----
  #* input$scenario1_go ------
  observeEvent(input$scenario1_go, 
               run_scenario1(), 
               ignoreInit = TRUE)

  #** Log needed -----
  # COnsider whether to and how to
  
  #* input$chamber ------
  observeEvent(input$chamber, {
    active_tab <- NULL
    for (id in c("main_tabs","results_tabs","navbar","tabs")) {
      if (!is.null(input[[id]])) { active_tab <- input[[id]]; break }
    }
    if (!is.null(active_tab) && active_tab %in% c("Results - details", "Results")) {
      run_scenario1()
    }
  }, ignoreInit = TRUE)  
  
}# end of server function -----

# ___________________________-----
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
