source(file.path("R", "tree_diff.R"))

parse_json_input <- function(raw_text, label) {
  text <- trimws(raw_text)
  if (identical(text, "")) {
    stop(sprintf("%s JSON input is empty.", label), call. = FALSE)
  }

  jsonlite::fromJSON(text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
}

compute_diff <- function(base_text, compare_text) {
  tryCatch(
    {
      base_data <- parse_json_input(base_text, "Base")
      compare_data <- parse_json_input(compare_text, "Compare")
      diff_value(base_data, compare_data)
    },
    error = function(err) {
      list(error = conditionMessage(err))
    }
  )
}

base_sample_text <- paste(readLines(file.path("samples", "base.json"), warn = FALSE), collapse = "\n")
compare_sample_text <- paste(readLines(file.path("samples", "compare.json"), warn = FALSE), collapse = "\n")

pretty_json_text <- function(raw_text) {
  text <- trimws(raw_text)
  if (!nzchar(text)) {
    return(text)
  }

  tryCatch(
    {
      parsed <- jsonlite::fromJSON(text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
      jsonlite::toJSON(parsed, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
    },
    error = function(err) {
      raw_text
    }
  )
}

as_diff_rows <- function(result) {
  if (is.null(result) || !is.list(result) || !is.null(result$error) || length(result) == 0L) {
    return(data.frame(
      row_index = integer(),
      op = character(),
      path_base = character(),
      path_compare = character(),
      value_base = character(),
      value_compare = character(),
      stringsAsFactors = FALSE
    ))
  }

  data.frame(
    row_index = seq_along(result),
    op = vapply(result, function(item) if (is.null(item$op)) "" else item$op, character(1), USE.NAMES = FALSE),
    path_base = vapply(result, function(item) if (is.null(item$path_base)) "" else item$path_base, character(1), USE.NAMES = FALSE),
    path_compare = vapply(result, function(item) if (is.null(item$path_compare)) "" else item$path_compare, character(1), USE.NAMES = FALSE),
    value_base = vapply(result, function(item) {
      val <- item$value_base
      if (is.null(val)) "" else jsonlite::toJSON(val, auto_unbox = TRUE, null = "null", na = "null", digits = 17, keep_vec_names = TRUE, ensure_ascii = FALSE)
    }, character(1), USE.NAMES = FALSE),
    value_compare = vapply(result, function(item) {
      val <- item$value_compare
      if (is.null(val)) "" else jsonlite::toJSON(val, auto_unbox = TRUE, null = "null", na = "null", digits = 17, keep_vec_names = TRUE, ensure_ascii = FALSE)
    }, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

ui_text <- function(value) {
  if (is.null(value)) {
    return("")
  }

  raw <- paste(as.character(value), collapse = " ")
  htmltools::HTML(htmltools::htmlEscape(raw))
}

row_state_class <- function(op) {
  if (identical(op, "add")) {
    return("diff-add")
  }
  if (identical(op, "remove")) {
    return("diff-remove")
  }
  "diff-update"
}

ui <- shiny::fluidPage(
  title = "tree-diff-r JSON diff v0.0.1",
  shiny::includeCSS("www/styles.css"),
  shiny::includeScript("www/sync.js"),
  shiny::div(
    class = "app-shell",
    shiny::div(
      class = "topbar",
      shiny::div(
        class = "brand",
        "tree-diff-r",
        shiny::tags$small("JSON diff explorer")
      )
    ),
    shiny::div(
      class = "editor-grid",
      shiny::div(
        class = "panel",
        shiny::div(class = "panel-header", "Base JSON"),
        shiny::div(
          class = "panel-body",
          shiny::div(
            class = "file-row",
            shiny::fileInput("base_file", "Upload base JSON", accept = c(".json", ".txt"), width = "100%"),
            shiny::actionButton("prettify_base", "Prettify", class = "btn-secondary")
          ),
          shinyAce::aceEditor(
            "base_text",
            value = base_sample_text,
            mode = "json",
            theme = "tomorrow_night",
            height = "360px",
            debounce = 100,
            fontSize = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = TRUE,
            showPrintMargin = FALSE,
            showInvisibles = FALSE,
            tabSize = 2,
            useSoftTabs = TRUE,
            wordWrap = FALSE,
            minLines = 20,
            maxLines = 40
          )
        )
      ),
      shiny::div(
        class = "panel",
        shiny::div(class = "panel-header", "Compare JSON"),
        shiny::div(
          class = "panel-body",
          shiny::div(
            class = "file-row",
            shiny::fileInput("compare_file", "Upload compare JSON", accept = c(".json", ".txt"), width = "100%"),
            shiny::actionButton("prettify_compare", "Prettify", class = "btn-secondary")
          ),
          shinyAce::aceEditor(
            "compare_text",
            value = compare_sample_text,
            mode = "json",
            theme = "tomorrow_night",
            height = "360px",
            debounce = 100,
            fontSize = 13,
            showLineNumbers = TRUE,
            highlightActiveLine = TRUE,
            showPrintMargin = FALSE,
            showInvisibles = FALSE,
            tabSize = 2,
            useSoftTabs = TRUE,
            wordWrap = FALSE,
            minLines = 20,
            maxLines = 40
          )
        )
      )
    ),
    shiny::div(
      class = "toolbar",
      shiny::actionButton("run_diff", "Compare JSON", class = "btn-primary")
    ),
    shiny::div(
      class = "result-panel",
      shiny::div(
        class = "result-header",
        shiny::span("Diff output"),
        shiny::span(class = "result-status", shiny::uiOutput("diff_status_badge"))
      ),
      shiny::div(
        class = "result-body",
        shiny::tabsetPanel(
          shiny::tabPanel("Structured diff", shiny::uiOutput("diff_output")),
          shiny::tabPanel("Summary table", shiny::uiOutput("diff_table")),
          shiny::tabPanel("Detailed table", shiny::uiOutput("diff_detail_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  diff_result <- shiny::reactiveVal(compute_diff(base_sample_text, compare_sample_text))
  diff_status <- shiny::reactiveVal("live")
  selected_diff_row <- shiny::reactiveVal(NULL)
  last_computed <- shiny::reactiveValues(
    base = base_sample_text,
    compare = compare_sample_text
  )

  reconstruct_diff <- function() {
    base_text <- input$base_text
    compare_text <- input$compare_text
    result <- compute_diff(base_text, compare_text)
    diff_result(result)
    last_computed$base <- base_text
    last_computed$compare <- compare_text
    diff_status(if (!is.null(result$error)) "error" else "live")
  }

  shiny::observe(
    {
      current_base <- input$base_text
      current_compare <- input$compare_text

      if (!is.null(current_base) && !is.null(current_compare)) {
        if (!identical(current_base, last_computed$base) || !identical(current_compare, last_computed$compare)) {
          diff_status("stale")
        }
      }
    },
    priority = 100
  )

  shiny::observeEvent(input$base_file, {
    if (!is.null(input$base_file$datapath)) {
      base_text <- paste(readLines(input$base_file$datapath, warn = FALSE), collapse = "\n")
      shinyAce::updateAceEditor(session, "base_text", value = base_text)
      reconstruct_diff()
    }
  })

  shiny::observeEvent(input$compare_file, {
    if (!is.null(input$compare_file$datapath)) {
      compare_text <- paste(readLines(input$compare_file$datapath, warn = FALSE), collapse = "\n")
      shinyAce::updateAceEditor(session, "compare_text", value = compare_text)
      reconstruct_diff()
    }
  })

  shiny::observeEvent(input$prettify_base, {
    pretty_text <- pretty_json_text(input$base_text)
    shinyAce::updateAceEditor(session, "base_text", value = pretty_text)
    reconstruct_diff()
  })

  shiny::observeEvent(input$prettify_compare, {
    pretty_text <- pretty_json_text(input$compare_text)
    shinyAce::updateAceEditor(session, "compare_text", value = pretty_text)
    reconstruct_diff()
  })

  shiny::observeEvent(input$run_diff, {
    reconstruct_diff()
  })

  shiny::observeEvent(input$selected_diff_row,
    {
      index <- suppressWarnings(as.integer(input$selected_diff_row))
      if (!is.na(index) && index >= 1L) {
        selected_diff_row(index)
      }
    },
    ignoreNULL = TRUE
  )

  shiny::observe({
    index <- selected_diff_row()
    result <- diff_result()

    if (is.null(index) || is.null(result) || !is.list(result) || !is.null(result$error)) {
      return()
    }

    if (length(result) < index) {
      return()
    }

    item <- result[[index]]
    if (is.null(item)) {
      return()
    }

    session$sendCustomMessage(
      "sync-diff-selection",
      list(
        operation = if (is.null(item$op)) "" else item$op,
        base_path = if (is.null(item$path_base)) "" else item$path_base,
        compare_path = if (is.null(item$path_compare)) "" else item$path_compare,
        base_text = input$base_text,
        compare_text = input$compare_text
      )
    )
  })

  output$diff_status_badge <- shiny::renderUI({
    status <- diff_status()

    if (identical(status, "live")) {
      shiny::tags$span("live", class = "status-live")
    } else if (identical(status, "stale")) {
      shiny::tags$span("stale", class = "status-stale")
    } else {
      shiny::tags$span("error", class = "status-error")
    }
  })

  render_diff_table <- function(rows, selected_index = NULL, show_values = TRUE) {
    if (nrow(rows) == 0L) {
      return(shiny::tags$div(class = "empty-state", "No diff items."))
    }

    rows_html <- lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      is_selected <- !is.null(selected_index) && selected_index == row$row_index
      class_name <- paste(c("diff-table-row", row_state_class(row$op), if (is_selected) "selected" else NULL), collapse = " ")

      base_cells <- list(
        shiny::tags$td(ui_text(row$op)),
        shiny::tags$td(ui_text(row$path_base)),
        shiny::tags$td(ui_text(row$path_compare))
      )

      if (isTRUE(show_values)) {
        base_cells <- c(
          base_cells,
          list(
            shiny::tags$td(ui_text(row$value_base)),
            shiny::tags$td(ui_text(row$value_compare))
          )
        )
      }

      shiny::tags$tr(
        class = class_name,
        `data-row-index` = row$row_index,
        onclick = "Shiny.setInputValue('selected_diff_row', this.dataset.rowIndex, {priority: 'event'});",
        shiny::tagList(base_cells)
      )
    })

    base_headers <- list(
      shiny::tags$th("Op"),
      shiny::tags$th("Base path"),
      shiny::tags$th("Compare path")
    )

    if (isTRUE(show_values)) {
      base_headers <- c(
        base_headers,
        list(
          shiny::tags$th("Base value"),
          shiny::tags$th("Compare value")
        )
      )
    }

    shiny::tags$table(
      class = "diff-table",
      shiny::tags$thead(
        shiny::tags$tr(shiny::tagList(base_headers))
      ),
      shiny::tags$tbody(shiny::tagList(rows_html))
    )
  }

  output$diff_output <- shiny::renderUI({
    result <- diff_result()
    if (is.null(result) || !is.list(result) || !is.null(result$error)) {
      return(shiny::tags$pre("No diff available."))
    }

    rows <- as_diff_rows(result)
    if (nrow(rows) == 0L) {
      return(shiny::tags$pre("No changes detected."))
    }

    items <- lapply(seq_len(nrow(rows)), function(i) {
      row <- rows[i, , drop = FALSE]
      is_selected <- !is.null(selected_diff_row()) && selected_diff_row() == row$row_index
      class_name <- paste(c("diff-output-item", row_state_class(row$op), if (is_selected) "selected" else NULL), collapse = " ")
      label <- sprintf("[%s] %s -> %s", row$op, row$path_base, row$path_compare)
      shiny::tags$div(
        class = class_name,
        `data-row-index` = row$row_index,
        onclick = "Shiny.setInputValue('selected_diff_row', this.dataset.rowIndex, {priority: 'event'});",
        ui_text(label)
      )
    })

    shiny::tags$div(class = "diff-output-list", shiny::tagList(items))
  })

  output$diff_table <- shiny::renderUI({
    result <- diff_result()
    rows <- as_diff_rows(result)
    render_diff_table(rows, selected_diff_row(), show_values = FALSE)
  })

  output$diff_detail_table <- shiny::renderUI({
    result <- diff_result()
    rows <- as_diff_rows(result)
    render_diff_table(rows, selected_diff_row(), show_values = TRUE)
  })
}

options(shiny.launch.browser = TRUE)
shiny::shinyApp(ui = ui, server = server)
