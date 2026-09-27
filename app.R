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

ui <- shiny::fluidPage(
  title = "tree-diff-r JSON diff v0.0.1",
  shiny::includeCSS("www/styles.css"),
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
          shiny::fileInput("base_file", "Upload base JSON", accept = c(".json", ".txt"), width = "100%"),
          shiny::textAreaInput(
            "base_text",
            NULL,
            value = base_sample_text,
            width = "100%",
            height = "290px"
          )
        )
      ),
      shiny::div(
        class = "panel",
        shiny::div(class = "panel-header", "Compare JSON"),
        shiny::div(
          class = "panel-body",
          shiny::fileInput("compare_file", "Upload compare JSON", accept = c(".json", ".txt"), width = "100%"),
          shiny::textAreaInput(
            "compare_text",
            NULL,
            value = compare_sample_text,
            width = "100%",
            height = "290px"
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
        shiny::span("live")
      ),
      shiny::div(
        class = "result-body",
        shiny::tabsetPanel(
          shiny::tabPanel("Structured diff", shiny::verbatimTextOutput("diff_output")),
          shiny::tabPanel("Summary table", shiny::tableOutput("diff_table"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  diff_result <- shiny::reactiveVal(compute_diff(base_sample_text, compare_sample_text))

  reconstruct_diff <- function() {
    base_text <- input$base_text
    compare_text <- input$compare_text
    diff_result(compute_diff(base_text, compare_text))
  }

  shiny::observeEvent(input$base_file, {
    if (!is.null(input$base_file$datapath)) {
      base_text <- paste(readLines(input$base_file$datapath, warn = FALSE), collapse = "\n")
      shiny::updateTextAreaInput(session, "base_text", value = base_text)
      reconstruct_diff()
    }
  })

  shiny::observeEvent(input$compare_file, {
    if (!is.null(input$compare_file$datapath)) {
      compare_text <- paste(readLines(input$compare_file$datapath, warn = FALSE), collapse = "\n")
      shiny::updateTextAreaInput(session, "compare_text", value = compare_text)
      reconstruct_diff()
    }
  })

  shiny::observeEvent(input$run_diff, {
    reconstruct_diff()
  })

  output$diff_output <- shiny::renderPrint({
    result <- diff_result()
    if (is.null(result) || !is.list(result)) {
      return(invisible())
    }

    if (!is.null(result$error)) {
      cat(result$error)
      return(invisible())
    }

    cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"))
  })

  output$diff_table <- shiny::renderTable(
    {
      result <- diff_result()
      if (is.null(result) || !is.list(result) || !is.null(result$error) || length(result) == 0L) {
        return(data.frame(op = character(), path_base = character(), path_compare = character()))
      }

      data.frame(
        op = vapply(result, `[[`, character(1), "op", USE.NAMES = FALSE),
        path_base = vapply(result, `[[`, character(1), "path_base", USE.NAMES = FALSE),
        path_compare = vapply(result, `[[`, character(1), "path_compare", USE.NAMES = FALSE),
        stringsAsFactors = FALSE
      )
    },
    rownames = FALSE
  )
}

options(shiny.launch.browser = TRUE)
shiny::shinyApp(ui = ui, server = server)
