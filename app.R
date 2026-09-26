source(file.path("R", "tree_diff.R"))

parse_json_input <- function(raw_text, label) {
  text <- trimws(raw_text)
  if (identical(text, "")) {
    stop(sprintf("%s JSON input is empty.", label), call. = FALSE)
  }

  jsonlite::fromJSON(text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
}

ui <- shiny::fluidPage(
  shiny::titlePanel("tree-diff-r JSON diff", windowTitle = "tree-diff-r JSON diff v0.0.1"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 4,
      shiny::fileInput("base_file", "Upload base JSON file", accept = c(".json", ".txt")),
      shiny::fileInput("compare_file", "Upload compare JSON file", accept = c(".json", ".txt")),
      shiny::hr(),
      shiny::textAreaInput(
        "base_text",
        "Base JSON",
        value = '{"project":"demo","status":"draft"}',
        width = "100%",
        height = "220px"
      ),
      shiny::textAreaInput(
        "compare_text",
        "Compare JSON",
        value = '{"project":"demo","status":"published"}',
        width = "100%",
        height = "220px"
      ),
      shiny::actionButton("run_diff", "Compute diff", class = "btn-primary")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Diff output",
          shiny::verbatimTextOutput("diff_output")
        ),
        shiny::tabPanel(
          "Summary table",
          shiny::tableOutput("diff_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  diff_result <- shiny::eventReactive(input$run_diff, {
    tryCatch(
      {
        base_text <- if (!is.null(input$base_file$datapath)) {
          paste(readLines(input$base_file$datapath, warn = FALSE), collapse = "\n")
        } else {
          input$base_text
        }

        compare_text <- if (!is.null(input$compare_file$datapath)) {
          paste(readLines(input$compare_file$datapath, warn = FALSE), collapse = "\n")
        } else {
          input$compare_text
        }

        base_data <- parse_json_input(base_text, "Base")
        compare_data <- parse_json_input(compare_text, "Compare")
        diff_value(base_data, compare_data)
      },
      error = function(err) {
        list(error = conditionMessage(err))
      }
    )
  })

  output$diff_output <- shiny::renderPrint({
    result <- diff_result()
    if (!is.null(result$error)) {
      cat(result$error)
      return()
    }

    cat(jsonlite::toJSON(result, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"))
  })

  output$diff_table <- shiny::renderTable(
    {
      result <- diff_result()
      if (!is.null(result$error) || length(result) == 0L) {
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

shiny::shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
