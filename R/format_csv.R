parse_csv_input <- function(raw_text) {
    text <- sub("^\\ufeff", "", raw_text)
    if (!nzchar(trimws(text))) {
        records <- data.frame()
    } else {
        connection <- textConnection(text)
        on.exit(close(connection))
        records <- read.csv(
            connection,
            stringsAsFactors = FALSE,
            check.names = FALSE,
            colClasses = "character",
            na.strings = character(),
            strip.white = FALSE,
            fill = FALSE,
            comment.char = ""
        )
    }

    headers <- names(records)
    if (length(headers) != length(unique(headers))) {
        stop("CSV column names must be unique.", call. = FALSE)
    }

    rows <- lapply(seq_len(nrow(records)), function(row_index) {
        as.list(records[row_index, , drop = FALSE])
    })
    names(rows) <- NULL
    attr(rows, "tree_diff_segment") <- "csv_rows"
    columns <- as.list(headers)
    names(columns) <- NULL
    attr(columns, "tree_diff_segment") <- "csv_columns"

    result <- list(columns = columns, row = rows)
    attr(result, "tree_diff_root") <- "csv"
    result
}

csv_escape_path_segment <- function(key) {
    characters <- strsplit(as.character(key), "", fixed = TRUE)[[1]]
    escaped <- vapply(characters, function(character) {
        if (character %in% c("\\", ",")) paste0("\\", character) else character
    }, character(1), USE.NAMES = FALSE)
    paste0(escaped, collapse = "")
}

csv_path_join <- function(pointer, key, parent = NULL) {
    parent_segment <- attr(parent, "tree_diff_segment")
    if (identical(parent_segment, "csv_rows")) {
        if (!nzchar(key)) {
            return(pointer)
        }
        return(as.character(as.integer(key) + 1L))
    }
    if (identical(parent_segment, "csv_columns")) {
        if (!nzchar(key)) {
            return(pointer)
        }
        return(csv_escape_path_segment(parent[[as.integer(key) + 1L]]))
    }
    if (identical(attr(parent, "tree_diff_root"), "csv") && key %in% c("columns", "row")) {
        return("")
    }
    if (!nzchar(pointer) || !nzchar(key)) {
        return(if (nzchar(key)) csv_escape_path_segment(key) else pointer)
    }
    paste0(pointer, ",", csv_escape_path_segment(key))
}
