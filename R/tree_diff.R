canonical_json <- function(value) {
  if (is.null(value)) {
    return("null")
  }

  toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = 17,
    keep_vec_names = TRUE,
    ensure_ascii = FALSE,
    pretty = FALSE,
    force = TRUE
  )
}

is_scalarish <- function(x) {
  is.null(x) || (!is.list(x) && length(x) == 1L)
}

is_object_like <- function(x) {
  is.list(x) && !is.null(names(x))
}

is_array_like <- function(x) {
  is.list(x) && is.null(names(x))
}

parse_format_input <- function(raw_text, format, label = "Input") {
  text <- trimws(raw_text)
  if (!nzchar(text)) stop(sprintf("%s %s input is empty.", label, toupper(format)), call. = FALSE)
  parser <- switch(format,
    json = parse_json_input,
    xml = parse_xml_input,
    csv = parse_csv_input,
    stop(sprintf("Unsupported input format: %s", format), call. = FALSE)
  )
  parser(if (identical(format, "csv")) raw_text else text)
}

path_join <- function(pointer, key, format = "json", parent = NULL) {
  switch(format,
    json = json_path_join(pointer, key),
    xml = xml_path_join(pointer, key, parent),
    csv = csv_path_join(pointer, key, parent),
    stop(sprintf("Unsupported input format: %s", format), call. = FALSE)
  )
}

array_parent_path <- function(pointer, parent, format) {
  switch(format,
    json = pointer,
    xml = xml_array_parent_path(pointer, parent),
    csv = pointer,
    stop(sprintf("Unsupported input format: %s", format), call. = FALSE)
  )
}

compute_format_diff <- function(base_text, compare_text, format) {
  tryCatch(
    {
      base_data <- parse_format_input(base_text, format, "Base")
      compare_data <- parse_format_input(compare_text, format, "Compare")
      diff_value(base_data, compare_data, format = format)
    },
    error = function(err) list(error = conditionMessage(err))
  )
}

flatten_diff_results <- function(results) {
  out <- list()
  for (item in results) {
    if (is.list(item) && length(item) > 0L) {
      out <- c(out, item)
    }
  }
  out
}

sort_diff_results <- function(results) {
  if (length(results) == 0L) {
    return(list())
  }

  ordering <- order(vapply(results, function(item) {
    toJSON(
      item,
      auto_unbox = TRUE,
      null = "null",
      na = "null",
      digits = 17,
      keep_vec_names = TRUE,
      ensure_ascii = FALSE,
      pretty = FALSE,
      force = TRUE
    )
  }, character(1), USE.NAMES = FALSE))

  results[ordering]
}

diff_scalar <- function(base_value, compare_value, base_pointer = "", compare_pointer = "") {
  if (identical(base_value, compare_value)) {
    return(list())
  }

  list(list(
    op = "replace",
    path_base = base_pointer,
    path_compare = compare_pointer,
    value_base = base_value,
    value_compare = compare_value
  ))
}

diff_object <- function(base_object, compare_object, base_pointer = "", compare_pointer = "", format = "json") {
  base_keys <- names(base_object)
  compare_keys <- names(compare_object)
  if (is.null(base_keys)) base_keys <- character(0)
  if (is.null(compare_keys)) compare_keys <- character(0)

  deletions <- lapply(setdiff(base_keys, compare_keys), function(key) {
    list(
      op = "remove",
      path_base = path_join(base_pointer, key, format, base_object),
      path_compare = compare_pointer,
      value_base = base_object[[key]]
    )
  })

  additions <- lapply(setdiff(compare_keys, base_keys), function(key) {
    list(
      op = "add",
      path_base = base_pointer,
      path_compare = path_join(compare_pointer, key, format, compare_object),
      value_compare = compare_object[[key]]
    )
  })

  common_keys <- sort(intersect(base_keys, compare_keys))
  updates <- list()
  for (key in common_keys) {
    inner_updates <- diff_value(
      base_object[[key]],
      compare_object[[key]],
      path_join(base_pointer, key, format, base_object),
      path_join(compare_pointer, key, format, compare_object),
      format
    )
    if (length(inner_updates) > 0L) {
      updates <- c(updates, inner_updates)
    }
  }

  sort_diff_results(c(deletions, additions, updates))
}

diff_array <- function(base_array, compare_array, base_pointer = "", compare_pointer = "", format = "json") {
  matches <- match_array_items(base_array, compare_array)
  matched_base <- unique(vapply(matches, function(pair) pair[[1]], integer(1), USE.NAMES = FALSE))
  matched_compare <- unique(vapply(matches, function(pair) pair[[2]], integer(1), USE.NAMES = FALSE))

  base_unmatched <- setdiff(seq_along(base_array), matched_base)
  compare_unmatched <- setdiff(seq_along(compare_array), matched_compare)

  deletions <- lapply(base_unmatched, function(index) {
    list(
      op = "remove",
      path_base = path_join(base_pointer, as.character(index - 1L), format, base_array),
      path_compare = array_parent_path(compare_pointer, compare_array, format),
      value_base = base_array[[index]]
    )
  })

  additions <- lapply(compare_unmatched, function(index) {
    list(
      op = "add",
      path_base = array_parent_path(base_pointer, base_array, format),
      path_compare = path_join(compare_pointer, as.character(index - 1L), format, compare_array),
      value_compare = compare_array[[index]]
    )
  })

  moves <- lapply(matches, function(pair) {
    base_index <- pair[[1]]
    compare_index <- pair[[2]]

    if (base_index == compare_index) {
      return(NULL)
    }

    list(
      op = "move",
      path_base = path_join(base_pointer, as.character(base_index - 1L), format, base_array),
      path_compare = path_join(compare_pointer, as.character(compare_index - 1L), format, compare_array),
      value_base = base_array[[base_index]],
      value_compare = compare_array[[compare_index]]
    )
  })
  moves <- Filter(Negate(is.null), moves)

  updates <- list()
  for (pair in matches) {
    base_index <- pair[[1]]
    compare_index <- pair[[2]]

    inner_updates <- diff_value(
      base_array[[base_index]],
      compare_array[[compare_index]],
      path_join(base_pointer, as.character(base_index - 1L), format, base_array),
      path_join(compare_pointer, as.character(compare_index - 1L), format, compare_array),
      format
    )

    if (length(inner_updates) > 0L) {
      updates <- c(updates, inner_updates)
    }
  }

  sort_diff_results(c(deletions, additions, moves, updates))
}

diff_value <- function(base_value, compare_value, base_pointer = "", compare_pointer = "", format = "json") {
  if (is.null(base_value) || is.null(compare_value) ||
    (is_scalarish(base_value) && is_scalarish(compare_value))) {
    result <- diff_scalar(base_value, compare_value, base_pointer, compare_pointer)
    return(sort_diff_results(result))
  }

  if (is_object_like(base_value) && is_object_like(compare_value)) {
    return(diff_object(base_value, compare_value, base_pointer, compare_pointer, format))
  }

  if (is_array_like(base_value) && is_array_like(compare_value)) {
    return(diff_array(base_value, compare_value, base_pointer, compare_pointer, format))
  }

  if (identical(base_value, compare_value)) {
    return(list())
  }

  sort_diff_results(diff_scalar(base_value, compare_value, base_pointer, compare_pointer))
}
