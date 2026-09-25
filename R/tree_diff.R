canonical_json <- function(value) {
  if (is.null(value)) {
    return("null")
  }

  jsonlite::toJSON(
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

path_join <- function(base_pointer, key) {
  if (is.null(base_pointer) || base_pointer == "") {
    if (is.null(key) || key == "") {
      return("/")
    }
    return(paste0("/", key))
  }

  if (is.null(key) || key == "") {
    return(base_pointer)
  }

  paste0(base_pointer, "/", key)
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
    jsonlite::toJSON(
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

ngram_tokens <- function(text, gram_size) {
  if (is.null(text) || is.na(text) || nchar(text) == 0L) {
    return(character(0))
  }

  text <- gsub("\\n", " ", text, fixed = TRUE)
  if (nchar(text) <= gram_size) {
    return(text)
  }

  start_positions <- seq_len(nchar(text) - gram_size + 1L)
  vapply(start_positions, function(pos) substr(text, pos, pos + gram_size - 1L), character(1), USE.NAMES = FALSE)
}

ngram_similarity <- function(left, right, gram_size = 2L) {
  left_tokens <- ngram_tokens(as.character(left), gram_size)
  right_tokens <- ngram_tokens(as.character(right), gram_size)

  if (length(left_tokens) == 0L || length(right_tokens) == 0L) {
    return(0)
  }

  intersection_size <- length(intersect(left_tokens, right_tokens))
  union_size <- length(unique(c(left_tokens, right_tokens)))

  if (union_size == 0L) {
    return(0)
  }

  intersection_size / union_size
}

match_array_items <- function(base_array, compare_array) {
  base_text <- vapply(seq_along(base_array), function(i) canonical_json(base_array[[i]]), character(1), USE.NAMES = FALSE)
  compare_text <- vapply(seq_along(compare_array), function(i) canonical_json(compare_array[[i]]), character(1), USE.NAMES = FALSE)

  base_lookup <- split(seq_along(base_text), base_text)
  compare_lookup <- split(seq_along(compare_text), compare_text)

  shared_keys <- sort(unique(c(names(base_lookup), names(compare_lookup))))
  matches <- list()
  matched_base <- rep(FALSE, length(base_text))
  matched_compare <- rep(FALSE, length(compare_text))

  for (key in shared_keys) {
    base_matches <- base_lookup[[key]]
    compare_matches <- compare_lookup[[key]]

    if (is.null(base_matches) || is.null(compare_matches)) {
      next
    }

    limit <- min(length(base_matches), length(compare_matches))
    for (idx in seq_len(limit)) {
      base_idx <- base_matches[[idx]]
      compare_idx <- compare_matches[[idx]]

      if (!matched_base[[base_idx]] && !matched_compare[[compare_idx]]) {
        matches[[length(matches) + 1L]] <- c(base_idx, compare_idx)
        matched_base[[base_idx]] <- TRUE
        matched_compare[[compare_idx]] <- TRUE
      }
    }
  }

  remaining_base <- which(!matched_base)
  remaining_compare <- which(!matched_compare)

  if (length(remaining_base) > 0L && length(remaining_compare) > 0L) {
    gram_size <- max(2L, min(12L, floor(mean(nchar(c(base_text, compare_text)), na.rm = TRUE) / 2)))

    while (length(remaining_base) > 0L && length(remaining_compare) > 0L) {
      best_score <- -1
      best_pair <- NULL

      for (base_idx in remaining_base) {
        for (compare_idx in remaining_compare) {
          score <- ngram_similarity(base_text[[base_idx]], compare_text[[compare_idx]], gram_size)
          if (score > best_score) {
            best_score <- score
            best_pair <- c(base_idx, compare_idx)
          }
        }
      }

      if (is.null(best_pair) || best_score <= 0) {
        break
      }

      matches[[length(matches) + 1L]] <- best_pair
      matched_base[[best_pair[[1]]]] <- TRUE
      matched_compare[[best_pair[[2]]]] <- TRUE
      remaining_base <- setdiff(remaining_base, best_pair[[1]])
      remaining_compare <- setdiff(remaining_compare, best_pair[[2]])
    }
  }

  matches
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

diff_object <- function(base_object, compare_object, base_pointer = "", compare_pointer = "") {
  base_keys <- names(base_object)
  compare_keys <- names(compare_object)
  if (is.null(base_keys)) base_keys <- character(0)
  if (is.null(compare_keys)) compare_keys <- character(0)

  deletions <- lapply(setdiff(base_keys, compare_keys), function(key) {
    list(
      op = "remove",
      path_base = path_join(base_pointer, key),
      path_compare = path_join(compare_pointer, key),
      value_base = base_object[[key]]
    )
  })

  additions <- lapply(setdiff(compare_keys, base_keys), function(key) {
    list(
      op = "add",
      path_base = path_join(base_pointer, key),
      path_compare = path_join(compare_pointer, key),
      value_compare = compare_object[[key]]
    )
  })

  common_keys <- sort(intersect(base_keys, compare_keys))
  updates <- list()
  for (key in common_keys) {
    inner_updates <- diff_value(
      base_object[[key]],
      compare_object[[key]],
      path_join(base_pointer, key),
      path_join(compare_pointer, key)
    )
    if (length(inner_updates) > 0L) {
      updates <- c(updates, inner_updates)
    }
  }

  sort_diff_results(c(deletions, additions, updates))
}

diff_array <- function(base_array, compare_array, base_pointer = "", compare_pointer = "") {
  matches <- match_array_items(base_array, compare_array)
  matched_base <- unique(vapply(matches, function(pair) pair[[1]], integer(1), USE.NAMES = FALSE))
  matched_compare <- unique(vapply(matches, function(pair) pair[[2]], integer(1), USE.NAMES = FALSE))

  base_unmatched <- setdiff(seq_along(base_array), matched_base)
  compare_unmatched <- setdiff(seq_along(compare_array), matched_compare)

  deletions <- lapply(base_unmatched, function(index) {
    list(
      op = "remove",
      path_base = path_join(base_pointer, as.character(index - 1L)),
      path_compare = path_join(compare_pointer, ""),
      value_base = base_array[[index]]
    )
  })

  additions <- lapply(compare_unmatched, function(index) {
    list(
      op = "add",
      path_base = path_join(base_pointer, ""),
      path_compare = path_join(compare_pointer, as.character(index - 1L)),
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
      path_base = path_join(base_pointer, as.character(base_index - 1L)),
      path_compare = path_join(compare_pointer, as.character(compare_index - 1L)),
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
      path_join(base_pointer, as.character(base_index - 1L)),
      path_join(compare_pointer, as.character(compare_index - 1L))
    )

    if (length(inner_updates) > 0L) {
      updates <- c(updates, inner_updates)
    }
  }

  sort_diff_results(c(deletions, additions, moves, updates))
}

diff_value <- function(base_value, compare_value, base_pointer = "", compare_pointer = "") {
  if (is.null(base_value) || is.null(compare_value) ||
      (is_scalarish(base_value) && is_scalarish(compare_value))) {
    result <- diff_scalar(base_value, compare_value, base_pointer, compare_pointer)
    return(sort_diff_results(result))
  }

  if (is_object_like(base_value) && is_object_like(compare_value)) {
    return(diff_object(base_value, compare_value, base_pointer, compare_pointer))
  }

  if (is_array_like(base_value) && is_array_like(compare_value)) {
    return(diff_array(base_value, compare_value, base_pointer, compare_pointer))
  }

  if (identical(base_value, compare_value)) {
    return(list())
  }

  sort_diff_results(diff_scalar(base_value, compare_value, base_pointer, compare_pointer))
}
