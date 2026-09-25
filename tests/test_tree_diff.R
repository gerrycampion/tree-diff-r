source(file.path("..", "R", "tree_diff.R"), chdir = TRUE)

base_json <- jsonlite::fromJSON(file.path("..", "samples", "base.json"), simplifyVector = FALSE)
compare_json <- jsonlite::fromJSON(file.path("..", "samples", "compare.json"), simplifyVector = FALSE)
expected_json <- jsonlite::fromJSON(file.path("..", "samples", "diff_paths.json"), simplifyVector = FALSE)

actual <- diff_value(base_json, compare_json)
actual_paths <- lapply(actual, function(item) {
  list(
    op = item[["op"]],
    path_base = item[["path_base"]],
    path_compare = item[["path_compare"]]
  )
})

if (!identical(actual_paths, expected_json)) {
  stop("tree-diff-r output does not match the expected sample diff.")
}

cat("tree-diff-r sample test passed\n")
