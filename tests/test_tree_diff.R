source(file.path("..", "R", "tree_diff.R"), chdir = TRUE)

base_json <- jsonlite::fromJSON(file.path("..", "samples", "json", "base.json"), simplifyVector = FALSE)
compare_json <- jsonlite::fromJSON(file.path("..", "samples", "json", "compare.json"), simplifyVector = FALSE)
expected_json <- jsonlite::fromJSON(file.path("..", "samples", "json", "diff_paths.json"), simplifyVector = FALSE)

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

xml_attribute_diff <- compute_format_diff(
  '<catalog version="1"/>',
  '<catalog version="2"/>',
  "xml"
)
if (!identical(xml_attribute_diff[[1]]$path_base, "/catalog/@version")) {
  stop("XML attributes should use XPath-style paths.")
}

xml_text_diff <- compute_format_diff(
  "<root><item>A</item><item>B</item></root>",
  "<root><item>B</item><item>A</item></root>",
  "xml"
)
if (!all(vapply(xml_text_diff, function(item) grepl("/root/item\\[[12]\\]$", item$path_base), logical(1)))) {
  stop("Repeated XML elements should include one-based element indexes.")
}

xml_add_diff <- compute_format_diff(
  "<root><tags><tag>A</tag></tags></root>",
  "<root><tags><tag>A</tag><tag>B</tag></tags></root>",
  "xml"
)
if (length(xml_add_diff) != 1L || !identical(xml_add_diff[[1]]$path_base, "/root/tags[1]") ||
  !identical(xml_add_diff[[1]]$path_compare, "/root/tags[1]/tag[2]")) {
  stop("XML additions should use a valid parent path and indexed target path.")
}

xml_remove_diff <- compute_format_diff(
  "<root><tags><tag>A</tag><tag>B</tag></tags></root>",
  "<root><tags><tag>A</tag></tags></root>",
  "xml"
)
if (length(xml_remove_diff) != 1L || !identical(xml_remove_diff[[1]]$path_base, "/root/tags[1]/tag[2]") ||
  !identical(xml_remove_diff[[1]]$path_compare, "/root/tags[1]")) {
  stop("XML removals should use a valid parent path on the missing side.")
}

csv_diff <- compute_format_diff(
  "name,value\nalpha,1\n",
  "name,value\nalpha,2\n",
  "csv"
)
if (length(csv_diff) != 1L || !identical(csv_diff[[1]]$path_base, "1,value")) {
  stop("CSV cells should use row,column paths.")
}

csv_header <- paste0('"a,b', intToUtf8(92), 'c"', intToUtf8(10))
csv_escaped_diff <- compute_format_diff(
  paste0(csv_header, "1", intToUtf8(10)),
  paste0(csv_header, "2", intToUtf8(10)),
  "csv"
)
expected_csv_path <- paste0("1,a", intToUtf8(92), ",b", intToUtf8(92), intToUtf8(92), "c")
if (length(csv_escaped_diff) != 1L || !identical(csv_escaped_diff[[1]]$path_base, expected_csv_path)) {
  stop("CSV path separators should be escaped in column names.")
}

for (format in c("xml", "csv")) {
  extension <- if (identical(format, "xml")) ".xml" else ".csv"
  base_text <- paste(readLines(file.path("..", "samples", format, paste0("base", extension)), warn = FALSE), collapse = "\n")
  compare_text <- paste(readLines(file.path("..", "samples", format, paste0("compare", extension)), warn = FALSE), collapse = "\n")
  sample_diff <- compute_format_diff(base_text, compare_text, format)
  if (length(sample_diff) == 1L && !is.null(sample_diff$error)) {
    stop(sprintf("%s sample diff failed: %s", toupper(format), sample_diff$error))
  }
}

cat("tree-diff-r sample test passed\n")
