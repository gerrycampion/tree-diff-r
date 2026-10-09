library(treediffr)

json_diff <- compute_format_diff('{"name":"alpha"}', '{"name":"beta"}', "json")
if (length(json_diff) != 1L || !identical(json_diff[[1]]$op, "replace") ||
  !identical(json_diff[[1]]$path_base, "/name")) {
  stop("JSON text inputs should produce a path-aware diff.")
}

value_diff <- diff_value(list(name = "alpha"), list(name = "beta"))
if (length(value_diff) != 1L || !identical(value_diff[[1]]$op, "replace")) {
  stop("diff_value should be available as a package API.")
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

csv_added_row <- compute_format_diff("id\n1\n", "id\n1\n2\n", "csv")
if (length(csv_added_row) != 1L || !identical(csv_added_row[[1]]$path_base, "") ||
  !identical(csv_added_row[[1]]$path_compare, "2")) {
  stop("CSV row additions should use an empty whole-table path on the base side.")
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

cat("treediffr package tests passed\n")
