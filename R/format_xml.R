xml_element_to_tree <- function(element) {
    attributes <- xml_attrs(element)
    result <- as.list(attributes)
    names(result) <- if (length(attributes) > 0L) paste0("@", names(attributes)) else character(0)

    direct_text <- xml_text(xml_find_first(element, "./text()"), trim = TRUE)
    if (length(direct_text) > 0L && !is.na(direct_text) && nzchar(direct_text)) {
        result[["#text"]] <- direct_text
    }

    children <- xml_children(element)
    if (length(children) > 0L) {
        child_names <- xml_name(children)
        for (child_name in unique(child_names)) {
            child_nodes <- children[child_names == child_name]
            child_values <- lapply(as.list(child_nodes), xml_element_to_tree)
            attr(child_values, "tree_diff_segment") <- child_name
            result[[child_name]] <- child_values
        }
    }
    result
}

parse_xml_input <- function(raw_text) {
    document <- read_xml(raw_text)
    root <- xml_root(document)
    setNames(list(xml_element_to_tree(root)), xml_name(root))
}

xml_path_join <- function(pointer, key, parent = NULL) {
    xml_segment <- attr(parent, "tree_diff_segment")
    if (!is.null(xml_segment)) {
        if (is.null(key) || !nzchar(key)) {
            return(pointer)
        }
        return(paste0(pointer, "[", as.integer(key) + 1L, "]"))
    }
    if (is.null(pointer) || pointer == "") {
        if (is.null(key) || key == "") {
            return("/")
        }
        return(paste0("/", key))
    }
    if (is.null(key) || key == "") {
        return(pointer)
    }
    if (identical(key, "#text")) {
        return(paste0(pointer, "/text()"))
    }
    paste0(pointer, "/", key)
}

xml_array_parent_path <- function(pointer, parent) {
    segment <- attr(parent, "tree_diff_segment")
    suffix <- if (is.null(segment)) "" else paste0("/", segment)
    if (nzchar(suffix) && endsWith(pointer, suffix)) {
        return(substr(pointer, 1L, nchar(pointer) - nchar(suffix)))
    }
    pointer
}
