parse_json_input <- function(raw_text) {
    fromJSON(raw_text, simplifyVector = FALSE, simplifyDataFrame = FALSE)
}

json_escape_path_segment <- function(key) {
    key <- gsub("~", "~0", as.character(key), fixed = TRUE)
    gsub("/", "~1", key, fixed = TRUE)
}

json_path_join <- function(pointer, key) {
    if (is.null(pointer) || pointer == "") {
        if (is.null(key) || key == "") {
            return("/")
        }
        return(paste0("/", json_escape_path_segment(key)))
    }
    if (is.null(key) || key == "") {
        return(pointer)
    }
    paste0(pointer, "/", json_escape_path_segment(key))
}
