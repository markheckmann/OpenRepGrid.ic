# ////////////////////////////////////////////////////////////
#
#                       Helper functions
#
# ////////////////////////////////////////////////////////////


#' Create empty dataframe while keeping columns names
#'
#' Also works on lists if dataframes are contained as list elements.
#' @param x Dataframe or list.
#' @export
#' @keywords internal
#'
emptify_object <- function(x) {
  if (is.data.frame(x)) {
    return(x[integer(0), ])
  }

  for (nm in names(x)) {
    df <- x[[nm]]
    if (is.data.frame(df)) {
      df <- df[integer(0), ]
    } else {
      df <- list()
    }
    x[[nm]] <- df
  }
  x
}


#' Default datatable output when loading
#'
#' @param text Text to be shown
#' @param header Table header, defaults to an  empty string.
#' @export
#' @keywords internal
dt_default <- function(text = "Waiting for data ...",
                       header = "") {
  df <- data.frame(col1 = text)
  names(df) <- header

  # create datatable and format
  df %>%
    (DT::datatable)(filter = "none",
      selection = "none",
      colnames = header,
      class = "compact", rownames = FALSE,
      options = list(
        dom = "t",
        ordering = TRUE,
        pageLength = 10
      )
    )
}


#' String splitter for comma separated values in Excel cell
#'
#' @param x String to be split
#' @export
#' @keywords internal
#' @examples
#'
#' cell_text_split("10, 20,30")
#' cell_text_split("; ,  10  ,,,  20;30,,") # very robust
#'
cell_text_split <- function(x) {
  x %>%
    stringr::str_replace_all(" +|;+", ",") %>% # space, semicolon to comma
    stringr::str_replace_all(",+", ",") %>% # several commas to one
    stringr::str_replace_all("^,+|,+$", "") %>% # remove leading and trainling commas
    strsplit(",") # split string at comma
}


#' @rdname make-names
#' @export
make_names_vec <- function(x) {
  nms <- tolower(x)
  nms <- stringr::str_replace_all(nms, "[[:blank:]]+", "_") # replace blanks
  nms <- stringr::str_replace_all(nms, "\\.|-|/|\\(|\\)|&|\\?", "_") # replace . - \ ? to _ (underscore)
  nms <- stringr::str_replace_all(nms, "[_]+", "_") # replace multiple underscores by one
  nms <- stringr::str_replace_all(nms, "[_]+$", "") # remove trailing underscores
  nms <- stringr::str_replace_all(nms, "\u00DF", "ss")
  nms <- stringr::str_replace_all(nms, "\u00E4", "ae") # replace German umlauts by their two letter ASCII version
  nms <- stringr::str_replace_all(nms, "\u00FC", "ue")
  nms <- stringr::str_replace_all(nms, "\u00F6", "oe")
  nms
}


#' standardized variable names
#'
#' @param x Dataframe or vector
#' @keywords internal
#' @rdname make-names
#' @export
make_names <- function(x) {
  if (!is.data.frame(x)) {
    stop("x muste be a dataframe")
  }

  names(x) <- make_names_vec(names(x))
  x
}


#' Format as x digit number
#'
#' @export
#' @keywords internal
#'
fnum <- function(x, digits = 2) {
  x <- round(x, digits)
  formatC(x, digits = digits, format = "f")
}
