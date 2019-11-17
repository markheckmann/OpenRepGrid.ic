#////////////////////////////////////////////////////////////
#
#                       Helper functions
#
#////////////////////////////////////////////////////////////


#' Create empty dataframe while keeping columns names
#' 
#' Also works on lists if dataframes are contained as list elements.
#' @param x Dataframe or list.
#' @export
#' @keywords internal
#' 
emptify_object <- function(x) 
{
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
#' @export
#' 
dt_default <- function(text = "Waiting for data ...", 
                       header = "") 
{
  df <- data.frame(Spalte = text)
  names(df) <- header
  
  # create datatable and format
  df %>%
    datatable(filter= "none", 
              selection ="none", 
              colnames = header,
              class = 'compact', rownames = FALSE, 
              options = list(
                dom = 't',  
                ordering = TRUE,
                pageLength = 10
              )
   )
}


#' Spezieller String Split für Alter Definition
#' 
#' Alter in denen eine Maßnahme statfinden soll
#' können als in einem String als Abfolge eingegeben
#' werden. 
#' 
#' @param x String zum splitten
#' @export
#' @keywords internal
#' @examples
#' alter_split("10, 20,30" )
#' alter_split("; ,  10  ,,,  20;30,," ) # sehr robust
#' 
alter_split <- function(x)
{
  x %>% 
    str_replace_all(" +|;+", ",") %>%   # space, semicolon zu Komma
    str_replace_all(",+", ",") %>%      # meherer kommas zu einem
    str_replace_all("^,+|,+$", "") %>%  # kommas am anfang und ende entfernen
    strsplit(",")                       # string am komma splitten
}


#' @rdname make-names
#' @export
make_names_vec <- function(x) 
{
  nms <- tolower(x)
  nms <- str_replace_all(nms, "[[:blank:]]+", "_")       # Leerzeichen ersetzen
  nms <- str_replace_all(nms, "\\.|-|/|\\(|\\)|&|\\?", "_")  # punkte, bindestrich, (back-)slashes, Fragezeichen zu unterstrich
  nms <- str_replace_all(nms, "[_]+", "_")               # mehrere unterstriche zu einem
  nms <- str_replace_all(nms, "[_]+$", "")               # unterstriche am ende entfernen
  nms <- str_replace_all(nms, "\u00E4", "ae")            # umlaute ersetzen
  nms <- str_replace_all(nms, "\u00FC", "ue") 
  nms <- str_replace_all(nms, "\u00F6", "oe") 
  nms
}


#' standardisierte variablen namen 
#'
#' @param x Dataframe oder Vektor
#' @keywords internal
#' @rdname make-names
#' @export
make_names <- function(x) 
{
  if (!is.data.frame(x))
    stop("x muste be a dataframe")
  
  names(x) <- make_names_vec( names(x) )
  x
}

#' Zahl sauber mit zwei Stellen
#' 
#' @export
#' @keywords internal
#' 
fnum <- function(x, digits=2)
{
  x <- round(x, digits)
  formatC(x, digits =digits, format="f")
}









