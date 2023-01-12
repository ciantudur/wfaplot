#' WFA Colours
#'
#' This function returns the HEX codes for WFA colours.
#' @param cols Name a colour from the WFA palette to return the HEX code.
#' Leave argument empty to return all colours.
#' Choose from red, orange, yellow, lime, green, teal, blue, pink, purple,
#' indigo, dark grey, and light grey.
#' @keywords
#' color
#' colour
#' @export
#' @examples
#' wfa_cols()
#' wfa_cols("red")
wfa_cols <-
  function(cols = "") {
    wfa_colours <- c(
      `red`        = "#CE0538",
      `orange`     = "#ED8035",
      `yellow`     = "#F9D362",
      `lime`       = "#C0DE72",
      `green`      = "#84E296",
      `teal`       = "#14D9CC",
      `blue`       = "#1AC8ED",
      `pink`       = "#D14099",
      `purple`     = "#9D7CE1",
      `indigo`     = "#41A9FB",
      `dark grey`  = "#373737",
      `light grey` = "#CECECE"
    )

    if (cols == "") {
      return(wfa_colours)
    } else {
      if (is.na(wfa_colours[cols])) {
        message(paste0(
          "'", cols, "' ", "is not a valid colour. ",
          "Use wfa_cols() to view all colours."
        ))
      } else {
        unname(wfa_colours[cols])
      }
    }
  }
