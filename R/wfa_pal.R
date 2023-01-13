#' WFA Colour Palettes
#'
#' This function returns an WFA colour palette.
#' @param palette Select an WFA colour palette. Choose from 'main', 'grey',
#' 'colourful', 'extra_colourful', and 'fuchsia'.
#' @param reverse Defaults to FALSE. Set to TRUE to reverse the order of the
#' colour palette
#' @keywords
#' palette
#' @import
#' grDevices
#' @export
#' @examples
#' wfa_pal()
#' wfa_pal("extra_colourful", reverse = TRUE)
wfa_pal <-
  function(palette = "main", reverse = FALSE) {
    wfa_palettes <- list(
      `main` = c(
        wfa_cols("red"),
        wfa_cols("dark grey"),
        wfa_cols("light grey")
      ),
      `grey` = c(
        wfa_cols("light grey"),
        wfa_cols("dark grey")
      ),
      `colourful` = c(
        wfa_cols("red"),
        wfa_cols("yellow"),
        wfa_cols("green"),
        wfa_cols("blue")
      ),
      `extra_colourful` = c(
        wfa_cols("red"),
        wfa_cols("orange"),
        wfa_cols("yellow"),
        wfa_cols("lime"),
        wfa_cols("green"),
        wfa_cols("teal"),
        wfa_cols("blue"),
        wfa_cols("indigo"),
        wfa_cols("purple"),
        wfa_cols("pink")
      ),
      `fuchsia` = c(
        wfa_cols("red"),
        wfa_cols("purple"),
        wfa_cols("blue")
      )
    )

    if (palette %in% names(wfa_palettes) == FALSE) {
      message(paste0(
        "'", palette, "' ", "is not a valid colour palette. ",
        "The options are 'main', 'grey', 'colourful', 'extra_colourful', ",
        "and 'fuchsia'."
      ))
    } else {
      pal <- wfa_palettes[[palette]]
      if (reverse) pal <- rev(pal)

      rampGenerator <- function(colors, ...) {
        ramp <- grDevices::colorRamp(colors, ...)
        function(n) {
          if (n <= length(colors) &
            n > 0) {
            colors[1:n]
          } else {
            x <- ramp(seq.int(0, 1, length.out = n))
            if (ncol(x) == 4L) {
              rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
            } else {
              rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
            }
          }
        }
      }

      rampGenerator(pal)
    }
  }
