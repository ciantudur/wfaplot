#’ -----------------------------------------------------------------------------
#’ WFAPLOT - COLOURS
#’ colours.R
#’ -----------------------------------------------------------------------------
#’
#' @title wfa_cols
#' @description This function returns the HEX codes for WFA colours.
#'
#' @export
#’
#' @param ... Name of colour (e.g. "red", "blue")
#’
#' @return Corresponding HEX codes for WFA colours
#'
#' @examples
#' wfa_cols("red")
#' wfa_cols("dark grey")


wfa_cols <- function(...) {
  wfa_colors <- c(
    `red`        = "#ce0538",
    `yellow`     = "#f9d362",
    `green`      = "#84e296",
    `blue`       = "#1ac8ed",
    `dark grey`  = "#373737",
    `light grey` = "#cecece"
  )

  cols <- c(...)

  if (is.null(cols)) {
    return(wfa_colors)
  }

  wfa_colors[cols]
}




#' @title wfa_pal
#' @description This function returns the WFA colour palettes.
#'
#' @export
#’
#' @param palette Name of colour palette (e.g. "main", "mixed", "grey")
#' @param reverse Boolean value (reverse colour palette = T, don't reverse = F)
#' @param ... Other optional parameters (see ggplot documentation)
#’
#' @return WFA colour palette for use with ggplot object
#'
#' @examples
#' wfa_pal("main", TRUE)
#' wfa_pal("mixed", FALSE)


wfa_pal <- function(palette = "main", reverse = FALSE, ...) {
  wfa_palettes <- list(
    `main` = wfa_cols("red", "dark grey"),

    `grey` = wfa_cols("light grey", "dark grey"),

    `mixed` = wfa_cols("red", "yellow", "green", "blue")
  )

  pal <- wfa_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  grDevices::colorRampPalette(pal, ...)
}




#' @title scale_color_wfa
#' @description Generate ggplot colour scale in WFA colours.
#'
#' @export
#’
#' @param palette Name of colour palette (e.g. "main", "mixed", "grey")
#' @param discrete Boolean value (discrete var = T, continuous var = F)
#' @param reverse Boolean value (reverse colour palette = T, don't reverse = F)
#' @param ... Other optional parameters (see ggplot documentation)
#’
#' @return ggplot colour scale in WFA colours
#'
#' @examples
#' scale_color_wfa("main", TRUE, FALSE)
#' scale_color_wfa("mixed", FALSE, FALSE)


scale_color_wfa <- function(palette = "main", discrete = TRUE,
                            reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("wfa_", palette),
      palette = pal, ...
    )
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}




#' @title scale_fill_wfa
#' @description Generate ggplot fill scale in WFA colours.
#'
#' @export
#’
#' @param palette Name of colour palette (e.g. "main", "mixed", "grey")
#' @param discrete Boolean value (discrete var = T, continuous var = F)
#' @param reverse Boolean value (reverse colour palette = T, don't reverse = F)
#' @param ... Other optional parameters (see ggplot documentation)
#’
#' @return ggplot fill scale in WFA colours
#'
#' @examples
#' scale_fill_wfa("main", TRUE, FALSE)
#' scale_fill_wfa("mixed", FALSE, FALSE)


scale_fill_wfa <- function(palette = "main", discrete = TRUE,
                           reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("wfa_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
