#’------------------------------------------------------------------------------
#’ WFAPLOT - COLOURS
#’ colours.R
#’------------------------------------------------------------------------------
#’ 
#’
#' @export

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
  
  if (is.null(cols))
    return (wfa_colors)
  
  wfa_colors[cols]
}


#’------------------------------------------------------------------------------
#’ 
#’
#' @export

wfa_pal <- function(palette = "main", reverse = FALSE, ...) {
  
  wfa_palettes <- list(
    `main`  = wfa_cols("red", "dark grey"),
    
    `grey` = wfa_cols("light grey", "dark grey"),
    
    `mixed` = wfa_cols("red", "yellow", "green", "blue")
  )
  
  pal <- wfa_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  grDevices::colorRampPalette(pal, ...)
}


#’------------------------------------------------------------------------------
#’ 
#’
#' @export

scale_color_wfa <- function(palette = "main", discrete = TRUE,
                            reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("wfa_", palette),
                            palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#’------------------------------------------------------------------------------
#’ 
#’
#' @export

scale_fill_wfa <- function(palette = "main", discrete = TRUE,
                           reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("fill", paste0("wfa_", palette), palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}