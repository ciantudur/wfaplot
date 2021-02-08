## -------------------------------------------------------------------------- ##
## WFAPLOT - DEFINE COLOURS AND PALETTES ------------------------------------ ##
## -------------------------------------------------------------------------- ##
## R/colours.R
## 08 February 2021
## Cian Sion (SionC1@cardiff.ac.uk)


## REMARKS ---------------------------------------------------------------------
# This script defines the base colours and includes a function for these to be
# used within wfaplot.


# Define base colours
wfa_colors <- c(
  `red`        = "#ce0538",
  `yellow`     = "#f9d362",
  `green`      = "#84e296",
  `blue`       = "#1ac8ed",
  `dark grey`  = "#373737",
  `light grey` = "#cecece"
)


# Function to extract colours as hex codes
wfa_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (wfa_colors)
  
  wfa_colors[cols]
}


# Define color palettes
wfa_palettes <- list(
  `main`  = wfa_cols("red", "dark grey"),
  
  `grey` = wfa_cols("light grey", "dark grey"),
  
  `mixed` = wfa_cols("red", "yellow", "green", "blue")
)


# Return function to interpolate a WFA color palette
wfa_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- wfa_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}


# Color scale constructor for WFA colors
scale_color_wfa <- function(palette = "main", discrete = TRUE,
                            reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("wfa_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


# Fill scale constructor for WFA colors
scale_fill_wfa <- function(palette = "main", discrete = TRUE,
                           reverse = FALSE, ...) {
  pal <- wfa_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("wfa_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

