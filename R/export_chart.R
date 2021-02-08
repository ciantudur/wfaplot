#’ WFAPLOT COLOURS
#’
#’ This script contains the functions for exporting the charts with and without
#’ the WFA logo
#’
#’



## FUNCTION TO ADD LOGO --------------------------------------------------------
#' @export
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10) {
  if (!logo_position %in% c(
    "top right", "top left",
    "bottom right", "bottom left"
  )) {
    stop("Error Message: Uh oh! Logo Position not recognized\nTry:
         logo_positon = 'top left', 'top right', 'bottom left', 'bottom right'")
  }
  
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  logo <- magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  
  if (logo_position == "top right") {
    x_pos <- plot_width - logo_width - 0.01 * plot_width
    y_pos <- 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos <- 0.01 * plot_width
    y_pos <- 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos <- plot_width - logo_width - 0.01 * plot_width
    y_pos <- plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos <- 0.01 * plot_width
    y_pos <- plot_height - logo_height - 0.01 * plot_height
  }
  
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}


# Function to export chart
#' @export
export_chart <- function(chart_name, file_name, include_logo = c(T, F),
                         export_width = NULL, export_height = NULL,
                         export_res = NULL) { 
  if (include_logo == T) {
    if (is.null(export_width)) {
      export_width <- 6.22222
    } else {}
    if (is.null(export_height)) {
      export_height <- 3.5
    } else {}
    if (is.null(export_res)) {
      export_res <- 900
    } else {}
    
    grDevices::png(as.character(file_name), units = "in", width = export_width,
        height = export_height, res = export_res) 
    plot(chart_name)
    grDevices::dev.off()
    
    plot_with_logo <- add_logo(
      plot_path = as.character(file_name),
      logo_path = "https://github.com/ciantudur/wfaplot/tree/main/inst/wfalogo.png",
      logo_position = "top right",
      logo_scale = 7.5)
    magick::image_write(plot_with_logo, as.character(file_name))
  } else if (include_logo == F) {
    if (is.null(export_width)) {
      export_width <- 6
    } else {}
    if (is.null(export_height)) {
      export_height <- 3
    } else {}
    if (is.null(export_res)) {
      export_res <- 900
    } else {}
    
    chart_name <- chart_name +
      ggplot2::theme(
        plot.title = ggplot2::element_blank(),
        plot.subtitle = ggplot2::element_blank(),
        plot.caption = ggplot2::element_blank())
    
    grDevices::png(as.character(file_name), units = "in", width = export_width,
        height = export_height, res = export_res) 
    plot(chart_name)
    grDevices::dev.off()
  } 
}
