#’ -----------------------------------------------------------------------------
#’ WFAPLOT - EXPORT CHARTS
#’ export_chart.R
#’ -----------------------------------------------------------------------------
#’
#' @title export_chart
#' @description This function exports ggplot charts with and without the WFA
#' logo.
#'
#' @export
#’
#' @param chart_name Name of ggplot object to export
#' @param file_name File path for exported chart
#' @param include_logo Boolean value (include logo = T, no logo = F)
#' @param export_width Width of exported chart (in) *optional
#' @param export_height Height of exported chart (in) *optional
#' @param export_res Resolution of exported chart (dpi) *optional
#’
#' @return ggplot chart saved as png in root directory
#'
#' @examples
#' export_chart(my_chart, "my_chart.png", TRUE)
#' export_chart(my_chart, "my_chart.png", FALSE, 6, 2.5, 300)


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

    grDevices::png(as.character(file_name),
      units = "in", width = export_width,
      height = export_height, res = export_res
    )
    plot(chart_name)
    grDevices::dev.off()

    plot <- magick::image_read(file_name)
    logo_raw <- magick::image_read("wfalogo.png")

    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width

    logo <- magick::image_scale(logo_raw, as.character(plot_width / 7.5))
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height

    x_pos <- plot_width - logo_width - 0.01 * plot_width
    y_pos <- 0.01 * plot_height

    plot_with_logo <- magick::image_composite(plot, logo,
      offset = paste0(
        "+", x_pos, "+", y_pos
      )
    )
    magick::image_write(plot_with_logo, as.character(file_name))

    if (file.exists("wfalogo.png")) {
    } else {
      message("Error: Could not find wfalogo.png in root directory")
    }
    
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
        plot.caption = ggplot2::element_blank()
      )

    grDevices::png(as.character(file_name),
      units = "in", width = export_width,
      height = export_height, res = export_res
    )
    plot(chart_name)
    grDevices::dev.off()
  }
}
