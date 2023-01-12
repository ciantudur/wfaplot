#' WFA Save
#'
#' This function saves a ggplot object with or without the WFA logo.
#' @param chart_name Name of ggplot object to export.
#' @param file_name File name of chart to export.
#' @param include_logo Defaults to FALSE. Set to TRUE to include logo in
#' rasterised output (only works with png, jpeg and tiff files). The wfalogo.png
#' file must be present in root directory.
#' @param file_type Select filetype (.png, .jpeg, .svg, .tiff, .pdf). The file
#' name extension takes priority (if present).
#' @param export_width Override default chart width (inches).
#' @param export_height Override default chart height (inches).
#' @param export_res Override default resolution (400dpi). Warning: Changing
#' this value may impact the scaling of text. Add
#' 'showtext::showtext_opts(dpi = ##)' to ggplot object before saving to fix
#' this.
#' @keywords
#' save
#' wfa
#' logo
#' @import
#' grDevices
#' magick
#' ggplot2
#' @export
#' @examples
#' # Generate ggplot object for use in exammple
#' df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
#' ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {data.frame(
#'    mean = mean(d$y), sd = sd(d$y), gp = d$gp)}))
#' plot <- ggplot2::ggplot(df, ggplot2::aes(gp, y)) + ggplot2::geom_point() +
#'    ggplot2::geom_point(data = ds, ggplot2::aes(y = mean), colour = 'red',
#'    size = 3)
#' # Save ggplot objects using wfa_save function
#' wfa_save(plot, "chart.svg")
#' wfa_save(plot, "chart.png", FALSE)
#' wfa_save(plot, "chart.tiff", FALSE, export_width = 6,
#' export_height = 8)
wfa_save <- function(chart_name,
                     file_name,
                     include_logo = c(F, T),
                     file_type = c("png", "jpeg", "svg", "tiff", "pdf"),
                     export_width = NULL,
                     export_height = NULL,
                     export_res = NULL) {
  if (ggplot2::is.ggplot(chart_name) == FALSE) {
    message("chart_name is not a valid ggplot object")
  } else {
    if (length(include_logo) != 1) {
      include_logo <- F
    } else {}
    if (include_logo == T) {
      if (file.exists("wfalogo.png")) {
      } else {
        message("Error: Could not find wfalogo.png in root directory")
      }
      if (is.null(export_width)) {
        export_width <- 6.2222
      } else {}
      if (is.null(export_height)) {
        export_height <- 3.5
      } else {}
      if (is.null(export_res)) {
        export_res <- 400
      }
      if (length(file_type) != 1) {
        file_type <- NULL
      } else {}
      if (is.null(file_type) == T) {
        if (endsWith(file_name, ".pdf") == T) {
          file_type <- "pdf"
        } else if
        (endsWith(file_name, ".jpeg") == T) {
          file_type <- "jpeg"
        } else if
        (endsWith(file_name, ".svg") == T) {
          file_type <- "svg"
        } else if
        (endsWith(file_name, ".tiff") == T) {
          file_type <- "tiff"
        } else {
          file_type <- "png"
        }
      } else {}
      if (file_type == "png") {
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

      } else if (file_type == "jpeg") {
        grDevices::jpeg(as.character(file_name),
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

            } else if (file_type == "tiff") {
        grDevices::tiff(as.character(file_name),
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

      } else {
        message("Error: Can only export chart with logo as a rasterized file (png, jpeg or tiff)")
      }
    } else if (include_logo == F) {
      if (length(file_type) != 1) {
        file_type <- NULL
      }
      if (is.null(export_width)) {
        export_width <- 6
      } else {}
      if (is.null(export_height)) {
        export_height <- 3
      } else {}
      if (is.null(export_res)) {
        export_res <- 400
      } else {}
      chart_name <- chart_name +
        ggplot2::theme(
          plot.title = ggplot2::element_blank(),
          plot.subtitle = ggplot2::element_blank(),
          plot.caption = ggplot2::element_blank()
        )
      if (is.null(file_type) == T) {
        if (endsWith(file_name, ".pdf") == T) {
          file_type <- "pdf"
        } else if
        (endsWith(file_name, ".jpeg") == T) {
          file_type <- "jpeg"
        } else if
        (endsWith(file_name, ".svg") == T) {
          file_type <- "svg"
        } else if
        (endsWith(file_name, ".tiff") == T) {
          file_type <- "tiff"
        } else {
          file_type <- "png"
        }
      } else {}
      if (file_type == "jpeg") {
        grDevices::jpeg(as.character(file_name),
          units = "in", width = export_width,
          height = export_height, res = export_res
        )
        plot(chart_name)
        grDevices::dev.off()
      } else if (file_type == "svg") {
        grDevices::svg(as.character(file_name),
          width = export_width,
          height = export_height
        )
        plot(chart_name)
        grDevices::dev.off()
      } else if (file_type == "tiff") {
        grDevices::tiff(as.character(file_name),
          units = "in", width = export_width,
          height = export_height, res = export_res
        )
        plot(chart_name)
        grDevices::dev.off()
      } else if (file_type == "pdf") {
        grDevices::pdf(as.character(file_name),
          width = export_width,
          height = export_height
        )
        plot(chart_name)
        grDevices::dev.off()
      } else if (file_type == "png") {
        grDevices::png(as.character(file_name),
          units = "in", width = export_width,
          height = export_height, res = export_res
        )
        plot(chart_name)
        grDevices::dev.off()
      }
    }
  }
  message("--> Check saved output")
}
