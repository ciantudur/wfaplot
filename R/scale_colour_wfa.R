#' WFA Colour Scale
#'
#' This function applies an WFA-themed colour scale to a ggplot object.
#' @param palette Select palette to use. Defaults to "main".
#' @param discrete Defaults to TRUE. Set to FALSE for continuous variable.
#' @param reverse Defaults to FALSE. Set to TRUE to reverse the order of the
#' colour scale.
#' @keywords
#' colour
#' scale
#' @import
#' ggplot2
#' @export
#' @examples
#' scale_colour_wfa("fuchsia")
#' scale_colour_wfa(palette = "main", discrete = "TRUE", reverse = "FALSE")
scale_colour_wfa <- function(palette = "main",
                             discrete = TRUE,
                             reverse = FALSE) {
  pal <- wfa_pal(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("colour", paste0("wfa_", palette),
      palette = pal
    )
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256))
  }
}
