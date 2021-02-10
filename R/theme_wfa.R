#' -----------------------------------------------------------------------------
#' WFAPLOT - THEME FOR GGPLOT
#' theme_wfa.R
#' -----------------------------------------------------------------------------
#'
#' @title theme_wfa
#' @description This script contains the parameters for themeing ggplots in the
#' WFA style.
#'
#' @export
#'
#' @return ggplot theme objects
#'
#' @examples
#' #ggplot2(data = my_data, aes(x = xval, y= yval)) +
#' #geom_line() +
#' #theme_wfa()


theme_wfa <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        color = "#5c5c5c",
        family = "Fira Sans"
      ),
      axis.text.x.bottom = ggplot2::element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5
      ),
      axis.title = ggplot2::element_text(
        colour = "#5c5c5c",
        family = "Fira Sans",
        size = 8
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = "Fira Sans",
                                          color = "#373737"),
      plot.title = ggplot2::element_text(family = "Fira Sans Medium"),
      plot.subtitle = ggplot2::element_text(family = "Fira Sans",
                                            color = "#373737",
                                            size = 10),
      plot.caption = ggplot2::element_text(
        size = 6,
        hjust = 0,
        face = "plain",
        family = "Fira Sans",
        colour = "#373737"
      ),
      complete = F
    ) + theme(plot.caption = ggtext::element_markdown())
}
