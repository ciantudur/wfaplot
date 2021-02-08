#’ WFAPLOT COLOURS
#’
#’ This script contains the paramaters for theme_wfa() to be called when
#’ generating ggplots.
#’
#’


# ggplot theme paramaters
#' @export
theme_wfa<- function(){
  ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(
        color = "#373737",
        family = "Fira Sans"
      ),
      axis.text.x.bottom = ggplot2::element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ),
      axis.title.y = ggplot2::element_text(
        colour = "#373737",
        family = "Fira Sans",
        size = 8
      ),
      axis.title.x = ggplot2::element_text(
        colour = "#373737",
        family = "Fira Sans",
        size = 9
      ),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(family = "Fira Sans"),
      plot.title = ggplot2::element_text(family = "Fira Sans Medium"),
      plot.subtitle = ggplot2::element_text(family = "Fira Sans", color = "#373737"),
      plot.caption = ggplot2::element_text(
        size = 8,
        hjust = 0,
        face = "italic",
        family = "Fira Sans",
      ), complete = T)
}
