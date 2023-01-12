#' WFA Theme
#'
#' This function checks if required fonts are installed and applies the WFA
#' theme to a ggplot object.
#' @keywords
#' theme
#' wfa
#' @export
#' @examples
#' theme_wfa()
theme_wfa <-
  function() {
    require(curl)
    # Check if Fira Sans font is loaded, and add from Google if not
    if (("fira-sans" %in% sysfonts::font_families()) &
      ("source-sans" %in% sysfonts::font_families())) {
      showtext::showtext_auto()
    } else {
      message("Loading font families...")
      sysfonts::font_add_google(name = "Fira Sans", family = "fira-sans")
      sysfonts::font_add_google(name = "Source Sans Pro", family = "source-sans")
      showtext::showtext_auto()
    }

    # Apply changes to theme_minimal()
    ggplot2::theme_minimal()+
    showtext::showtext_opts(dpi = 400)
    ggplot2::theme(
      axis.title = ggplot2::element_text(
        colour = "#5c5c5c",
        family = "fira-sans",
        size = 8
      ),
      axis.text = ggplot2::element_text(
        color = "#5c5c5c",
        family = "fira-sans",
        size = 8
      ),
      axis.text.x.bottom = ggplot2::element_text(
        angle = 0,
        hjust = 0.5,
        vjust = 0.5,
        size = 8
      ),
      legend.background = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(
        family = "fira-sans",
        color = "#373737"
      ),
      legend.title = ggplot2::element_blank(),
      legend.title.align = 0,
      plot.title = ggplot2::element_text(
        family = "fira-sans",
      ),
      plot.subtitle = ggplot2::element_text(
        family = "source-sans",
        color = "#373737",
        size = 10
      ),
      plot.caption = ggplot2::element_text(
        size = 6,
        hjust = 0,
        face = "plain",
        family = "source-sans",
        colour = "#373737"
      ),
      strip.text = ggplot2::element_text(
        family = "fira-sans",
        size = 9,
        color = "#373737",
        hjust = 0
      ),
      complete = FALSE,
      validate = TRUE
    )
  }
