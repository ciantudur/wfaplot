## -------------------------------------------------------------------------- ##
## WFAPLOT - SET UP GGPLOT THEME -------------------------------------------- ##
## -------------------------------------------------------------------------- ##
## R/theme_wfa.R
## 08 February 2021
## Cian Sion (SionC1@cardiff.ac.uk)


## REMARKS ---------------------------------------------------------------------
# This script contains the paramaters for theme_wfa() to be called when
# generating ggplots.


theme_wfa<- function(){
  ggplot2::theme_minimal() %+replace%
    theme(
      axis.text = element_text(
        color = "#373737",
        family = "Fira Sans"
      ),
      axis.text.x.bottom = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5
      ),
      axis.title.y = element_text(
        colour = "#373737",
        family = "Fira Sans",
        size = 8
      ),
      axis.title.x = element_text(
        colour = "#373737",
        family = "Fira Sans",
        size = 9
      ),
      legend.title = element_blank(),
      legend.text = element_text(family = "Fira Sans"),
      plot.title = element_text(family = "Fira Sans Medium"),
      plot.subtitle = element_text(family = "Fira Sans", color = "#373737"),
      plot.caption = element_text(
        size = 8,
        hjust = 0,
        face = "italic",
        family = "Fira Sans",
      ), complete = T)
}
