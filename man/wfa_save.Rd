% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wfa_save.R
\name{wfa_save}
\alias{wfa_save}
\title{WFA Save}
\usage{
wfa_save(
  chart_name,
  file_name,
  include_logo = c(F, T),
  file_type = c("png", "jpeg", "svg", "tiff", "pdf"),
  export_width = NULL,
  export_height = NULL,
  export_res = NULL
)
}
\arguments{
\item{chart_name}{Name of ggplot object to export.}

\item{file_name}{File name of chart to export.}

\item{include_logo}{Defaults to FALSE. Set to TRUE to include logo in
rasterised output (only works with png, jpeg and tiff files). The wfalogo.png
file must be present in root directory.}

\item{file_type}{Select filetype (.png, .jpeg, .svg, .tiff, .pdf). The file
name extension takes priority (if present).}

\item{export_width}{Override default chart width (inches).}

\item{export_height}{Override default chart height (inches).}

\item{export_res}{Override default resolution (400dpi).}
}
\description{
This function saves a ggplot object with or without the WFA logo.
}
\examples{
# Generate ggplot object for use in exammple
df <- data.frame(gp = factor(rep(letters[1:3], each = 10)), y = rnorm(30))
ds <- do.call(rbind, lapply(split(df, df$gp), function(d) {
  data.frame(
    mean = mean(d$y), sd = sd(d$y), gp = d$gp
  )
}))
plot <- ggplot2::ggplot(df, ggplot2::aes(gp, y)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(
    data = ds, ggplot2::aes(y = mean), colour = "red",
    size = 3
  )
# Save ggplot objects using wfa_save function
wfa_save(plot, "chart.svg")
wfa_save(plot, "chart.png", FALSE)
wfa_save(plot, "chart.tiff", FALSE,
  export_width = 6,
  export_height = 8
)
}
\keyword{logo}
\keyword{save}
\keyword{wfa}
