
#' @export
theme_custom <- function(font_size = 14, font_family = "sans") {

  ggplot2::theme_bw(font_size, font_family) %+replace%
    theme(
      text = ggplot2::element_text(color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      # panel.grid.major = ggplot2::element_blank(),
      # panel.grid.minor = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
}
