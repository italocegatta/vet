# Theme for internal plots
#' @export
theme_custom <- function(base_size = 11, base_family = "") {

  ggplot2::theme_bw(base_size, base_family) %+replace%
    ggplot2::theme(
      text = ggplot2::element_text(color = "black"),
      axis.text = ggplot2::element_text(color = "black"),
      plot.margin = ggplot2::margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
    )
}
