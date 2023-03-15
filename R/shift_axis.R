#' @export
shift_axis <- function(p, y=0){
  g <- ggplot2::ggplotGrob(p)
  dummy <- data.frame(y=y)
  ax <- g[["grobs"]][g$layout$name == "axis-b"][[1]]
  p + ggplot2::annotation_custom(grid::grobTree(ax, vp = grid::viewport(y=1, height=sum(ax$height))), ymax=y, ymin=y)+
    ggplot2::geom_hline(ggplot2::aes(yintercept=y), data = dummy) +
    ggplot2::theme(axis.text.x = ggplot2::element_blank())
}
