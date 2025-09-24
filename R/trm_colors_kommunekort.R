#' Farver i kommunkortTRM
#'
#' @param farver Choose between these colors from gradient to continous
#' @export
trm_colors_kommunekort <- function(type="gradient"){

  farve_palette <- RColorBrewer::brewer.pal(9, name = "Blues")[c(T,F)][1:9] |> 
    (\(y) y[!is.na(y)])() 
  
  farver <- switch(type,
         "gradient" = c('#F2683E','#7BB9CE', '#002447'),
         "continous" = farve_palette)
 
  return(factor(farver))
}
