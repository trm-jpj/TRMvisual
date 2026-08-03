#' Farver i TRM
#'
#' @param farver Vælg mellem c("blaa", "cyan", "orange", "groen", "roed")
#' @param lys Lyshedsgrad: 0 (fuld), 1, 2 eller 3
#' @export
trm_colors <- function(farver, lys = 0) {
  
  farver_ind <- tolower(farver)
  
  palette <- list(
    blaa   = c("#002447", "#536D86", "#A6B6C5", "#CFDAE5"),
    cyan   = c("#127EA3", "#469CB9", "#7BB9CE", "#AFD7E4"),
    orange = c("#F2683E", "#F58A6A", "#F8AB95", "#FBCDC1"),
    groen  = c("#086B39", "#43916C", "#7EB89E", "#B9DED1"),
    roed   = c("#B50012", "#C9414D", "#DE8287", "#F2C3C2")
  )
  
  ukendt <- setdiff(farver_ind, names(palette))
  if (length(ukendt) > 0) {
    stop("Ukendt farve: ", paste(ukendt, collapse = ", "),
         ". Vaelg mellem: ", paste(names(palette), collapse = ", "))
  }
  if (!all(lys %in% 0:3)) stop("lys skal vaere 0, 1, 2 eller 3")
  
  lys <- rep_len(lys, length(farver_ind))
  unname(mapply(function(f, l) palette[[f]][l + 1], farver_ind, lys))
}
