#' Farver i TRM
#'
#' @param farver Choose between these colors c("blaa", "orange","graa", "gul","groen", "lyseblaa")
#' @export
trm_colors <- function(farver){

  farver_ind <- tolower(farver)

  farver_valg <- c("blaa" = grDevices::rgb(0,169,224, max=255), "orange" = grDevices::rgb(249,186,4, max=255),
    "graa" = grDevices::rgb(173,175,175, max=255), "gul" = grDevices::rgb(223,223,0, max=255),
    "groen" = grDevices::rgb(128,172,158, max=255), "lyseblaa"= grDevices::rgb(91,198,232, max=255))

  til_out <- farver_valg[order(factor(names(farver_valg), levels = farver_ind))] |>
    (\(y) y[names(y) %in% farver_ind])()

  return(as.character(til_out))
}




