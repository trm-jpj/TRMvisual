#' @examples 
#' \dontrun{
#' input <- read_sf("C:/Users/TRM/Desktop/Git/filer til kort/kommuner") |> 
#' left_join(DINE_DATA, by = c("navn"="KOMMUNE")) 
#' }
#' @importFrom rlang .data
#' @export
kommune_kort <- function(input, var, legend_title, legend_lab =NULL, farver, NA_farve = "darkgrey"){
  
  if(!methods::is(input[[var]], "numeric")){
    
    assert::assert(length(unique(input[[var]]))<=length(c(farver, NA_farve)), 
                   msg = "Når labels er kommet ind i figuren skal der som minimum være en farve pr. label")
    
    if(!is.null(legend_lab)){
      assert::assert(length(legend_lab)<=length(c(farver, NA_farve)), 
                     msg = "Når labels er kommet ind i figuren skal der som minimum være en farve pr. label")
    }
    
    
    if(is.null(legend_lab)){
      
      legend_lab <- unique(input[[var]]) |> sort()
      
      }
    
    scale_fill <-   ggplot2::scale_fill_manual(values = farver, 
                                               breaks = legend_lab,
                                               na.value=NA_farve) 
    
    
  } else {
    
    assert::assert(length(farver) == 3,
                   msg = "Når der er tale om en numerisk variabel skal der være 3 farver: Low, mid og high" )
    
    scale_fill <- ggplot2::scale_fill_gradient2(
      low=farver[1], mid=farver[2], high=farver[3], 
      midpoint = max(input[[var]], na.rm = T) - (abs(max(input[[var]], na.rm = T)) + abs(min(input[[var]], na.rm = T)))/2, 
      na.value=NA_farve)
    
  }
  
  spacing <- input[[var]] |> round(0) |>  as.character() |> str_length() |> max(na.rm = T)
  
  # Plot med ladepunkter per 1.000 indbyggere. 
  # Kort uden Bornholm: 
  # Alt undtagen FRB plottes f?rst og s? plottes FRB ovenp?. 
  k1 <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data = input |> 
                       dplyr::filter(as.numeric(.data$kode)!=400),
                     ggplot2::aes(fill = get(var))) +
    ggplot2::labs(fill=stringr::str_wrap(legend_title, width=10)) +
    scale_fill + 
    ggplot2::theme_void() +
    # ggplot2::theme_void() +
    # xlim(56,57) + #Afgr?nser x-aksen
    ggplot2::theme( 
      legend.position = c(0.85, 0.35),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.key.width = grid::unit(0.2,"cm"),
      legend.spacing.x = grid::unit(spacing/10, "cm"),
      legend.title = ggplot2::element_text(size = 8, family = "Georgia"),
      legend.text = ggplot2::element_text(size = 6, family = "Georgia")) 
  
  k2 <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data = input |> dplyr::filter(as.numeric(.data$kode)==400),
                     ggplot2::aes(fill = get(var))) +
    scale_fill +
    ggplot2::theme_void() + 
    ggplot2::scale_y_continuous(expand = c(0.08,0.08)) +
    ggplot2::scale_x_continuous(expand = c(0.1,0.1)) +
    ggplot2::theme(legend.position = "none"
                   # , 
                   # panel.border = element_rect(color = "black",
                   #                                   fill = NA,
                   #                                   size = 1)
    )
  
  dk_kort <- cowplot::ggdraw(k1) +
    cowplot::draw_plot(k2,
                       x = 0.6, 
                       # The distance along a (0,1) y-axis to draw the bottom edge of the plot
                       y = 0.72,
                       # The width and height of the plot expressed as proportion of the entire ggdraw object
                       width = 0.2, 
                       height = 0.2
    )
  
  return(dk_kort)     
  
}



