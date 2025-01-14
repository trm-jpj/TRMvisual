#' @examples 
#' \dontrun{
#' input <- read_sf("C:/Users/TRM/Desktop/Git/filer til kort/kommuner") |> 
#' left_join(DINE_DATA, by = c("navn"="KOMMUNE")) 
#' }
#' @importFrom rlang .data
#' @export
kommune_kort <- function(input, var, legend_title, legend_lab =NULL, farver, NA_farve = "darkgrey", midpoint=NULL){
  
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
    
    if(is.null(midpoint)) midpoint <- max(input[[var]], na.rm = T) - (abs(max(input[[var]], na.rm = T)) + abs(min(input[[var]], na.rm = T)))/2
    
    scale_fill <- ggplot2::scale_fill_gradient2(
      low=farver[1], mid=farver[2], high=farver[3], 
      midpoint = midpoint, 
      na.value=NA_farve)
    
  }
  
  if(methods::is(input[[var]], "numeric")){
    spacing <- input[[var]] |> round(0) |>  as.character() |> stringr::str_length() |> max(na.rm = T)-1
  } else{
    spacing = 1
  }
  
  kommuner_til_zoom <- c(101, 147, 155, 185, 165, 151, 153, 157, 159, 161, 163, 167, 169, 183, 173, 175, 187, 201, 240, 190, 223, 230)
  
  var_1 <- input %>% names() %>% stringr::str_subset("geometry", negate = T) %>% tibble::enframe(name = NULL) %>% 
    dplyr::mutate(koll = NA) %>% tidyr::pivot_wider(names_from = "value", values_from = "koll")
  
  # til_k1 <- rbind(input %>% 
  #                   dplyr::filter(!as.numeric(kode) %in% kommuner_til_zoom),
  #                 input |>
  #                   dplyr::filter(as.numeric(kode) %in% kommuner_til_zoom) %>% 
  #                   st_combine() %>% 
  #                   st_as_sf() %>% rename(geometry=x) %>% cbind(var_1))
  
  
  # Plot med ladepunkter per 1.000 indbyggere. 
  # Kort uden Bornholm: 
  # Alt undtagen FRB plottes f?rst og s? plottes FRB ovenp?. 
  k1 <- ggplot2::ggplot(input |> 
                          dplyr::filter(as.numeric(.data$kode)!=400)) + 
    ggplot2::geom_sf(ggplot2::aes(fill = get(var))) +
    ggplot2::labs(fill=stringr::str_wrap(legend_title, width=10)) +
    scale_fill + 
    ggplot2::theme_void() +
    # ggplot2::theme_void() +
    # xlim(56,57) + #Afgr?nser x-aksen
    ggplot2::theme( 
      legend.position = c(0.77, 0.75),
      legend.key.size = grid::unit(0.4, "cm"),
      legend.key.width = grid::unit(0.2,"cm"),
      legend.spacing.x = grid::unit(spacing/10, "cm"),
      legend.title = ggplot2::element_text(size = 8, family = "Georgia"),
      legend.text = ggplot2::element_text(size = 6, family = "Georgia")) +
    ggmagnify::geom_magnify(aes(from = as.numeric(.data$kode) %in% kommuner_til_zoom),
                    to = c(13.3, 15.5 ,54.8 , 56), shadow = TRUE, 
                    aspect = "fixed", 
                    expand = 0)
  
  k2 <- ggplot2::ggplot() + 
    ggplot2::geom_sf(data = input |> dplyr::filter(as.numeric(.data$kode)==400),
                     ggplot2::aes(fill = get(var))) +
    scale_fill +
    ggplot2::theme_void() + 
    ggplot2::scale_y_continuous(expand = c(0.08,0.08)) +
    ggplot2::scale_x_continuous(expand = c(0.1,0.1)) +
    ggplot2::theme(legend.position = "none",
                   panel.background = ggplot2::element_rect(colour = "black", linewidth=0.5)
                   # , 
                   # panel.border = element_rect(color = "black",
                   #                                   fill = NA,
                   #                                   size = 1)
    )
  

  # k3 <- ggplot2::ggplot() + 
  #   ggplot2::geom_sf(data = input |> dplyr::filter(as.numeric(.data$kode) %in% kommuner_til_zoom),
  #                    ggplot2::aes(fill = get(var))) +
  #   scale_fill +
  #   ggplot2::theme_void() + 
  #   ggplot2::scale_y_continuous(expand = c(0.08,0.08)) +
  #   ggplot2::scale_x_continuous(expand = c(0.1,0.1)) +
  #   ggplot2::theme(legend.position = "none",
  #                  panel.background = element_rect(colour = "black", linewidth=1)
  #                  # , 
  #                  # panel.border = element_rect(color = "black",
  #                  #                                   fill = NA,
  #                  #                                   size = 1)
  #   )
  
  
  dk_kort <- cowplot::ggdraw(k1) +
    cowplot::draw_plot(k2,
                       x = 0.55, 
                       # The distance along a (0,1) y-axis to draw the bottom edge of the plot
                       y = 0.72,
                       # The width and height of the plot expressed as proportion of the entire ggdraw object
                       width = 0.2, 
                       height = 0.2
                       
    ) 
  
  
  return(dk_kort)     
  
}


theme_kort_trm <- function(base_size = 11, base_family = "", base_line_size = base_size/22, 
                            base_rect_size = base_size/22){ 
  
  half_line <- base_size/2
  theme(
    # line = element_blank(), 
    # rect = element_blank(), 
    #   text = element_text(family = base_family, face = "plain", 
    #                       colour = "black", size = base_size, lineheight = 0.9, 
    #                       hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
    #                       debug = FALSE), axis.text = element_blank(), axis.title = element_blank(), 
    #   axis.ticks.length = unit(0, "pt"), axis.ticks.length.x = NULL, 
    #   axis.ticks.length.x.top = NULL, axis.ticks.length.x.bottom = NULL, 
    #   axis.ticks.length.y = NULL, axis.ticks.length.y.left = NULL, 
    #   axis.ticks.length.y.right = NULL, legend.box = NULL, 
    #   legend.key.size = unit(1.2, "lines"), legend.position = "right", 
    #   legend.text = element_text(size = rel(0.8)), legend.title = element_text(hjust = 0), 
      # strip.clip = "inherit", strip.text = element_text(size = rel(0.8)), 
      # strip.switch.pad.grid = unit(half_line/2, "pt"), strip.switch.pad.wrap = unit(half_line/2, 
      #                                                                               "pt"), 
      # panel.ontop = FALSE, panel.spacing = unit(half_line, 
      #                                                                                                                                "pt"), plot.margin = unit(c(0, 0, 0, 0), "lines"), 
      # plot.title = element_text(size = rel(1.2), hjust = 0, 
      #                           vjust = 1, margin = margin(t = half_line)), plot.title.position = "panel", 
      # plot.subtitle = element_text(hjust = 0, vjust = 1, margin = margin(t = half_line)), 
      # plot.caption = element_text(size = rel(0.8), hjust = 1, 
      #                             vjust = 1, margin = margin(t = half_line)), plot.caption.position = "panel", 
      # plot.tag = element_text(size = rel(1.2), hjust = 0.5, 
      #                         vjust = 0.5), plot.tag.position = "topleft", complete = TRUE
    )
  }

