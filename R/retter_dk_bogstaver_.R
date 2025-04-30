#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @export
retter_dk_bogstaver_header <- function(input, sprog = "da"){
  
  assert::assert(sprog %in% c("da", "en"))
  
  if(sprog == "da"){
    oversaet_dk <- c("Ã¥"="å", "Ã¸"="ø", "Ã¦"="æ", "Ã†"="Æ", "Ã…"="Å", "Ã˜"="Ø")
  } else if(sprog=="en") {
    oversaet_dk <- c("Ã¥"="aa", "Ã¸"="oe", "Ã¦"="ae", "Ã†"="Ae", "Ã…"="Aa", "Ã˜"="Oe")
  }
  
  colnames(input) <- names(input) %>% 
    stringr::str_replace_all(oversaet_dk)
  
  return(input)
  
}

retter_dk_bogstaver_col <- function(input, sprog = "da"){
  
  assert::assert(sprog %in% c("da", "en"))
  
  if(sprog == "da"){
    oversaet_dk <- c("Ã¥"="å", "Ã¸"="ø", "Ã¦"="æ", "Ã†"="Æ", "Ã…"="Å", "Ã˜"="Ø")
  } else if(sprog=="en") {
    oversaet_dk <- c("Ã¥"="aa", "Ã¸"="oe", "Ã¦"="ae", "Ã†"="Ae", "Ã…"="Aa", "Ã˜"="Oe")
  }
  
  input <- input %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~stringr::str_replace_all(.x, oversaet_dk)))
  
  return(input)
  
}