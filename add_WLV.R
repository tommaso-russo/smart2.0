add_WLV = function(x, tpar){
  x = x %>% dplyr::select(-any_of(c("W", "GVL")))
  x = left_join(x, tpar)
  x$W = 10 * x$effort * x$lpue
  x$GVL = x$tGVL * x$W * x$Price
  x = x %>% dplyr::select(-any_of(c("tEC", "tOC", "tLC", "tGVL")))
  return(x)
}




