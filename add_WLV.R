add_WLV = function(x){
  x = x %>% dplyr::select(-any_of(c("W", "GVL")))
  x = left_join(x, tpar)
  x$W = x$effort * x$lpue 
  x$GVL = tGVL * x$W * x$Price
  x = x %>% dplyr::select(-tGVL)
  return(x)
}



