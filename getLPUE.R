getLPUE = function(xy){
  
  # Get nnls_x
  nnls_x = data.matrix( xy %>% dplyr::select(-c(CFR, MONTH, Kg, Gear)))
  if(nrow(which(is.na(nnls_x), arr.ind = TRUE)) > 0) 
    nnls_x[which(is.na(nnls_x), arr.ind = TRUE)] = 0
  coltot = apply(nnls_x, 2, sum)
  coltot_pos = coltot[which(coltot > 0)]
  
  nnls_x = nnls_x[,which(apply(nnls_x, 2, sum) > quantile(apply(nnls_x, 2, sum), 0.3))]
  id_grid_lpue = colnames(nnls_x)
  
  # Get nnls_y
  nnls_y = xy$Kg
  
  nnls_xy = nnls(nnls_x, nnls_y)
  
  lpue = data.frame(id_grid = id_grid_lpue,
                    lpue = as.numeric(nnls_xy$x))
  
  df_check = data.frame(obs = nnls_y, fitted = nnls_xy$fitted)

  
  return(list(lpue = lpue, df_check = df_check))
}