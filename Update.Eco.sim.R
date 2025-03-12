Update.Eco.sim <- function(x, df_lpue_rates, agg_price, tpar, sala_params,
                           df_harbs_grid_dists, fuel_cost, OCfact, INVfact, LCfact){
  
  x$effort = x$fishing_time * x$loa
  x = inner_join(x, data.frame(MONTH = factor(df_lpue_rates$MONTH, levels = month.name),
                               id_grid = df_lpue_rates$id_grid,
                               Species = df_lpue_rates$Species,
                               lpue = df_lpue_rates$lpue), relationship = "many-to-many")
  
  x = x %>% filter(lpue > 0.005)
  x = left_join(x, agg_price, relationship = "many-to-many")
  x = add_WLV(x, tpar)
  
  # dim(x %>% dplyr::select(CFR, MONTH, Gear, Metier, loa, ntrip, harbour) %>% distinct())
  # length(unique(x$CFR))
  
  # x = x %>% filter(CFR == 9524)
  
  x.eco = IBM2eco(x, sala_params, df_harbs_grid_dists,
                  fuel_cost, OCfact, INVfact, LCfact, tpar)    
  
  
  dim(x.eco %>% dplyr::select(CFR, MONTH, Gear, Metier, loa, ntrip, harbour) %>% distinct())
  length(unique(x.eco$CFR))
  

  x.eco$VL = factor(c("VL0006", "VL0612", "VL1215", 
                            "VL1518", "VL1824", "VL2440", "VL40XX")[findInterval(x.eco$loa, 
                                                                                 c(0, 6, 12, 15, 18, 24, 40, Inf))], 
                          levels = c("VL0006", "VL0612", "VL1215", "VL1518", 
                                     "VL1824", "VL2440", "VL40XX"))
  return(x.eco)
}
