Update.Eco.sim <- function(IBM.sim, df_lpue_rates, agg_price, tpar, sala_params,
                           df_harbs_grid_dists, fuel_cost, OCfact, INVfact, LCfact){
  IBM.sim$effort = IBM.sim$fishing_time * IBM.sim$loa
  IBM.sim = suppressMessages(inner_join(IBM.sim, 
                       data.frame(MONTH = factor(df_lpue_rates$MONTH, levels = month.name),
                                  id_grid = df_lpue_rates$id_grid,
                                  Species = df_lpue_rates$Species,
                                  lpue = df_lpue_rates$lpue), relationship = "many-to-many"))
  IBM.sim = IBM.sim %>% filter(lpue > 0.005)
  IBM.sim = suppressMessages(left_join(IBM.sim, agg_price, relationship = "many-to-many"))
  IBM.sim = add_WLV(IBM.sim, tpar)
  IBM.sim.eco = IBM2eco(IBM.sim, sala_params, df_harbs_grid_dists,
                        fuel_cost, OCfact, INVfact, LCfact, tpar)    
  
  IBM.sim.eco$VL = factor(c("VL0006", "VL0612", "VL1215", 
                            "VL1518", "VL1824", "VL2440", "VL40XX")[findInterval(IBM.sim.eco$loa, 
                                                                                 c(0, 6, 12, 15, 18, 24, 40, Inf))], 
                          levels = c("VL0006", "VL0612", "VL1215", "VL1518", 
                                     "VL1824", "VL2440", "VL40XX"))
  return(IBM.sim.eco)
}


