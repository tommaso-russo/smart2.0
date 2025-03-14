Integrate.Sim = function(x, final_fleet){
  x = suppressMessages(left_join(x, unique(data.frame(CFR = as.character(final_fleet$CFR),
                                              MONTH = final_fleet$MONTH,
                                              loa = final_fleet$loa,
                                              ntrip = final_fleet$ntrip,
                                              harbour = final_fleet$HARBOUR))))
  x = suppressMessages(left_join(x, df_lpue_month))
  x = x %>% filter(lpue > 0.005)
  x = suppressMessages(left_join(x, agg_price))
  x = add_WLV(x, tpar)
  y = IBM2eco(x, sala_params, df_harbs_grid_dists,
              fuel_cost, OCfact, INVfact, LCfact, tpar)
  return(y)
}















