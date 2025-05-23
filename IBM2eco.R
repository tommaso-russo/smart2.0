IBM2eco = function(x, params, df_harbs_grid_dists,
                   fuel_cost, OCfact, INVfact, LCfact, 
                   tpar){
  
  
  x_agg_fishing_time <-  x %>% dplyr::select(c("CFR", "YEAR", "MONTH", "Gear", "Metier", 
                                               "harbour", "loa", "ntrip", "fishing_time", "id_grid")) %>% distinct()
  
  x_agg_fishing_time =  aggregate(data = x_agg_fishing_time,
                                  fishing_time ~ ., FUN = "sum" , na.rm = TRUE)
  
  x_agg_fishing_time =  suppressMessages(left_join(x_agg_fishing_time,
                                  df_harbs_grid_dists, 
                                  by = c("id_grid", "harbour")))
  
  x_agg_fishing_time =  x_agg_fishing_time[complete.cases(x_agg_fishing_time), ]
  
  x_agg_WD = x_agg_fishing_time %>%
    group_by(CFR, YEAR, MONTH, Gear, Metier, harbour, loa, ntrip) %>%
    summarise(MeanWeightDistance = weighted.mean(distance, fishing_time, na.rm = TRUE), .groups = "drop")
  
  x_agg_sum = x %>%
    group_by(CFR, MONTH, Gear, Metier, harbour, loa, ntrip) %>%
    summarize(across(c(W, GVL), sum, na.rm = TRUE))
  
  # Combine alltogheter
  x_agg = suppressMessages(inner_join(as.data.frame(x_agg_sum), 
                     as.data.frame(x_agg_WD)))
  x_agg = inner_join(x_agg, 
                     aggregate(data = x_agg_fishing_time,
                               fishing_time ~ CFR + YEAR + MONTH + Gear + Metier + harbour,
                               FUN = "sum"))
  
  x_agg = suppressMessages(left_join(x_agg, tpar, relationship = "many-to-many"))
  x_agg$steaming_time = tfuel * (x_agg$MeanWeightDistance/steaming_speed) * x_agg$ntrip
  x_agg = suppressMessages(left_join(x_agg, params, relationship = "many-to-many"))
  x_agg$FC = x_agg$q * x_agg$loa^x_agg$m * (x_agg$fishing_time + x_agg$steaming_time)
  x_agg = suppressMessages(left_join(x_agg, fuel_cost))
  x_agg$EC = x_agg$tEC * (x_agg$FC * x_agg$fuel_cost)
  x_agg$OC = x_agg$tOC * x_agg$GVL * OCfact
  x_agg$INV = x_agg$GVL * INVfact
  x_agg$LC = x_agg$tLC * x_agg$GVL * LCfact
  x_agg$GVA = x_agg$GVL - x_agg$EC - x_agg$OC - x_agg$INV - x_agg$LC
  x_agg$GPM = x_agg$GVA/x_agg$GVL
  x_agg = x_agg %>% dplyr::select(-c(tEC, tOC, tLC))
  return(x_agg)
}
