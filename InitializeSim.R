InitializeSim <- function(effort_sf_gridded_list, IBM, final_fleet, key_columns, crowding.factor){
  IBM.sim.ref = as.data.frame(rbindlist(effort_sf_gridded_list)) %>%
    dplyr::select(CFR, YEAR, MONTH, Gear, Metier,id_grid, loa, fishing_time) %>% distinct()
  IBM.sim.ref = IBM.sim.ref %>% filter(id_grid %in% IBM$id_grid)
  IBM.sim.ref = left_join(IBM.sim.ref, unique(data.frame(CFR = as.character(final_fleet$CFR),
                                                         MONTH = final_fleet$MONTH,
                                                         loa = final_fleet$loa,
                                                         ntrip = final_fleet$ntrip,
                                                         harbour = final_fleet$HARBOUR))) %>% filter(harbour %in% df_harbs_grid_dists$harbour)
  IBM.sim.ref = aggregate(data = IBM.sim.ref,
                          . ~ CFR + YEAR + MONTH + Gear + Metier + id_grid + ntrip + loa + harbour,
                          FUN = "mean")
  IBM.sim.ref$effort = IBM.sim.ref$loa * IBM.sim.ref$fishing_time
  IBM.effo = aggregate(data = IBM.sim.ref %>% dplyr::select(any_of(c(key_columns, "id_grid", "effort"))) %>% distinct(),
                       effort ~ id_grid + MONTH, FUN = "sum", na.rm = TRUE)
  maxE <- crowding.factor * max(IBM.effo$effort)
  InitList = list(IBM.sim.ref = IBM.sim.ref,
                  IBM.effo = IBM.effo, 
                  maxE = maxE)
  return(InitList)
}



