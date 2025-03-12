update.IBM.sim <- function(IBM.sim.ref, IBM.sim, improved_patterns, key_columns){
  if(nrow(improved_patterns) > 0){
    cases_to_update <- improved_patterns %>% select(all_of(key_columns)) %>% distinct()
    IBM.sim.ref <- anti_join(IBM.sim.ref, cases_to_update, by = key_columns)
    IBM_2add <- semi_join(IBM.sim, cases_to_update, by = key_columns)
    IBM.sim.ref <- bind_rows(IBM.sim.ref, IBM_2add)
  }
  return(IBM.sim.ref)
}
