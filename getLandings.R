# This function generates landings from LPUE, effort and fleet register

getLandings <- function(df_effort, df_lpue, fleet_register){
  
  # Combine effort and fleet register
  df_effort$YEAR <- as.numeric(df_effort$YEAR)
  
  # Join effort and lpue
  df_effort_lpue = full_join(df_effort, 
                             df_lpue,
                             relationship = "many-to-many")
  
  
  # Get landings
  df_effort_lpue$landings = df_effort_lpue$effort * df_effort_lpue$lpue
  
  return(df_effort_lpue)
  
}