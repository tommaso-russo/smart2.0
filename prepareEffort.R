prepareEffort <- function(effort, grid_sf, CS_gsas, gears_to_submit){
  
  # Fix some fields
  effort$Gear <- substr(effort$Metier, 1, 3)
  effort$MONTH = factor(month.name[effort$MONTH], levels = month.name)
  effort$YEAR = as.character(effort$YEAR)
  effort$CFR = as.character(effort$CFR)
  
  # Select the effort for the GSA and Gear
  effort = effort %>% 
    filter(GSA %in% CS_gsas, 
           Gear %in% gears_to_submit)
  
  # Reshape the effort as sf object
  effort_sf = st_as_sf(effort, coords  = c("LON", "LAT"), crs = st_crs(grid_sf))
  st_crs(effort_sf) = st_crs(grid_sf)
  
  return(effort_sf)
}