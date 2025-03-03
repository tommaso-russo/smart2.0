griddingEffort <- function(effort_sf, grid_sf){
  effort_sf_gridded <- as.data.frame(st_intersects(effort_sf, grid_sf))
  effort_sf$id_grid = NA
  effort_sf$id_grid[effort_sf_gridded$row.id] = as.character(grid_sf$id_grid[effort_sf_gridded$col.id])
  effort_sf_gridded <-  effort_sf %>% filter(!is.na(id_grid))
  return(effort_sf_gridded)
}