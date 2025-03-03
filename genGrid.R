genGrid <- function(cellsizekm, CS_buffer, crs = 4326, bathy){
  cellSize = c(cellsizekm   * 1/82.5, cellsizekm  * 1/111.3)
  grid_sf = st_make_grid(CS_buffer,
                         cellsize = cellSize, what = "polygons",
                         square = TRUE, crs = 4326, flat_topped = FALSE)
  grid_sf = st_sf(grid_sf)
  inter <- unique(unlist(st_intersects(CS_buffer, grid_sf)))
  over <- unique(unlist(st_crosses(CS_buffer, grid_sf)))
  grid_sf$id_grid <- row.names(grid_sf)
  grid_sf = grid_sf %>% filter(id_grid %in% c(inter, over))
  st_crs(grid_sf) = crs
  grid_centroid = st_centroid(grid_sf)
  
  depth <- -1 * get.depth(mat = bathy, x = st_coordinates(grid_centroid)[, c("X","Y")], locator = FALSE)$depth
  grid_sf$depth = depth
  grid_CS = grid_sf
  
  grid_sf = grid_sf[which(grid_sf$depth <= thr_depth),]
  
  grid_centroid = st_centroid(grid_sf)
  bbox_grid = data.frame(st_bbox(grid_sf))
  outs = list(grid_sf = grid_sf,
              grid_centroid = grid_centroid,
              bbox_grid = bbox_grid,
              grid_CS = grid_CS)
  
  return(outs)
}