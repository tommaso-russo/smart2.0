gridDistance <- function(grid_centroid, grid_sf, harbs_df_sf) {

r <- raster(nrows = 500, ncols = 500, crs = st_crs(grid_sf))
extent(r) <- st_bbox(grid_sf) # Set extent based on the bounding box of grid_sf

# Rasterize grid_sf using fasterize
rtas_pts <- grid_sf %>% mutate(id_grid = as.integer(id_grid)) %>% fasterize( r, field = "id_grid")
rtas_pts <- rast(rtas_pts)

harbs_id <- cellFromXY(rtas_pts, st_coordinates(harbs_df_sf))

for (i in seq_along(harbs_id)) {
  rtmp <- rtas_pts
  rtmp[harbs_id[i]] <- 2
  dist_from_harb_rast <- gridDist(rtmp, target = 2, scale = 0.01)
  dist_from_harb_sf <- cbind(grid_sf, extract(dist_from_harb_rast, st_coordinates(grid_centroid)))
  dist_from_harb_sf$harb <- harbs_df_sf$HARBOUR[i]
  colnames(dist_from_harb_sf)[3] <- "dist"
  if (i == 1) {
    dist_stack <- dist_from_harb_sf
  } else {
    dist_stack <- rbind(dist_stack, dist_from_harb_sf)
  }
}

return(dist_stack)

}

