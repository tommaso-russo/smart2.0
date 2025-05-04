getCore = function(full_model_agg_mean, 
                   grid_centroid, 
                   opt_var,
                   features_vars,
                   ndigits = 4, 
                   target = 0.90,
                   gap = 0.1,
                   edge_factor = 0.5){
  
  df.model = data.frame(id_grid = grid_centroid$id_grid,
                        lon = st_coordinates(grid_centroid)[, 1],
                        lat = st_coordinates(grid_centroid)[, 2])
  df.model = inner_join(df.model, full_model_agg_mean)
  
  core_list = vector(mode = "list", length = length(unique(full_model_agg_mean$Gear)))
  
  for(g in 1:length(unique(full_model_agg_mean$Gear))){
    gdf.model = df.model %>% dplyr::filter(Gear == gears_to_submit[g])
    target_var = (gdf.model %>% dplyr::select(all_of(opt_var)))[,1]
    gdf.model$target_var = target_var
    gdf.model = gdf.model %>% dplyr::select(all_of(c("id_grid", "lon", "lat", "target_var")))
    gdf.model$target_var = gdf.model$target_var/10^(nchar(max(floor(gdf.model$target_var)))-1)
    
    e_df.agg.rast = extent(data.frame(x = gdf.model$lon, y = gdf.model$lat))
    r_df.agg.rast = raster(e_df.agg.rast, 
                           ncol = length(unique(round(df.model$lat, 2))), 
                           nrow = length(unique(round(df.model$lon, 2))))
    df.agg.sdf = SpatialPointsDataFrame(coords = gdf.model[, c("lon", "lat")], 
                                        data = as.data.frame(gdf.model[, c("target_var")]), 
                                        proj4string = CRS("EPSG:4326"), 
                                        bbox = NULL)
    r_new_df.agg.rast = rasterize(df.agg.sdf, y = r_df.agg.rast, 
                                  values = as.numeric(df.agg.rast$target_var),
                                  fun = mean, na.rm = TRUE, touches = TRUE)
    pre_grid_rast = data.frame(x = coordinates(r_new_df.agg.rast)[,1],
                               y = coordinates(r_new_df.agg.rast)[,2],
                               revenues = as.data.frame(r_new_df.agg.rast)[,2])
    grid_rast <- as_spatraster(pre_grid_rast, xycols = 1:2, crs = "EPSG:3857", digits = ndigits)
    
    df.agg.effo = full_model_agg_mean  %>% 
      dplyr::filter(Gear == unique(full_model_agg_mean$Gear)[g]) %>% 
      dplyr::select(all_of(c("id_grid", features_vars)))
    features1 = (df.agg.effo %>% dplyr::select(all_of(features_vars[1])))[,1]
    features2 = (df.agg.effo %>% dplyr::select(all_of(features_vars[2])))[,1]
    df.agg.effo = df.agg.effo  %>% dplyr::select(all_of(c("id_grid")))
    df.agg.effo$features1 = features1/10^(nchar(max(floor(features1)))-1)
    df.agg.effo$features2 = features2/10^(nchar(max(floor(features2)))-1)
    df.agg.effo.rast = left_join(df.agg.effo, df.model)[, c("lon", "lat", "features1", "features2")]
    df.agg.effo.sdf = SpatialPointsDataFrame(coords = df.agg.effo.rast[, c("lon", "lat")], 
                                             data = as.data.frame(df.agg.effo.rast[, c("features1", "features2")]), 
                                              proj4string = CRS("EPSG:4326"), 
                                              bbox = NULL)
    r_new_df.agg.effo.rast = rasterize(df.agg.effo.sdf, 
                                       y = r_df.agg.rast, 
                                       values = as.data.frame(df.agg.effo.rast[, c("features1", "features2")]),
                                       fun = mean, 
                                       na.rm = TRUE)
    pre_grid_effo.rast = data.frame(x = coordinates(r_new_df.agg.effo.rast)[,1],
                                    y = coordinates(r_new_df.agg.rast)[,2],
                                    features1 = as.data.frame(r_new_df.agg.effo.rast)[,2],
                                    features2 = as.data.frame(r_new_df.agg.effo.rast)[,3])
    grid_rast_features <- as_spatraster(pre_grid_effo.rast, 
                                        xycols = 1:2, 
                                        crs = "EPSG:3857", 
                                        digits = ndigits)
    
    # Core Fishing ground 90%
    penalty = 100
    budget <- terra::global(grid_rast, "sum", na.rm = TRUE)[[1]] * 0.1
    df.features = left_join(df.agg.effo, df.model)[,c("id_grid", "lon", "lat", "features1", "features2")]
    
    pf1 <- problem(grid_rast, 
                   features = grid_rast_features) %>%
      add_min_set_objective() %>%
      add_relative_targets(target) %>%
      add_binary_decisions() %>%
      add_rsymphony_solver(gap = gap) 
    
    pf2 <- pf1 %>%
      add_boundary_penalties(penalty = penalty, edge_factor = edge_factor)
    sf2 <- solve(pf2, force = TRUE)
    print(attr(sf2, "objective"))
    
    Opt = as.data.frame(sf2, xy = T)
    
    
    grid_opt = data.frame(id_grid = grid_centroid$id_grid,
                    lon = st_coordinates(grid_centroid)[,1],
                    lat = st_coordinates(grid_centroid)[,2])
    
    grid_opt$core =  factor(c("Marginal", "Core")[as.numeric(extract(sf2, 
                                                                     data.matrix(grid_opt[, c("lon", "lat")]))[,1]) + 1], 
                            levels = c("Marginal", "Core"))

  
    Opt = data.frame(id_grid = grid_opt$id_grid, 
                     Gear = unique(full_model_agg_mean$Gear)[g],
                     core = grid_opt$core)
    
    core_list[[g]] = Opt
    
  }
  return(core_list)
}



