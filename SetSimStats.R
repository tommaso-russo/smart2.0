SetSimStats <- function(IBM.sim.ref, n.max.stable.epochs){
  # Set Sim Stats
  patterns = IBM.sim.ref %>% distinct(CFR, YEAR, MONTH, Gear, Metier, loa, ntrip, harbour)
  patterns.epochs = numeric(nrow(patterns))
  n.stable.epochs = numeric(nrow(patterns))
  patterns.to.adapt = which(n.stable.epochs < n.max.stable.epochs)
  vessels.to.adapt = unique(patterns$CFR[patterns.to.adapt])
  vessels.adapted = unique(patterns$CFR[-patterns.to.adapt])
  n.vessels = length(unique(IBM.sim.ref$CFR))
  n_initial_patterns = nrow(patterns)
  n_initial_vessels = length(unique(patterns$CFR))
  set.universe = IBM.sim.ref %>% dplyr::select(id_grid, Gear, Metier) %>% distinct()
  mean.time = numeric(0)
  n.epochs = 0
  n.consex.crowd = 0
  InitSimStats = list(patterns = patterns,
                      patterns.epochs = patterns.epochs,
                      n.stable.epochs = n.stable.epochs,
                      patterns.to.adapt = patterns.to.adapt,
                      vessels.to.adapt = vessels.to.adapt,
                      n.vessels = n.vessels,
                      n_initial_patterns = n_initial_patterns,
                      n_initial_vessels = n_initial_vessels,
                      set.universe = set.universe,
                      mean.time = mean.time,
                      n.epochs = n.epochs,
                      n.consex.crowd = n.consex.crowd)
  return(InitSimStats)
}







