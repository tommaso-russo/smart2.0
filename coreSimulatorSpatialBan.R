coreSimulatorSpatialBan <- function(x, 
                                      patterns.to.adapt, 
                                      patterns, 
                                      fra.cells,
                                      effort.regime = 100,
                                      scale.factor = 100,
                                      apply.regime = F) {
  
  setDT(x)
  setDT(patterns)
  
  results <- vector("list", length(patterns.to.adapt))
  
  for (i in seq_along(patterns.to.adapt)) {
    k <- patterns.to.adapt[i]
    
    x.k <- x[CFR == patterns$CFR[k] & 
             YEAR == patterns$YEAR[k] &
             MONTH == patterns$MONTH[k] & 
             Gear == patterns$Gear[k] & 
             Metier == patterns$Metier[k] & 
             harbour == patterns$harbour[k] & 
             loa == patterns$loa[k] & 
             ntrip == patterns$ntrip[k]]
    
    k.universe <- unique(x[MONTH == patterns$MONTH[k] & 
                           YEAR == patterns$YEAR[k] &
                           Gear == patterns$Gear[k] & 
                           Metier == patterns$Metier[k] & 
                           harbour == patterns$harbour[k], 
                           .(id_grid)])
    
    dfk <- data.table(CFR = patterns$CFR[k], 
                      YEAR = patterns$YEAR[k],
                      MONTH = patterns$MONTH[k], 
                      Gear = patterns$Gear[k],
                      Metier = patterns$Metier[k], 
                      loa = patterns$loa[k],
                      ntrip = patterns$ntrip[k], 
                      harbour = patterns$harbour[k],
                      id_grid = setdiff(k.universe$id_grid, fra.cells), 
                      fishing_time = 0)
    
    x.k.agg <- x.k[, .(fishing_time = sum(fishing_time)), by = id_grid]
    x.k.agg$fishing_time = sum(x.k$fishing_time) * x.k.agg$fishing_time/sum(x.k.agg$fishing_time)
    df.k.tot <- ifelse(apply.regime, (effort.regime / 100), 1) * sum(x.k.agg$fishing_time)
    setkey(dfk, id_grid)
    setkey(x.k.agg, id_grid)
    dfk[x.k.agg, fishing_time := i.fishing_time]
    
    eff.star <- genFlatEffo2(effoPatt = scale.factor * dfk$fishing_time, 
                             noise = 0.005, plot = FALSE)/scale.factor
    dfk[, fishing_time := eff.star]
    dfk <- dfk[fishing_time > 0]
    results[[i]] <- dfk
  }
  
  new_patterns <- rbindlist(results)
  new_patterns <- new_patterns[id_grid %in% x$id_grid]
   setkey(x, CFR, YEAR, MONTH, Gear, Metier, harbour, loa, ntrip)
  setkey(patterns, CFR, YEAR, MONTH, Gear, Metier, harbour, loa, ntrip)
  x <- x[!patterns[patterns.to.adapt], on = c(key_columns)]
  new.x <- rbind(x, new_patterns)
  return(new.x)
}