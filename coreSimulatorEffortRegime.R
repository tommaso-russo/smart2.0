coreSimulatorEffortRegime <- function(x, 
                                      patterns.to.adapt, 
                                      patterns, 
                                      effort.regime = 100,
                                      scale.factor = 100,
                                      apply.regime = F,
                                      noise = 0.005) {
  
  # Convertiamo IBM.sim e patterns in data.table per migliorare la velocità
  setDT(x)
  setDT(patterns)
  
  # Preallocare una lista per raccogliere i risultati
  results <- vector("list", length(patterns.to.adapt))
  
  for (i in seq_along(patterns.to.adapt)) {
    k <- patterns.to.adapt[i]
    
    # Filtraggio rapido usando chiavi
    x.k <- x[CFR == patterns$CFR[k] & 
             YEAR == patterns$YEAR[k] &
             MONTH == patterns$MONTH[k] & 
             Gear == patterns$Gear[k] & 
             Metier == patterns$Metier[k] & 
             harbour == patterns$harbour[k] & 
             loa == patterns$loa[k] & 
             ntrip == patterns$ntrip[k]]
    
    # Selezioniamo id_grid unici in modo più efficiente
    k.universe <- unique(x[MONTH == patterns$MONTH[k] & 
                           YEAR == patterns$YEAR[k] &
                           Gear == patterns$Gear[k] & 
                           Metier == patterns$Metier[k] & 
                           harbour == patterns$harbour[k], 
                           .(id_grid)])
    
    # Creiamo il nuovo data.table dfk
    dfk <- data.table(CFR = patterns$CFR[k], 
                      YEAR = patterns$YEAR[k],
                      MONTH = patterns$MONTH[k], 
                      Gear = patterns$Gear[k],
                      Metier = patterns$Metier[k], 
                      loa = patterns$loa[k],
                      ntrip = patterns$ntrip[k], 
                      harbour = patterns$harbour[k],
                      id_grid = k.universe$id_grid, 
                      fishing_time = 0)
    
    # Aggregazione con data.table (più veloce di aggregate)
    x.k.agg <- x.k[, .(fishing_time = sum(fishing_time)), by = id_grid]
    
    # Calcolo del tempo di pesca
    df.k.tot <- ifelse(apply.regime, (effort.regime / 100), 1) * sum(x.k.agg$fishing_time)
    
    # Aggiornare fishing_time
    setkey(dfk, id_grid)
    setkey(x.k.agg, id_grid)
    dfk[x.k.agg, fishing_time := i.fishing_time]
    
    # Generazione di eff.star
    eff.star <- genFlatEffo2(effoPatt = scale.factor * dfk$fishing_time, 
                             noise = 0.005, plot = FALSE)/scale.factor
    dfk[, fishing_time := eff.star]
    
    # Rimuoviamo righe con fishing_time = 0
    dfk <- dfk[fishing_time > 0]
    
    # Salviamo il risultato nella lista
    results[[i]] <- dfk
  }
  
  # Combiniamo i risultati in un unico data.table
  new_patterns <- rbindlist(results)
  
  # Rimuoviamo le vecchie righe in un'unica operazione
  setkey(x, CFR, YEAR, MONTH, Gear, Metier, harbour, loa, ntrip)
  setkey(patterns, CFR, YEAR, MONTH, Gear, Metier, harbour, loa, ntrip)
  x <- x[!patterns[patterns.to.adapt], on = c(key_columns)]
  
  # Uniamo i nuovi dati a IBM.sim
  new.x <- rbind(x, new_patterns)
  
  return(new.x)
}
