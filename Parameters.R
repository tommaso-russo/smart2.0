steaming_speed = 8.5 * 1.852
fuel_cost = data.frame(YEAR = as.character(2020:2023),
                       fuel_cost = 0.62) # Euros per litre
quantile_thrB <- 0.1 # 
quantile_Logit <- 0.05 # 
quantile_Effort <- 0.05 # 
quantile_effort_fleet <- 0.1#
thr_depth <- 2000 # This is the maximum depth of cells (as assigned using marmap) allowed for the analysis
tfuel = 1.5
tpar = data.frame(Gear = c("OTB", "LLS"),
                   tEC = c(1.2 * 3, 1.2 * 40), 
                   tOC = c(1, 1.5),
                   tLC = c(0.4, 1.2),
                   tGVL = c( 1.5, 2.7))




