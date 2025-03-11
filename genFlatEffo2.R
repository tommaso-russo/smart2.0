library(data.table)  # Per fifelse()
library(ggplot2)

genFlatEffo2 <- function(effoPatt, noise = 0.01, plot = FALSE) {
  
  # Computation of the probability distribution
  npp_star <- sum(effoPatt)
  if (npp_star == 0) return(rep(0, length(effoPatt)))  
  pj <- effoPatt / npp_star  
  pj <- fifelse(pj > 0, pj + rnorm(length(pj), 0, noise), 0)  
  pj <- fifelse(pj > 0, pj / sum(pj), 0) 
  
  Ecell <- sample(seq_along(effoPatt), size = ceiling(npp_star), prob = pj, replace = TRUE)
  
  Estar <- tabulate(Ecell, nbins = length(effoPatt))
  
  if (plot) {
    xy <- data.frame(ftime = c(effoPatt, pj * npp_star, Estar),
                     source = rep(c("Obs", "Noise", "Simulation"), each = length(effoPatt)))
    print(ggplot(xy, aes(x = ftime, colour = source)) + geom_density() + theme_test())
  }
  
  return(Estar)
}