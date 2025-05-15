getSwa <- function(speed, loa, a = 3.9273, b = 35.8254, deltat = 1/6){
  SwaT <- speed * deltat * (a * loa + b)
  return(SwaT)
}


