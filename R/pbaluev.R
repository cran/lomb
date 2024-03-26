
pbaluev <- function(Z,fmax,tm) {
  #code copied from astropy timeseries
  N=length(tm)
  Dt=mean(tm^2)-mean(tm)^2
  NH=N-1
  NK=N-3
  fsingle=(1 - Z) ^ (0.5 * NK)
  Teff = sqrt(4 * pi * Dt) # Effective baseline
  W = fmax * Teff
  tau=ggamma(NH) * W * (1 - Z) ^ (0.5 * (NK - 1))*sqrt(0.5 * NH * Z)
  p=-(exp(-tau)-1) + fsingle * exp(-tau)
  return(p)
}
