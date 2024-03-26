ggamma <- function(N){
  return (sqrt(2 / N) * exp(lgamma(N / 2) - lgamma((N - 1) / 2)))
}