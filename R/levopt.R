levopt<-  function(x,alpha,fmax,tm){
  prob=pbaluev(x,fmax,tm)
  (log(prob)-log(alpha))^2
}