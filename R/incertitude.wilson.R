WilsonBinCI <-  function(n, p, a=0.05) {
  z <- qnorm(1-a/2,lower.tail=T)
 # n<-n

  #
    if (n<30) {
  J13<-2*p*n+z^2
  K13<-z^2-2-1/n+4*p*(n*(1-p)+1)
  M13<-2*(n+z^2)
  L13<-z^2+2-1/n+(4*p*(n*(1-p)-1))
  l<-(J13-1-z*sqrt(K13))/M13
  u<-(J13+1+z*sqrt(L13))/M13
   }  else {
  u <- 1/(1+1/n*z^2)*(p + 1/2/n*z^2 + 
                           z*sqrt(1/n*p*(1-p) + 1/4/n^2*z^2))
   l <- 1/(1+1/n*z^2)*(p + 1/2/n*z^2 -
                           z*sqrt(1/n*p*(1-p) + 1/4/n^2*z^2))
   } 


    bornes <- c(l,u)

  return(bornes)

}


