kde2d.weighted2<-function (x, y, h, n = 25, lims = c(range(x), range(y)), w) 
{
  nx <- length(x)
  if (length(y) != nx) 
    stop("data vectors must be the same length")
  if (length(w) != nx & length(w) != 1) 
    stop("weight vectors must be 1 or length of data")
  gx <- seq(lims[1], lims[2], length = n)
  gy <- seq(lims[3], lims[4], length = n)
  h <- if (missing(h)) 
    c(bandwidth.nrd(x), bandwidth.nrd(y))
  else rep(h, length.out = 2L)
  if (any(h <= 0)) 
    stop("bandwidths must be strictly positive")
  if (missing(w)) 
    w <- numeric(nx) + 1
  h <- h/4
  ax <- outer(gx, x, "-")/h[1]
  ay <- outer(gy, y, "-")/h[2]
  z <- (matrix(rep(w, n), nrow = n, ncol = nx, byrow = TRUE) * 
          matrix(dnorm(ax), n, nx)) %*% t(matrix(dnorm(ay), n, 
                                                 nx))/(sum(w) * h[1] * h[2])
  return(list(x = gx, y = gy, z = z))
}