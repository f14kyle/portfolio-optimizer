# nsga2(fn, idim, odim, ...,
#      constraints = NULL, cdim = 0,
#      lower.bounds = rep(-Inf, idim), upper.bounds = rep(Inf, idim),
#      popsize = 100, generations = 100,
#      cprob = 0.7, cdist = 5,
#      mprob = 0.2, mdist = 10,
#      vectorized=FALSE)

library(mco)


vnt <- function(x) {
  y <- numeric(3)
  xn <- crossprod(x, x)
  y[1] <- xn/2 + sin(xn);
  y[2] <- (crossprod(c(3, -2), x) + 4)^2/8 + (crossprod(c(1, -1), x) + 1)^2/27 + 15
  y[3] <- 1/(xn + 1) - 1.1*exp(-xn)
  return (y)
}

r2 <- nsga2(vnt, 2, 3,
            generations=150, popsize=100,
            lower.bounds=rep(-3, 2),
            upper.bounds=rep(3, 2))


