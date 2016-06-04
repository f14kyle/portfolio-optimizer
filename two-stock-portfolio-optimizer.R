library(quantmod)
library(lubridate)
library(mco)


date.start = ymd("20101231")
date.end = ymd("20151231")
getSymbols("VOO",src = "yahoo")
getSymbols("GBF",src = "yahoo")


# Calculate 2010 to 2015 average return

# Calculate CAGR

VOO.start = coredata(VOO[date.start,6])
VOO.end = coredata(VOO[date.end,6])

GBF.start = coredata(GBF[date.start,6])
GBF.end = coredata(GBF[date.end,6])

VOO.cagr = (VOO.end/VOO.start)^(1/5) - 1
GBF.cagr = (GBF.end/GBF.start)^(1/5) - 1



# Calculate SD
VOO.history = VOO['2010-12-31::2015-12-31',6]
GBF.history = GBF['2010-12-31::2015-12-31',6]
VOO.sd = sd(VOO.history)/mean(VOO.history)
GBF.sd = sd(GBF.history)/mean(GBF.history)

obj = function(x){
  y <- numeric(2)
  y[1] = x * VOO.cagr + (1 - x) * GBF.cagr
  y[2] = x * VOO.sd^2 + (1 - x) * GBF.sd^2 + 2 * x * (1 - x) * cov(VOO.history,GBF.history)
  return (y)
}

sol = nsga2(obj, 1, 2,
            generations=50, popsize=100,
            lower.bounds=c(0,0),
            upper.bounds=c(1,1))

# nsga2(fn, idim, odim, ...,
#      constraints = NULL, cdim = 0,
#      lower.bounds = rep(-Inf, idim), upper.bounds = rep(Inf, idim),
#      popsize = 100, generations = 100,
#      cprob = 0.7, cdist = 5,
#      mprob = 0.2, mdist = 10,
#      vectorized=FALSE)


