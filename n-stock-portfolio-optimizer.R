library(quantmod)
library(lubridate)
library(mco)

# Domestic: S&P 500
# Internationa: MSCI EAFE
# Bonds: Barclay's U.S. Intermediate Government Treasury Bond Index
# Short-term: U.S. 30 day T-bills

# [US LCV] U.S. large-cap value (regarded as underpriced; value)
# [US SCB] U.S. small-blend (higher return that S&P 500 because more room to grow)
# [US SCV] U.S. small-cap value (most productive)
# [US REIT] U.S. REIT

# [INT LCB] Internatiaonl large-cap blend
# [INT LCV] International large-cap value
# [INT SCB] International small-cap blend
# [INT SCV] International small-cap value

# [EM] Emerging Markets

# In long term
# stocks > bonds
# small cap > large cap
# value > growth

date.start = ymd("20101231")
date.end = ymd("20151231")
getSymbols("VOO",src = "yahoo")
getSymbols("GBF",src = "yahoo")
getSymbols("VNQ",src = "yahoo")


# Calculate 2010 to 2015 average return

# Calculate CAGR

VOO.start = coredata(VOO[date.start,6])
VOO.end = coredata(VOO[date.end,6])

GBF.start = coredata(GBF[date.start,6])
GBF.end = coredata(GBF[date.end,6])

VNQ.start = coredata(GBF[date.start,6])
VNQ.end = coredata(GBF[date.end,6])

VOO.cagr = (VOO.end/VOO.start)^(1/5) - 1
GBF.cagr = (GBF.end/GBF.start)^(1/5) - 1
VNQ.cagr = (VNQ.end/VNQ.start)^(1/5) - 1


# Calculate SD
VOO.history = VOO['2010-12-31::2015-12-31',6]
GBF.history = GBF['2010-12-31::2015-12-31',6]
VNQ.history = VNQ['2010-12-31::2015-12-31',6]
VOO.sd = sd(VOO.history)
GBF.sd = sd(GBF.history)
VNQ.sd = sd(VNQ.history)

# x = respective weights of assets
# y = vector containing total return and total variance

obj = function(x){
  y <- numeric(2)
  W = matrix(c(x[1],x[2],(1 - x[1] - x[2])),nrow = 1)
  y[1] = x[1] * VOO.cagr + x[2] * GBF.cagr + (1 - x[1] - x[2]) * VNQ.cagr
  y[2] = W %*% cov(cbind(VOO.history,GBF.history,VNQ.history)) %*% t(W)
  return (y)
}

sol = nsga2(obj, 2, 2,
            generations=100, popsize=200,
            lower.bounds=c(0,0),
            upper.bounds=c(1,1))

# nsga2(fn, idim, odim, ...,
#      constraints = NULL, cdim = 0,
#      lower.bounds = rep(-Inf, idim), upper.bounds = rep(Inf, idim),
#      popsize = 100, generations = 100,
#      cprob = 0.7, cdist = 5,
#      mprob = 0.2, mdist = 10,
#      vectorized=FALSE)

a <- c(1,2,3,4,5,6)
b <- c(2,3,5,6,1,9)
c <- c(3,5,5,5,10,8)
d <- c(10,20,30,40,50,55)
e <- c(7,8,9,4,6,10)

M = cbind(a,b,c,d,e)
k = ncol(M)
n = nrow(M)
M_mean = matrix(data = 1,nrow = n) %*% cbind(mean(a),mean(b),mean(c),mean(d),mean(e))
D = M - M_mean
