library(xts)
library(Quandl)
library(quantmod)
library(zoo)
library(PerformanceAnalytics)
library(fOptions)
library(MASS)

index.tickers <- c("^GSPC","^RUT","^DJI","^IXIC")
prices <- getSymbols(index.tickers[1], src="yahoo", from="2004-01-01", to="2018-12-31",
                     auto.assign=FALSE,return.class="xts")[,6]
for (i in 2:length(index.tickers)) {
  prices.tmp <- getSymbols(index.tickers[i], src="yahoo", from="2004-01-01", to="2018-12-31",
                           auto.assign=FALSE,return.class="xts")[,6]
  prices <- cbind(prices, prices.tmp)
}
index.names <- c("SPX","RUT","DJI","IXIC")
colnames(prices) <- index.names
log_returns <- na.omit(diff(log(prices)))
simple_returns <- na.omit(Return.calculate(prices,method = "discrete"))
simple_returns <- cbind(simple_returns,rowSums(simple_returns))
colnames(simple_returns) <- c("SPX","RUT","DJI","IXIC","PORTSUM")
portfolio_returns <- simple_returns$PORTSUM*1000000
head(portfolio_returns)
portfolio_returns_df <- data.frame(portfolio_returns)
portfolio_returns_df$DATE <- rownames(portfolio_returns_df)
i1 <- which(index(portfolio_returns)==index(portfolio_returns["2008-01-02"]))
i2 <- which(index(portfolio_returns)==index(portfolio_returns["2009-12-31"]))

#Question 1: Historical Simulated VaR and ES
historical_VaR = 0
for (i in i1:i2)
{historical_VaR[i+1-i1] = abs(quantile(portfolio_returns[(i-1000):(i-1)],p=0.05))}
date <- portfolio_returns_df$DATE[i1:i2]
historical_VaR_df <- data.frame(date,historical_VaR)
plot(historical_VaR_df$date,historical_VaR_df$historical_VaR, type = 'l',xlab = "Date",ylab="VAR",ylim=c(50000,200000),col="black")
es = 0
temp <- 0
k = 1
for (j in i1:i2){
  temp <- portfolio_returns[(j-1000):(j-1)]
  es[j+1-i1] = -mean(temp[temp <= (-historical_VaR[k])])
  k=k+1
}
es_df<-data.frame(date,es)
lines(es_df$date,es_df$es,col="blue")
legend(x="topleft",y=95,legend=c("Hist. Sim. VaR","Hist. Sim. ES"),col=c("black","blue"),
       lty = c(1, 1, 1), pch = c(NA, NA, NA))

#Question 2:
fv_spx_call <- ((19.70+18.70)/2)
fv_spx_put <- ((21.10+20.00)/2)
fv_djx_call <- ((1.96+1.80)/2)
fv_djx_put <- ((2.37+2.14)/2)
imv_spx_call <- GBSVolatility(fv_spx_call,TypeFlag="c",S = 2564.98, X = 2565,
                              Time = (25/365), r = 0.0124236, b = 0.0124236 - 0.0164791)
imv_spx_put <- GBSVolatility(fv_spx_put,TypeFlag="p",S = 2564.98, X = 2565,
                              Time = (25/365), r = 0.0124236, b = 0.0124236 - 0.0164791)
imv_djx_call <- GBSVolatility(fv_djx_call,TypeFlag="c",S = 232.74, X = 233,
                              Time = (25/365), r = 0.0124236, b = 0.0124236 - 0.0236134)
imv_djx_put <- GBSVolatility(fv_djx_put,TypeFlag="p",S = 232.74, X = 233,
                              Time = (25/365), r = 0.0124236, b = 0.0124236 - 0.0236134)
#Question 3, Monte Carlo
corr_mat <- c(1.00,0.97,-0.80,-0.75,0.97,1.00,-0.75,-0.80,-0.80,-0.75,1.00,0.90,-0.75,
             -0.80,0.90,1.00)
dim(corr_mat) <- c(4,4)
std_dev<-c(0.0105,0.0110,0.125,0.1150)
cov_mat <- matrix(,nrow = 4,ncol = 4)
for (i in 1:4){
  for (j in 1:4){
    cov_mat[i,j] <- corr_mat[i,j]*std_dev[i]*std_dev[j]
  }
}
mean <- c(0.0001,0.0001,0.0078,0.0066)
set.seed(67)
random <- mvrnorm(10000,mu=mean,Sigma = cov_mat)
port_int_val <- -50*100*fv_spx_call-50*100*fv_spx_put+600*100*fv_djx_call+600*100*fv_djx_put
port_final_val <- 0
for(i in 1:10000){
  port_final_val[i] <- -50*100*GBSOption(TypeFlag = "c",S = (2564.98*exp(random[i,1])),X = 2565, Time = (24/365),r = 0.0124236, b = (0.0124236 - 0.0164791), sigma = (imv_spx_call*exp(random[i,3])))@price-50*100*GBSOption(TypeFlag = "p",S = (2564.98*exp(random[i,1])),X = 2565, Time = (24/365),r = 0.0124236, b = (0.0124236 - 0.0164791), sigma = (imv_spx_put*exp(random[i,3])))@price+600*100*GBSOption(TypeFlag = "c",S = (232.74*exp(random[i,2])),X = 233, Time = (24/365),r = 0.0124236, b = (0.0124236 - 0.0236134), sigma = (imv_djx_call*exp(random[i,4])))@price+600*100*GBSOption(TypeFlag = "p",S = (232.74*exp(random[i,2])),X = 233, Time = (24/365),r = 0.0124236, b = (0.0124236 - 0.0236134), sigma = (imv_djx_put*exp(random[i,4])))@price
}
port_pnl <- port_final_val-port_int_val
VaR_MC <- -quantile(port_pnl,p=0.05)
#Question 4, Expected Shortfall
ES_MC <- -mean(port_pnl[port_pnl<=-VaR_MC])
