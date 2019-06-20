# team members: Harsh Baheti, Chinmayi Kargal Manjunath, Aparna Kesarkar, Utkarsh Mishra

library(xts)
library(Quandl)
library(quantmod)
library(zoo)
library(PerformanceAnalytics)


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
portfolio_returns_df <- data.frame(portfolio_returns)
portfolio_returns_df$DATE <- rownames(portfolio_returns_df)
i1 <- which(index(portfolio_returns)==index(portfolio_returns["2007-12-31"]))
i2 <- which(index(portfolio_returns)==index(portfolio_returns["2009-12-31"]))

#Question 1, Historical Simulation
historical_VaR = 0
for (i in i1:i2)
  {historical_VaR[i+1-i1] = abs(quantile(portfolio_returns[(i-1001):(i-1)],p=0.01))}
date <- portfolio_returns_df$DATE[i1:i2]
historical_VaR_df <- data.frame(date,historical_VaR)
plot(historical_VaR_df$date,historical_VaR_df$historical_VaR, type = 'l',xlab = "Date",ylab="VAR", ylim=c(100000, 500000),col="black")

#Question 2, Delta-Normal method with the equally-weighted covariance matrix estimator
covariance <- function(x,y){
  a = 0
  for (i in i1:i2){
     a[i-i1+1] <- sum(x[(i-1001):(i-1)]*y[(i-1001):(i-1)])/1000
  }
  return(a)
}
cov_spx_rut <- covariance(simple_returns$SPX,simple_returns$RUT)
cov_spx_dji <- covariance(simple_returns$SPX,simple_returns$DJI)
cov_spx_ixic <- covariance(simple_returns$SPX,simple_returns$IXIC)
cov_rut_dji <- covariance(simple_returns$RUT,simple_returns$DJI)
cov_rut_ixic <- covariance(simple_returns$RUT,simple_returns$IXIC)
cov_dji_ixic <- covariance(simple_returns$DJI,simple_returns$IXIC)

variance <- function(k){
  b = 0
  for (i in i1:i2){
    b[i-i1+1] <- sum(k[(i-1001):(i-1)]*k[(i-1001):(i-1)])/1000
  }
  return(b)
}
varinace_spx <- variance(simple_returns$SPX)
variance_rut <- variance(simple_returns$RUT)
variance_dji <- variance(simple_returns$DJI)
variance_ixic <- variance(simple_returns$IXIC)

variance_port <- (1000000*1000000)*(varinace_spx+variance_rut+variance_dji+variance_ixic+(2*cov_spx_rut)+
                                      (2*cov_spx_dji)+(2*cov_spx_ixic)+(2*cov_rut_dji)+(2*cov_rut_ixic)+
                                      (2*cov_dji_ixic))
delta_normal_VaR <- -qnorm(0.01)*sqrt(variance_port)
delta_normal_VaR_df <- data.frame(date,delta_normal_VaR)
lines(delta_normal_VaR_df$date, delta_normal_VaR_df$delta_normal_VaR,col="blue")

#Question 3, Delta-Normal method using the exponentially-weighted covariance estimator
lambda = 0.94
l <- vector(mode='numeric',length = 100)
l[1] = 0.06
for(i in 2:101){
  l[i] = lambda*l[i-1]
}
covariance_exp <- function(x,y){
  c = 0
  for (i in i1:i2){
    c[i-i1+1] <- sum(l*x[(i-101):(i-1)]*y[(i-101):(i-1)])
  }
  return(c)
}
cov_spx_rut_exp <- covariance_exp(simple_returns$SPX,simple_returns$RUT)
cov_spx_dji_exp <- covariance_exp(simple_returns$SPX,simple_returns$DJI)
cov_spx_ixic_exp <- covariance_exp(simple_returns$SPX,simple_returns$IXIC)
cov_rut_dji_exp <- covariance_exp(simple_returns$RUT,simple_returns$DJI)
cov_rut_ixic_exp <- covariance_exp(simple_returns$RUT,simple_returns$IXIC)
cov_dji_ixic_exp <- covariance_exp(simple_returns$DJI,simple_returns$IXIC)

variance_exp <- function(k){
  d = 0
  for (i in i1:i2){
    d[i-i1+1] <- sum(l*k[(i-101):(i-1)]*k[(i-101):(i-1)])
  }
  return(d)
}
varinace_spx_exp <- variance_exp(simple_returns$SPX)
variance_rut_exp <- variance_exp(simple_returns$RUT)
variance_dji_exp <- variance_exp(simple_returns$DJI)
variance_ixic_exp <- variance_exp(simple_returns$IXIC)

variance_port_exp <- (1000000*1000000)*(varinace_spx_exp+variance_rut_exp+variance_dji_exp+variance_ixic_exp+(2*cov_spx_rut_exp)+
                                      (2*cov_spx_dji_exp)+(2*cov_spx_ixic_exp)+(2*cov_rut_dji_exp)+(2*cov_rut_ixic_exp)+
                                      (2*cov_dji_ixic_exp))
delta_normal_VaR_exp <- -qnorm(0.01)*sqrt(variance_port_exp)
delta_normal_VaR_exp_df <- data.frame(date,delta_normal_VaR_exp)
tail(delta_normal_VaR_exp_df)
lines(delta_normal_VaR_exp_df$date, delta_normal_VaR_exp_df$delta_normal_VaR_exp,col="red", ylim=c(0,6000000))

#Question 4, Weighted historical simulation
i1_historical =which(index(portfolio_returns)==index(portfolio_returns["2008-01-02"]))
i2_historical =which(index(portfolio_returns)==index(portfolio_returns["2009-12-31"]))
weights_p = 0
for (i in 1:1000)
{
  weights_p[1001-i]=(0.995^(i-1))*(1-0.995)/(1-(0.995^1000))
}
VAR <- vector(length=i2_historical-i1_historical+1)
for (i in i1_historical:i2_historical){
  portfolio_returns_wt <- data.frame(ret=portfolio_returns_df$PORTSUM[(i-999):(i)],wei=weights_p)
  portfolio_returns_wt <- portfolio_returns_wt[order(portfolio_returns_wt$ret),]
  sum_wt=0;
  for(j in 1:1000){
    sum_wt = sum_wt + portfolio_returns_wt$wei[j]
    if(sum_wt >= 0.01){
      break;
    }
  }
  VAR[i-i1_historical+1] = abs(portfolio_returns_wt$ret[j])
}
date_var = portfolio_returns_df$DATE[i1_historical:i2_historical]
portfolio_final <- data.frame(date=date_var,VAR=VAR)
lines(portfolio_final$date, portfolio_final$VAR, type = 'l', col='limegreen')
legend(x="topright",y= 95,legend=c("Hist. Sim.","Equally wt. D-N","Exponentially wt. D-N","Wt. Hist. Sim."),
       col=c("black","blue","red","limegreen"),lty = c(1, 1, 1), pch = c(NA, NA, NA))


#Question 5,  Delta-Normal VaR of another portfolio
#(a)
require(derivmkts)
x<- bsopt(s=2647.08, k=2650, v=0.18, r=0.01, tt=0.25, d=0.02)
port_price<-(-5000*90.030536838)-(5000*99.536177770)
port_price
port_delta= (-5000*0.499479993)-(5000*-0.495532488)
port_delta
port_gamma<-(-5000*0.001666649)-(5000*0.001668923)
port_gamma
port_vega<-(-5000*5.253760799)-(5000*5.253760799)
port_vega
port_theta<-(-5000*-0.479488910)-(5000*-0.551389513)
port_theta
#(b)
x1=port_delta*2647.08
varchange= (x1^2)*(0.01^2)
sdchange=sqrt(varchange)
sd_DN=0-1.645*sdchange
var_DN=-sd_DN
var_DN
#(c)
x2=port_vega*18
varchange_IV_SPX=(x1^2)*(0.01^2)+((x2)^2)*(0.03^2)+2*x1*x2*(-.7)*(.01)*(.03)
sdchange_IV_SPX=sqrt(varchange_IV_SPX)
sd_IV_SPX=0-1.645*sdchange_IV_SPX
var_IV_SPX=-sd_IV_SPX
var_IV_SPX
