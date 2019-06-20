library(xts)
library(Quandl)
library(quantmod)
library(zoo)
library(PerformanceAnalytics)
library(fOptions)
library(MASS)
library(plyr)

#Question 1
data <- read.csv(file = "G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW6/HW5.data.csv",header = TRUE,sep = ",")
returns <- na.omit(diff(log(data$Close)))

#(a) Taken from FIN 567 Midterm'19
fr1 <- function(x) {  
  sigmasqhat = rep(0,length(returns))
  sigmasqhat[1] = x[4]^2 
  
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001||  x[4]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(returns)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*returns[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*returns^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess_1 <- c(0.1,0.8,0.01,0.01)
output1 <- optim(guess_1,fr1)
alpha <- output1$par[1]
beta <- output1$par[2]
sigma <- output1$par[3]
sigma1 <- output1$par[4]

#(b)
sigma_lr <- sqrt(sum(returns^2)/length(returns))
fr2 <- function(x) {  
  sigmasqhat = rep(0,length(returns))
  sigmasqhat[1] = x[3]^2 
  
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(returns)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*sigma_lr^2+x[1]*returns[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*returns^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess2 <- c(0.1,0.8,0.01)
output2 <- optim(guess2,fr2)
output2

#(c)
fr3 <- function(x) {  
  sigmasqhat = rep(0,length(returns))
  sigmasqhat[1] = (sum(returns^2)/length(returns))
  
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0 || x[3]<0.001){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(returns)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*x[3]^2+x[1]*returns[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*returns^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess3 <- c(0.1,0.8,0.04)
output3 <- optim(guess3,fr3)
output3

#(d)
fr4 <- function(x) {  
  sigmasqhat = rep(0,length(returns))
  sigmasqhat[1] = (sum(returns^2)/length(returns))
  
  if (x[1]+x[2]>=1 || x[1]<0 || x[2]<0){
    NeglogLH = 9999
  } else {
    for (i in 1:(length(returns)-1)) {
      sigmasqhat[i+1] = (1-x[1]-x[2])*sigma_lr^2+x[1]*returns[i]^2+x[2]*sigmasqhat[i]
    }
    f <-(1/(sqrt(2*pi*sigmasqhat)))*exp(-0.5*returns^2/sigmasqhat)
    NeglogLH = -sum(log(f))
  }
  return(NeglogLH)
}
guess4 <- c(0.1,0.8)
output4 <- optim(guess4,fr4)
output4

#(e)
sigmasqhat_a = rep(0,length(returns)+1)
sigmasqhat_a[1] = sigma1^2 

for (i in 1:(length(returns)-1)) {
  sigmasqhat_a[i+1]=(1-alpha-beta)*sigma^2+alpha*returns[i]^2+beta*sigmasqhat_a[i]
}
sqrt(sigmasqhat_a[length(sigmasqhat_a)-1])

sigmasqhat_b = rep(0,length(returns)+1)
sigmasqhat_b[1] = output2$par[3]^2 

for (i in 1:(length(returns)-1)) {
  sigmasqhat_b[i+1]=(1-output2$par[1]-output2$par[2])*sigma_lr^2+output2$par[1]*returns[i]^2+output2$par[2]*sigmasqhat_b[i]
}
sqrt(sigmasqhat_b[length(sigmasqhat_b)-1])

sigmasqhat_c = rep(0,length(returns)+1)
sigmasqhat_c[1] = (sum(returns^2)/length(returns)) 

for (i in 1:(length(returns)-1)) {
  sigmasqhat_c[i+1]=(1-output3$par[1]-output3$par[2])*output3$par[3]^2+output3$par[1]*returns[i]^2+output3$par[2]*sigmasqhat_c[i]
}
sqrt(sigmasqhat_c[length(sigmasqhat_c)-1])

sigmasqhat_d = rep(0,length(returns)+1)
sigmasqhat_d[1] = (sum(returns^2)/length(returns)) 

for (i in 1:(length(returns)-1)) {
  sigmasqhat_d[i+1]=(1-output4$par[1]-output4$par[2])*sigma_lr^2+output4$par[1]*returns[i]^2+output4$par[2]*sigmasqhat_d[i]
}
sqrt(sigmasqhat_d[length(sigmasqhat_c)-1])

#Question 2
#(a)
A <- rep(0,21)
A[1]<-(alpha*(returns[length(returns)]^2-sigma^2)+ beta*(sigmasqhat_a[1000]-sigma^2))

for (i in 1:20) {
  A[i+1]= (alpha+beta)*A[i]
}

var_est_1 = A + sigma^2
realized_var_1 <- sum(var_est_1)

B <- rep(0,21)
B[1]<-output2$par[1]*(returns[length(returns)]^2-sigma_lr^2)+ output2$par[2]*(sigmasqhat_a[1000]-sigma_lr^2)

for (i in 1:20) {
  B[i+1]= (output2$par[1]+output2$par[2])*B[i]
}

var_est_2 = B + sigma_lr^2
realized_var_2 <- sum(var_est_2)

C <- rep(0,21)
C[1]<-output3$par[1]*(returns[length(returns)]^2-output3$par[3]^2)+ output3$par[2]*(sigmasqhat_a[1000]-output3$par[3]^2)

for (i in 1:20) {
  C[i+1]= (output3$par[1]+output3$par[2])*C[i]
}

var_est_3 = C + output3$par[3]^2
realized_var_3 <- sum(var_est_3)

D <- rep(0,21)
D[1]<-output4$par[1]*(returns[length(returns)]^2-sigma_lr^2)+ output4$par[2]*(sigmasqhat_a[1000]-sigma_lr^2)

for (i in 1:20) {
  D[i+1]= (output4$par[1]+output4$par[2])*D[i]
}

var_est_4 = D + sigma_lr^2
realized_var_4 <- sum(var_est_4)

#(b)
annualized_vol_1 <- sqrt(realized_var_1*(252/21))
annualized_vol_2 <- sqrt(realized_var_2*(252/21))
annualized_vol_3 <- sqrt(realized_var_3*(252/21))
annualized_vol_4 <- sqrt(realized_var_4*(252/21))
