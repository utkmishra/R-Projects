library(xts)
library(Quandl)
library(quantmod)
library(zoo)
library(PerformanceAnalytics)
library(fOptions)
library(MASS)
library(plyr)

#Question 1
data <- read.csv(file = "G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW5/HW5.data.csv",header = TRUE,sep = ",")
returns <- na.omit(diff(log(data$Close)))
losses <- na.omit(abs(returns[returns<0]))
u=0.01
for (i in 2:21){
  u[i] <- u[i-1]+0.002
}
eu = 0
for (j in 1:21){
  eu[j] <- mean(losses[losses>=u[j]]-u[j])
}
mean_excess_function <- data.frame(u, eu)
plot(mean_excess_function$u,mean_excess_function$eu, type = 'l',xlab = "Threshold u",
     ylab="Mean Excess Loss",col="black",main="Mean Excess function")

#Question 2

loss_beyond_threshold = na.omit(losses[losses>=0.022])
length(loss_beyond_threshold)

#Question 3
excess_losses <- loss_beyond_threshold - 0.022
log_likelihood <- function(a){
  alpha=a[1]
  beta=a[2]
  if(alpha<0 || beta<0 || alpha+beta>=1){
    log_density_sum=9999
  } else {
    log_density= log(1/beta)-(1+1/alpha)*log(1+(alpha/beta)*excess_losses)
    log_density_sum = -sum(log_density)
  }
  return(log_density_sum)
}

par<- c(0.4, 0.6)
MLE= optim(par, log_likelihood)

#Question 4
alpha1 <- MLE$par[1]
beta1 <- MLE$par[2]
X_range <- losses[losses>=0.022&losses<=0.1]
cond_density <- (1/beta1)*(1+((alpha1*(X_range-0.022))/beta1))^(-(1+1/alpha1))
cond_density_df <- data.frame(X_range,cond_density)
plot(cond_density_df$X_range,cond_density_df$cond_density,xlab="Losses", ylab="Conditional Density",main="Conditional Density Function")

#Question 5
z<-seq(0.022,0.10,0.002)
Prob<- (length(loss_beyond_threshold)/length(returns))*(1+((alpha1*(z-0.022))/beta1))^(-1/alpha1)
Prob_df <- data.frame(z,Prob)
plot(Prob_df$z,Prob_df$Prob,type='l',xlab="Threshold u",ylab="Probability",main="Probabilities of Losses")
Prob_df[Prob_df$z==0.05,]['Prob']
Prob_df[Prob_df$z==0.022,]['Prob']
Prob_df[Prob_df$z==0.10,]['Prob']
