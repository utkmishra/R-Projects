  #FIN567 HW8
  #Submitted by  - 
  #Aparna Kesarkar
  #Chinmayi Kargal Manjunath
  #Harsh Baheti
  #Utkarsh Mishra
  
  p_def=0.012
  LGD=0.5
  lambda=-log(1-p_def)
  port_loans=0
  port_loans[1:100]=8 #100 small port_loans of 8M USD
  port_loans[101:120]=20 #20 large port_loans of 20M USD
  
  #Question 1.
  #Simulation of losses due to defaults
  simulation=function(port_loans, trials, lambda, LGD, rho){
    n =length(port_loans)
    market_factor=rnorm(trials)
    idiosyncratic_epsilon=matrix(rnorm(trials*n), nrow = n)
    losses=0
    default=matrix(0, trials, n)
    for (i in 1:trials)
    {
      Z=rho^0.5*market_factor[i] + (1 - rho)^0.5*idiosyncratic_epsilon[, i] #One factor Copula Model
      U=pnorm(Z)
      tau=-log(1-U)/lambda
      indicator=ifelse(tau > 1,  0, 1)
      losses[i]=sum(indicator*port_loans*LGD)
      default[i,]=indicator #Defaults in the simulation
    }
    default_corr=cor(default) #Correlation of Defaults
    result=list(losses = losses, default = default_corr)
    return(result)
  }
  trials=200000
  #1A
  rho=0
  distribution_of_losses1A=simulation(port_loans,trials,lambda,LGD,rho)
  hist(distribution_of_losses1A$losses,xlab="Distribution of losses",main="Histogram of Distribution of losses",breaks = 20, freq = FALSE)
  
  #1B
  #Distribution of losses for copula correlation equal to 30%
  rho=0.3
  distribution_of_losses1B=simulation(port_loans,trials,lambda,LGD,rho)
  hist(distribution_of_losses1B$losses,xlab="Distribution of losses", main="Histogram of Distribution of losses",breaks = 100, freq = FALSE)
  
  #1C
  population=2000
  rho=0.3
  simluate_loss=simulation(port_loans,population,lambda,LGD,rho)
  ans<-(simluate_loss$default[lower.tri(simluate_loss$default)])
  est_corr=summary(simluate_loss$default[lower.tri(simluate_loss$default)])
  est_corr
  default_correlation=as.numeric(est_corr[4])
  default_correlation
  
  #1D
  library(pastecs)
  est_dev<-stat.desc(as.numeric(ans))
  std_corr=as.numeric(est_dev[13])
  std_corr
  
  #Question 2
  #2A
  #Calculation of Economic Capital 
  Economic_Capital_2A=quantile(distribution_of_losses1A$losses,0.999)-mean(distribution_of_losses1A$losses)
  Economic_Capital_2A
  ##2B
  Economic_Capital_2B=quantile(distribution_of_losses1B$losses,0.999)-mean(distribution_of_losses1B$losses)
  Economic_Capital_2B
  
  #Question 3
  amt_hedge=40 #40M USD which is insured by the hedge fund
  hedge_ratio=0.5 
  simulation_func=function(loans, no_trials, lambda, LGD, rho){
    n =length(loans)
    market_factor=rnorm(no_trials)
    idiosyncratic_epsilon=matrix(rnorm(no_trials*n), nrow = n)
    losses=0
    default=matrix(0, no_trials, n)
    for (i in 1:no_trials)
    {
      Z=rho^0.5*market_factor[i] + (1 - rho)^0.5*idiosyncratic_epsilon[, i]
      U=pnorm(Z)
      tau=-log(1-U)/lambda
      indicator=ifelse(tau > 1,  0, 1)
      losses[i]=sum(indicator[1:100]*loans[1:100]*LGD)+sum(hedge_ratio*indicator[101:120]*loans[101:120]*LGD)+max(sum((1-hedge_ratio)*indicator[101:120]*loans[101:120]*LGD)-amt_hedge,0)
    }
    indicator
    return(losses)
  }
  
  #3A
  #Estimating the distribution of possible losses due to defaults of the 120 loans considering the hedge"
  trials=200000
  rho=0
  dist_of_losses=simulation_func(port_loans,trials,lambda,LGD,rho)
  hist(dist_of_losses, breaks = 20, freq = FALSE, xlab = "Distribution of Losses", main="Histogram of distribution of losses 3A")
  
  #3B
  #Calculating the expected losses,expected capital on the loan portfolio considering the hedge
  Expected_loss_hedge=mean(dist_of_losses)
  Expected_loss_hedge
  Economic_Capital_hedge=quantile(dist_of_losses,0.999)-Expected_loss_hedge
  Economic_Capital_hedge
  
  #3C
  #Estimating the distribution of possible losses due to defaults of the 120 loans considering the hedge"
  trials=200000
  rho=0.3
  dist_of_loss=simulation_func(port_loans,trials,lambda,LGD,rho)
  hist(dist_of_loss, breaks = 20, freq = FALSE,xlab = "Distribution of Losses", main="Histogram of distribution of losses 3C")
  Expected_losses_new=mean(dist_of_loss)
  Expected_losses_new
  Economic_Capital_new=quantile(dist_of_loss,0.999)-Expected_losses_new
  Economic_Capital_new
  
  #Question 4
  #Simulation of Default losses given a random rate of recovery, that is a random losses given default
  simulation_random_recovery=function(port_loans, trials, lambda, LGD, rho){
    n =length(port_loans)
    market_factor=rnorm(trials)
    idiosyncratic_epsilon=matrix(rnorm(trials*n), nrow = n)
    losses=0
    default=matrix(0, trials, n)
    aux=1
    for (i in 1:trials)
    {
      
      Z=rho^0.5*market_factor[i] + (1 - rho)^0.5*idiosyncratic_epsilon[, i]
      U=pnorm(Z)
      tau=-log(1-U)/lambda
      indicator=ifelse(tau > 1,  0, 1)
      losses[i]=sum(indicator*port_loans*LGD[aux:aux*n])
      aux=aux+1
    }
    indicator
    return(losses)
  }
  
  #4A
  #Compute losses due to default when random recovery
  trials=100000
  rho=0
  LGD=1-rbeta(trials*length(port_loans),2,2)
  distribution_of_losses4A=simulation_random_recovery(port_loans,trials,lambda,LGD,rho)
  hist(distribution_of_losses4A,breaks = 40,freq = FALSE)
  
  #4B
  trials=100000
  rho=0.3
  LGD=1-rbeta(trials*length(port_loans),2,2)
  distribution_of_losses4B=simulation_random_recovery(port_loans,trials,lambda,LGD,rho)
  hist(distribution_of_losses4B,breaks = 40,freq = FALSE)
