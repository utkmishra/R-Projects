library(MASS)

#question 1 A
d2i = qnorm(0.01) #qnorm is the inverse of pnorm

#B
cor1 = matrix(0.3,100,100)
diag(cor1) = 1
losses = 0

for(i in 1:10000){
  mu = rep(0, 100)
  zi = mvrnorm(1,mu=mu,cor1)
  A = rep(0,100)
  A[which(zi<d2i)] = 10000000*rbeta(1,2,2)
  losses[i] = sum(A)
}

hist(losses, xlab = "loss for rho=0.3")

#C
cor2 = matrix(0.4,100,100)
diag(cor2) = 1
losses1 = 0
for(i in 1:10000){
  mu = vector("numeric", 100)
  zi = mvrnorm(1,mu,cor2)
  B = rep(0,100)
  B[which(zi<d2i)] = 10000000*rbeta(1,2,2)
  losses1[i] = sum(B)
}
hist(losses1,xlab = "loss for rho=0.4")

#question 2 (a)
m_a_0=-qnorm(0.03/2)

#Q2 (b)
dtd <- function(x){
  pnorm(x+0.05) - exp(-0.1*x) * pnorm(-x+0.05) - 0.97
}
m_b_0 <- uniroot(dtd, c(0,3))$root

#Q2 (c)
rho = 0.3
dt = 1/300
time_step = seq(0,1, dt)
n = length(time_step) - 1
def = c()
for(i in 1: 10000){
  rv_a = rnorm(n)
  rv_b = rv_a*rho + sqrt(1-rho^2)*rnorm(n)
  m_a = m_a_0
  m_b = m_b_0
  for(j in 2 : n){
    m_a[j] = m_a[j-1] + sqrt(dt) * rv_a[j -1]
    m_b[j] = m_b[j-1] + 0.05*dt + sqrt(dt)*rv_b[j-1]
  }
  if((min(m_a) <= 0 )&(min(m_b) <=0)){def[i] = 1}
  else{def[i] = 0}
}
def_rate = sum(def)/10000

#Q2 (d)
def_new = c()
for(i in 1: 10000){
  rv_a = rnorm(n)
  rv_b = rv_a*rho + sqrt(1-rho^2)*rnorm(n)
  m_a = m_a_0
  m_b = m_b_0
  for(j in 2 : n){
    m_a[j] = m_a[j-1] + sqrt(dt) * rv_a[j -1]
    m_b[j] = m_b[j-1] + sqrt(dt)*rv_b[j-1]
  }
  if((min(m_a) <= 0 )&(min(m_b) <=0)){def_new[i] = 1}
  else{def_new[i] = 0}
}
def_rate_new = sum(def_new)/10000

#question 3
rho_q3=0.3
dt = 1/80
time_step = seq(0,1,dt)
n = length(time_step) - 1
tot_losses_q3 = c()
for(i in 1:1500){
  r_var = rnorm(n)
  zi = sqrt(rho_q3)*r_var+ sqrt(1-rho_q3)*matrix(rnorm(n*100),n,100)
  m = default = losses_q3 = matrix(0,n,100)
  m[1,] = 2.17
  for(j in 2: n){
    for(k in 1:100){
      m[j,k] = m[j-1,k] + sqrt(dt)*zi[j-1,k]
      default[j,k] = ifelse((m[j,k] <= 0),1,0)
      losses_q3[j, k] = ifelse((m[j,k] <= 0), (1-rbeta(1,2,2))*10000000, 0)
    }
  }
  tot_losses_q3[i] = sum(losses_q3)
}
hist(tot_losses_q3, breaks =60 , main = "Possible next year Losses")

