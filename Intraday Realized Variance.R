data <- read.csv("G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW7/Google_Data.csv",header = TRUE)

#Q1
returns_per_minute <- na.omit(diff(log(data$close)))
rv_6th_1min <- 0
for (i in 1:390){
  rv_6th_1min <- rv_6th_1min + (returns_per_minute[i]^2)
}
rv_1min <- c()
for(j in 1:9){
    rv_1min[j] <- sum(returns_per_minute[((j*391)+1):(((j+1)*391)-1)]^2)
}
rv_1min <- c(rv_6th_1min,rv_1min)
rv_avg_1min <- 0
for (i in 1:10){
  rv_avg_1min<-rv_avg_1min+rv_1min[i]
}
rv_avg_1min<- rv_avg_1min/10
rv_1min<- c(rv_1min,rv_avg_1min)

#Q2
#2 min return
ret_per_2minutes <- c()
for (i in 1:3908){
  ret_per_2minutes[i]<- (log(data$close[i+2])-log(data$close[i]))
}
rv_6th_2min <- 0
for (i in seq(1,389,2)){
  rv_6th_2min <- rv_6th_2min+(ret_per_2minutes[i]^2)
}
rv_2min <- c()
for (j in 1:9){
  rv_2min[j] <- sum(ret_per_2minutes[seq(((j*391)+1),(((j+1)*391)-2),2)]^2)
  }
rv_2min <- c(rv_6th_2min,rv_2min)
rv_avg_2min <- 0
for (i in 1:10){
  rv_avg_2min<-rv_avg_2min+rv_2min[i]
}
rv_avg_2min<- rv_avg_2min/10
rv_2min<-c(rv_2min,rv_avg_2min)

#5 min return
ret_per_5minutes <- c()
for (i in 1:3905){
  ret_per_5minutes[i]<- (log(data$close[i+5])-log(data$close[i]))
}
rv_6th_5min <- 0
for (i in seq(1,386,5)){
  rv_6th_5min <- rv_6th_5min+(ret_per_5minutes[i]^2)
}
rv_5min <- c()
for (j in 1:9){
    rv_5min[j]<-sum(ret_per_5minutes[seq(((j*391)+1),(((j+1)*391)-5),5)]^2)
  }
rv_5min <- c(rv_6th_5min,rv_5min)
rv_avg_5min <- 0
for (i in 1:10){
  rv_avg_5min<-rv_avg_5min+rv_5min[i]
}
rv_avg_5min<- rv_avg_5min/10
rv_5min<-c(rv_5min,rv_avg_5min)

#10 min return
ret_per_10minutes <- c()
for (i in 1:3900){
  ret_per_10minutes[i]<- (log(data$close[i+10])-log(data$close[i]))
}
rv_6th_10min <- 0
for (i in seq(1,381,10)){
  rv_6th_10min <- rv_6th_10min+(ret_per_10minutes[i]^2)
}
rv_10min <- 0
for (j in 1:9){
    rv_10min[j] <- sum(ret_per_10minutes[seq(((j*391)+1),(((j+1)*391)-10),10)]^2)
  }
rv_10min<- c(rv_6th_10min,rv_10min)
rv_avg_10min <- 0
for (i in 1:10){
  rv_avg_10min<-rv_avg_10min+rv_10min[i]
}
rv_avg_10min<- rv_avg_10min/10
rv_10min<-c(rv_10min,rv_avg_10min)

#15 min return
ret_per_15minutes <- c()
for (i in 1:3895){
  ret_per_15minutes[i]<- (log(data$close[i+15])-log(data$close[i]))
}
rv_6th_15min <- 0
for (i in seq(1,376,15)){
  rv_6th_15min <- rv_6th_15min+(ret_per_15minutes[i]^2)
}
rv_15min <- 0
for (j in 1:9){
    rv_15min[j] <- sum(ret_per_15minutes[seq(((j*391)+1),(((j+1)*391)-15),15)]^2)
  }
rv_15min <- c(rv_6th_15min,rv_15min)
rv_avg_15min <- 0
for (i in 1:10){
  rv_avg_15min<-rv_avg_15min+rv_15min[i]
}
rv_avg_15min<- rv_avg_15min/10
rv_15min<-c(rv_15min,rv_avg_15min)

#Q2 (a)
date <- c('06-02-2013','07-02-2013','08-02-2013','11-02-2013','12-02-2013','13-02-2013','14-02-2013','15-02-2013','19-02-2013','20-02-2013','Averave RV')
#rv_table <- table(date,rv_1min,rv_2min,rv_5min,rv_10min,rv_15min)
rv_df <- data.frame(date,rv_1min,rv_2min,rv_5min,rv_10min,rv_15min)

#Q2 (b)
adj_1min = 1
adj_2min = (1+1*195/194)/2
adj_5min = (1+4*78/77)/5
adj_10min = (1+9*39/38)/10
adj_15min = (1+14*26/25)/15
rv_1min_new<-adj_1min*rv_1min[1:10]
rv_2min_new<-adj_2min*rv_2min[1:10]
rv_5min_new<-adj_5min*rv_5min[1:10]
rv_10min_new<-adj_10min*rv_10min[1:10]
rv_15min_new<-adj_15min*rv_15min[1:10]
rv_avg_1min_new<-mean(rv_1min_new)
rv_avg_2min_new<-mean(rv_2min_new)
rv_avg_5min_new<-mean(rv_5min_new)
rv_avg_10min_new<-mean(rv_10min_new)
rv_avg_15min_new<-mean(rv_15min_new)

#Q2 (c) - Estimates of realized variance decrease as return interval becomes longer
rv_avg_15to1<- rv_avg_15min_new/rv_avg_1min_new
rv_avg_15to10 <- rv_avg_15min_new/rv_avg_10min_new

#Q3
GE_data <- read.csv("G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW7/GE_data.csv",header = TRUE)

#1 min return
GE_returns_per_minute <- na.omit(diff(log(GE_data$close)))
GE_rv_6th_1min <- 0
for (i in 1:390){
  GE_rv_6th_1min <- GE_rv_6th_1min + (GE_returns_per_minute[i]^2)
}
GE_rv_1min <- c()
for(j in 1:9){
  GE_rv_1min[j] <- sum(GE_returns_per_minute[((j*391)+1):(((j+1)*391)-1)]^2)
}
GE_rv_1min <- c(GE_rv_6th_1min,GE_rv_1min)
GE_rv_avg_1min <- 0
for (i in 1:10){
  GE_rv_avg_1min<-GE_rv_avg_1min+GE_rv_1min[i]
}
GE_rv_avg_1min<- GE_rv_avg_1min/10
GE_rv_1min<- c(GE_rv_1min,GE_rv_avg_1min)

#2 min return
GE_ret_per_2minutes <- c()
for (i in 1:3908){
  GE_ret_per_2minutes[i]<- (log(GE_data$close[i+2])-log(GE_data$close[i]))
}
GE_rv_6th_2min <- 0
for (i in seq(1,389,2)){
  GE_rv_6th_2min <- GE_rv_6th_2min+(GE_ret_per_2minutes[i]^2)
}
GE_rv_2min <- c()
for (j in 1:9){
  GE_rv_2min[j] <- sum(GE_ret_per_2minutes[seq(((j*391)+1),(((j+1)*391)-2),2)]^2)
}
GE_rv_2min <- c(GE_rv_6th_2min,GE_rv_2min)
GE_rv_avg_2min <- 0
for (i in 1:10){
  GE_rv_avg_2min<-GE_rv_avg_2min+GE_rv_2min[i]
}
GE_rv_avg_2min<- GE_rv_avg_2min/10
GE_rv_2min<-c(GE_rv_2min,GE_rv_avg_2min)

#5 min return
GE_ret_per_5minutes <- c()
for (i in 1:3905){
  GE_ret_per_5minutes[i]<- (log(GE_data$close[i+5])-log(GE_data$close[i]))
}
GE_rv_6th_5min <- 0
for (i in seq(1,386,5)){
  GE_rv_6th_5min <- GE_rv_6th_5min+(GE_ret_per_5minutes[i]^2)
}
GE_rv_5min <- c()
for (j in 1:9){
  GE_rv_5min[j]<-sum(GE_ret_per_5minutes[seq(((j*391)+1),(((j+1)*391)-5),5)]^2)
}
GE_rv_5min <- c(GE_rv_6th_5min,GE_rv_5min)
GE_rv_avg_5min <- 0
for (i in 1:10){
  GE_rv_avg_5min<-GE_rv_avg_5min+GE_rv_5min[i]
}
GE_rv_avg_5min<- GE_rv_avg_5min/10
GE_rv_5min<-c(GE_rv_5min,GE_rv_avg_5min)

#10 min return
GE_ret_per_10minutes <- c()
for (i in 1:3900){
  GE_ret_per_10minutes[i]<- (log(GE_data$close[i+10])-log(GE_data$close[i]))
}
GE_rv_6th_10min <- 0
for (i in seq(1,381,10)){
  GE_rv_6th_10min <- GE_rv_6th_10min+(GE_ret_per_10minutes[i]^2)
}
GE_rv_10min <- 0
for (j in 1:9){
  GE_rv_10min[j] <- sum(GE_ret_per_10minutes[seq(((j*391)+1),(((j+1)*391)-10),10)]^2)
}
GE_rv_10min<- c(GE_rv_6th_10min,GE_rv_10min)
GE_rv_avg_10min <- 0
for (i in 1:10){
  GE_rv_avg_10min<-GE_rv_avg_10min+GE_rv_10min[i]
}
GE_rv_avg_10min<- GE_rv_avg_10min/10
GE_rv_10min<-c(GE_rv_10min,GE_rv_avg_10min)

#15 min return
GE_ret_per_15minutes <- c()
for (i in 1:3895){
  GE_ret_per_15minutes[i]<- (log(GE_data$close[i+15])-log(GE_data$close[i]))
}
GE_rv_6th_15min <- 0
for (i in seq(1,376,15)){
  GE_rv_6th_15min <- GE_rv_6th_15min+(GE_ret_per_15minutes[i]^2)
}
GE_rv_15min <- 0
for (j in 1:9){
  GE_rv_15min[j] <- sum(GE_ret_per_15minutes[seq(((j*391)+1),(((j+1)*391)-15),15)]^2)
}
GE_rv_15min <- c(GE_rv_6th_15min,GE_rv_15min)
GE_rv_avg_15min <- 0
for (i in 1:10){
  GE_rv_avg_15min<-GE_rv_avg_15min+GE_rv_15min[i]
}
GE_rv_avg_15min<- GE_rv_avg_15min/10
GE_rv_15min<-c(GE_rv_15min,GE_rv_avg_15min)

#Q3 (a)
GE_date <- c('06-02-2013','07-02-2013','08-02-2013','11-02-2013','12-02-2013','13-02-2013','14-02-2013','15-02-2013','19-02-2013','20-02-2013','Averave RV')
#rv_table <- table(date,rv_1min,rv_2min,rv_5min,rv_10min,rv_15min)
GE_rv_df <- data.frame(GE_date,GE_rv_1min,GE_rv_2min,GE_rv_5min,GE_rv_10min,GE_rv_15min)

GE_rv_1min_new<-adj_1min*GE_rv_1min[1:10]
GE_rv_2min_new<-adj_2min*GE_rv_2min[1:10]
GE_rv_5min_new<-adj_5min*GE_rv_5min[1:10]
GE_rv_10min_new<-adj_10min*GE_rv_10min[1:10]
GE_rv_15min_new<-adj_15min*GE_rv_15min[1:10]
GE_rv_avg_1min_new<-mean(GE_rv_1min_new)
GE_rv_avg_2min_new<-mean(GE_rv_2min_new)
GE_rv_avg_5min_new<-mean(GE_rv_5min_new)
GE_rv_avg_10min_new<-mean(GE_rv_10min_new)
GE_rv_avg_15min_new<-mean(GE_rv_15min_new)

#Q3 (b) - Estimates of realized variance decrease as return interval becomes longer
GE_rv_avg_15to1<- GE_rv_avg_15min_new/GE_rv_avg_1min_new
GE_rv_avg_15to10 <- GE_rv_avg_15min_new/GE_rv_avg_10min_new

#Q4
Google_new_data <- read.csv("G:/Study/UIUC/Second Sem/FIN 567 Financial Risk Management/HW7/Google_Data_new.csv",header = TRUE)

#First Method
ret_6_5<-log(Google_new_data$close[392])-log(Google_new_data$close[1])
ctc_ret<- c()
for (i in 1:9){
  ctc_ret[i]<-log(Google_new_data$close[((i+1)*391)+1])-log(Google_new_data$close[(i*391)+1])
}
ctc_ret<-c(ret_6_5,ctc_ret)
sum_square_ctc<-sum(ctc_ret^2)
ratio = sum_square_ctc/rv_avg_15min_new
scaled_rv<-ratio*rv_15min_new

#Second Method
open_ret_6_5<-log(Google_new_data$close[2])-log(Google_new_data$close[1])
cto_ret<-c()
for (i in 1:9){
  cto_ret[i]<-log(Google_new_data$close[(i*391)+2])-log(Google_new_data$close[(i*391)+1])
}
cto_ret<-c(open_ret_6_5,cto_ret)
scaled_rv_2<-(cto_ret^2)+rv_15min_new
