
###############Initialisation 
rm(list=objects())
###############packages
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
library(magrittr)
library(dygraphs)
library(xts)
library(tidyverse)
library(viking)
###############packages

###############Import des donnees
setwd("/Users/bigmac/Desktop/MDA woohoo/projet ML prévision/")
#C:\Enseignement\2015-2016\Projet Data Mining\TP\Regression
#data0<-read.table("datahebdo0.txt", header=TRUE)
data1<-read.table("datahebdo1.txt", header=TRUE)
data_all<-read_csv("/Users/bigmac/Desktop/MDA woohoo/projet ML prévision/optiver-trading-at-the-close/train.csv", col_names =TRUE)
data0_bis <- data_all[data_all$stock_id == 1, ]




###############evaluation criteria
rmse<-function(eps)
{
  return(round(sqrt(mean(eps^2,na.rm=TRUE)),digits=0))
}

mape<-function(y,ychap)
{
  return(sum(abs(y-ychap))/length(y))
}




generate_features <- function(df) {
  features <- c('seconds_in_bucket', 'imbalance_buy_sell_flag',
                'imbalance_size', 'matched_size', 'bid_size', 'ask_size',
                'reference_price', 'far_price', 'near_price', 'ask_price', 'bid_price', 'wap',
                'imb_s1', 'imb_s2', 'time_id', 'date_id', 'target'
  )
  
  df$imb_s1 <- (df$bid_size - df$ask_size) / (df$bid_size + df$ask_size)
  df$imb_s2 <- (df$imbalance_size - df$matched_size) / (df$matched_size + df$imbalance_size)
  
  prices <- c('reference_price', 'far_price', 'near_price', 'ask_price', 'bid_price', 'wap')
  
  for (i in 1:length(prices)) {
    for (j in 1:length(prices)) {
      if (i > j) {
        df[[paste0(prices[i], "_", prices[j], "_imb")]] <- (df[[prices[i]]] - df[[prices[j]]]) / (df[[prices[i]]] + df[[prices[j]]])
        features <- c(features, paste0(prices[i], "_", prices[j], "_imb"))
      }
    }
  }
  
  for (i in 1:length(prices)) {
    for (j in 1:length(prices)) {
      for (k in 1:length(prices)) {
        if (i > j && j > k) {
          max_ <- pmax(df[[prices[i]]], df[[prices[j]]], df[[prices[k]]])
          min_ <- pmin(df[[prices[i]]], df[[prices[j]]], df[[prices[k]]])
          mid_ <- rowSums(df[, c(prices[i], prices[j], prices[k])] - min_ - max_)
          
          df[[paste0(prices[i], "_", prices[j], "_", prices[k], "_imb2")]] <- (max_ - mid_) / (mid_ - min_)
          features <- c(features, paste0(prices[i], "_", prices[j], "_", prices[k], "_imb2"))
        }
      }
    }
  }
  
  return(df[, features, with = FALSE])
}

data0_bis <- data_all[data_all$stock_id == 1, ]

data0_bis <- generate_features(data0_bis)



n_train <- 100*55
n_test <- 20*55
data0 <- data0_bis[1:n_train, ]
data_test <- data0_bis[n_train+1:n_train+n_test, ]

hist(data0$target)


plot_n_days <- function(n_days, n_length, df_plot, var, type_='p') {
  par(mfrow=c(3,as.integer(n_length/3)))
  for (i in 1:n_length){
    df_plot_batch <- df_plot[((i-1)*(55*n_days)+1):(i*(55*n_days)+1), ]
    plot(df_plot_batch[[var]], df_plot_batch$target,pch=16,cex=0.5
         , type=type_, main=paste(c("target ~", var, "; batch :", as.character(i))))
  }
  
  par(mfrow=c(1,1))
  
}
plot_n_days(10, 3, data0, "time_id", 'l')
plot_n_days(10, 3, data0, "date_id")
plot_n_days(10, 3, data0, "wap")
plot_n_days(10, 3, data0, "imbalance_size")
plot_n_days(10, 3, data0, "reference_price")
plot_n_days(10, 3, data0, "ask_price")
plot_n_days(10, 3, data0, "bid_price")
plot_n_days(10, 3, data0, "ask_size")
plot_n_days(10, 3, data0, "bid_size")
plot_n_days(10, 3, data0, "matched_size")
plot_n_days(20, 3, data0, "seconds_in_bucket")
plot_n_days(1, 3, data0, "seconds_in_bucket")
plot_n_days(1, 3, data0, "seconds_in_bucket")

####################################################
##############qq graphiques pour l'analyse descriptive
####################################################
attach(data0)
summary(data0)
plot(target,type='l')
plot(imbalance_size, target,pch=16,cex=0.5)
plot(wap, target,pch=16,cex=0.5)
plot(reference_price, target,pch=16,cex=0.5)


####################################################
##############exercice 1: choice of k
####################################################
plot(imb_s1, target,pch=16,cex=0.5)


#############################################

equation <- target~s(time_id,k=3)+s(date_id,k=30, bs='cc')+te(time_id, wap, k=c(3, 5))+s(reference_price, k=10)+
  s(matched_size)+s(ask_size)+s(imb_s1)+s(wap)

gam_test <- gam(equation, data=data0, select=TRUE)
summary(gam_test)
gam_test$gcv.ubre%>%sqrt

#######################################################################################################################################
#############################################ANOVA SELECTION
#######################################################################################################################################
equation1 <- target~s(time_id)+s(date_id)+s(wap)+s(reference_price)+matched_size+s(ask_size)
equation2 <- target~s(date_id)+ti(time_id, k=3)+ti(wap, k=10)+ti(time_id,wap, k=c(3,10))+s(reference_price)+matched_size+s(ask_size)

gam1<-gam(equation1, data=data0)
gam2<-gam(equation2, data=data0)
gam1$gcv.ubre%>%sqrt
gam2$gcv.ubre%>%sqrt

summary(gam1)
summary(gam2)
anova(gam1, gam2, test = "Chisq") ####p value <0.05 interaction is significant


#######################################################################################################
########shrinkage , "cs"
#####################################################################################################
equation <- target~s(date_id)+ti(time_id)+ti(wap)+s(reference_price)+s(matched_size, bs='cs')+s(ask_size, k=20, bs='cs')

gam_cs<-gam(equation, data=data0, select=TRUE)
gam_cs$gcv.ubre%>%sqrt

summary(gam_cs) ####doesn't shrink do to single lambda



#######################################################################################################
########shrinkage , "select"
#####################################################################################################

equation <- target~s(date_id)+ti(time_id, k=3)+ti(wap, k=10)+te(time_id,wap, k=c(3,10))+s(reference_price)+s(matched_size)+s(time_id,ask_size)

gam_select<-gam(equation, data=data0, select=TRUE)
gam_select$gcv.ubre%>%sqrt
summary(gam_select) 


##################################################################
######online learning
##################################################################
Data <-rbind(data0, data1)
# Data$time_id <- as.numeric(Data$Date)
# 
sel_a <- which(Data$Year<=max(data0$Year))
sel_b <- which(Data$Year>max(data0$Year))


#equation <- target~s(time_id,k=3)+s(date_id,k=30, bs='cc')+te(time_id, wap, k=c(3, 5))+s(reference_price, k=10)+matched_size+s(ask_size)
equation <- target~s(time_id,k=3)+s(date_id,k=30, bs='cc')+s(wap)+s(reference_price)+matched_size+s(ask_size)

gam<-gam(equation, data=data0)
summary(gam)
gam$gcv.ubre%>%sqrt
sum(gam$edf)
gam.forecast <- predict(gam, newdata=data1)

X <- predict(gam, newdata=Data, type='terms')
###scaling columns
for (j in 1:ncol(X))
  X[,j] <- (X[,j]-mean(X[,j])) / sd(X[,j])
X <- cbind(X,1)
d <- ncol(X)

y <- Data$target

# static 
ssm <- viking::statespace(X, y)
ssm
gam.kalman.static <- ssm$pred_mean%>%tail(nrow(data1))

rmse2(y=data1$target, ychap=gam.forecast)
rmse2(y=data1$target, ychap=gam.kalman.static)



# dynamic
# using iterative grid search
ssm_dyn <- viking::select_Kalman_variances(ssm, X[sel_a, ], y[sel_a], q_list = 2^(-30:0), p1 = 1, 
                                           ncores = 6)
ssm_dyn <- predict(ssm_dyn, X, y, type='model', compute_smooth = TRUE)
gam.kalman.Dyn <- ssm_dyn$pred_mean%>%tail(nrow(data1))
rmse2(y=data1$target, ychap=gam.kalman.Dyn)
plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date)

plot(ssm_dyn, pause=F, window_size = 14, date = Data$Date, sel = sel_b)
ssm_dyn$kalman_params$Q

# using expectation-maximization
ssm_em <- viking::select_Kalman_variances(ssm, X[sel_a,], y[sel_a], method = 'em', n_iter = 1000,
                                          Q_init = diag(d), verbose = 10, mode_diag = T)
ssm_em <- predict(ssm_em, X, y, type='model', compute_smooth = TRUE)
#ssm_em <-readRDS("Results/ssm_em.RDS")

gam.kalman.Dyn.em <- ssm_em$pred_mean%>%tail(nrow(data1))

ssm_em$kalman_params$Q

plot(ssm_em, pause=F, window_size = 14, date = Data$Date, sel = sel_b)
rmse2(y=data1$target, ychap=gam.kalman.Dyn.em)

plot(data1$target, type='l')
lines(gam.forecast, col='red')
lines(gam.kalman.static, col='blue')
lines(gam.kalman.Dyn, col='green')
lines(gam.kalman.Dyn.em, col='purple')


plot(cumsum(data1$target-gam.forecast), type='l', col='red')
lines(cumsum(data1$target-gam.kalman.static), col='blue')
lines(cumsum(data1$target-gam.kalman.Dyn), col='green')
lines(cumsum(data1$target-gam.kalman.Dyn.em), col='purple')

mape(data1$target,gam.forecast)
mape(data1$target,gam.kalman.static)
mape(data1$target,gam.kalman.Dyn)
mape(data1$target,gam.kalman.Dyn.em)

rmse2(data1$target,gam.forecast)
rmse2(data1$target,gam.kalman.static)
rmse2(data1$target,gam.kalman.Dyn)
rmse2(data1$target,gam.kalman.Dyn.em)

