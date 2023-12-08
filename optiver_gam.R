########################################################################################################
#objectif: selectionner les variables et construire un modele GAM realisant la meilleur prevision
#critere d'evaluation de la prevision: RMSE et MAPE


###########description des donnees
##########donnees de conso. francaise moyenne hebdomadaire de 1996 a 2009
##########data0: de 1996 a 2008, donnees d'apprentissage
##########data1: 2009, donnees test
#nom des variables:
#seconds_in_bucket: numero d'observation
#"Day"     "Month"   "Year": la date
#time_id:  numero de la semaine dans l'annee /52 (va de 0 a 1 tous les ans)
#target: la consommation en MW
#ask_size : la consommation en MW de la semaine precedente (lag 1)
#wap: la waperature moyenne hebdo. en ?C
#bid_size: la waperature en ?C  de la semaine precedente (lag 1)
#imbalance_size: Indice de production industriel (source INSEE)
#reference_price: Indice de production industriel corrige des variations saisonnieres


###############Initialisation 
rm(list=objects())
###############packages
library(mgcv)
library(mgcViz)
library(gridExtra)
library(yarrr)
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
    return(round(100*mean(abs(y-ychap)/abs(y)),digits=2))
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
g0 <- gam(target~s(imb_s1, k=5, bs="cr"), data=data0)
summary(g0)
plot(g0)
plot(data0$wap, g0$residuals, pch=16)

g_res_1 <- gam(g0$residuals~ s(data0$wap, k=5, bs="cr"))
summary(g_res_1)

# 
# fit0 <- getViz(g0, nsim = 50)
# plot(sm(fit0, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()
# ( check1D(fit0, "wap") + l_gridCheck1D(gridFun = mean, stand = "sc", n=100) )$ggObj


g1 <- gam(target~s(imb_s1, k=25, bs="cr"), data=data0)
summary(g1)
plot(g1)
block = (length(data0$target)-1000):length(data0$target)
forecast<-predict(g1, newdata=data0[block,])
eps_vect <- (data0[block,]$target-forecast)^2
plot(data0[block,]$wap, eps_vect)
eps_vect
sqrt(sum(eps_vect))

sqrt(g0$gcv.ubre)
sqrt(g1$gcv.ubre)

fit1 <- getViz(g1, nsim = 50)
summary(g1)
plot(sm(fit1, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()
( check1D(fit1, "wap") + l_gridCheck1D(gridFun = mean, stand = "sc", n=100) )$ggObj


############################################################
############block Cross Validation for choosing k
############################################################
univ<-function(k, block)
{
  #g<- gam(target~s(wap, k=k, bs="cr"), data=data0[-block,])
  g<- gam(target~s(wap, k=k, bs="cr") + s(seconds_in_bucket, k=20) + s(reference_price, k=10), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
}

Nblock<-10
borne_block<-seq(1, nrow(data0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))
  
K<-c(3:20)
rmseK<-lapply(K, function(k){lapply(block_list, univ,k=k)%>%unlist%>%rmse} )
plot(K, rmseK, type='b', pch=20)

################################################################################
############block Cross Validation for choosing k with different spline basis
################################################################################
univ<-function(k, block, bs)
{
  g<- gam(target~s(wap, k=k, bs=bs), data=data0[-block,])
  #g<- gam(target~s(wap, k=k, bs=bs) + s(seconds_in_bucket, k=3), data=data0[-block,])
  #g<- gam(target~s(wap, k=k, bs=bs) + s(seconds_in_bucket, k=3) + s(time_id, k=20), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
}

K<-c(4:20)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%rmse} )

col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "ps", "cs"), pch=20, ncol=2, bty='n')


gcr <- gam(target~s(wap, k=5, bs="cr"), data=data0)
gcr.fit <- predict(gcr, newdata=data0, type="terms")

gtp <- gam(target~s(wap, k=5, bs="tp"), data=data0)
gtp.fit <- predict(gtp, newdata=data0, type="terms")

gps <- gam(target~s(wap, k=5, bs="ps"), data=data0)
gps.fit <- predict(gps, newdata=data0, type="terms")

gad <- gam(target~s(wap, k=10, bs="ad", m=5, xt=list(bs="cr")), data=data0)
gad.fit <- predict(gad, newdata=data0, type="terms")


par(mfrow=c(1,1))
o<-order(data0$wap)
plot(data0$wap, data0$target-gcr$coef[1], pch=16, cex=0.5)
lines(data0$wap[o], gcr.fit[o], col=col[1])
lines(data0$wap[o], gtp.fit[o], col=col[2])
lines(data0$wap[o], gps.fit[o], col=col[3])
lines(data0$wap[o], gad.fit[o], col=col[4])
legend("top", col=col[1:4], c("cr", "tp", "ps", "ad"), pch=20, ncol=2, bty='n')



##########################################################################################################################
#####plot of each splines scaled by their coef (colors) and the estimated effect (black) for cubic regression basis
##########################################################################################################################
gcr.mat <- predict(gcr, newdata=data0, type="lpmatrix")
gcr.mat%*%gcr$coefficients-gcr$fitted
o<-order(data0$wap)
col<-piratepal("basel", length.out = 5)
plot(data0$wap, data0$target-gcr$coef[1], pch=16, cex=0.5)
for(i in c(2:ncol(gcr.mat)))
{
  lines(data0$wap[o], gcr.mat[o,i]*gcr$coefficients[i], col=col[i-1])
}
lines(data0$wap[o], gcr.fit[o], col="yellow", lwd=2)



##########################################################################################
#####plot of the different splines basis
##########################################################################################
par(mfrow=c(2,2))
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  print(dim(g.mat))
  plot(data0$wap[o],g.mat[o,2] , pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col=col[1], type='l', ylim=range(g.mat[,-1]))
  for(j in c(3:ncol(g.mat)))
  {
    lines(data0$wap[o], g.mat[o,j], col=col[j-1])
  }
}


#############en choisissant les noeuds
# gcr <- gam(target~s(wap, k=3, bs="cr"), data=data0, knots=list(wap=c(5,10,20)))
# g.mat <- predict(gcr, newdata=data0, type="lpmatrix")
# 
# plot(data0$wap[o],g.mat[o,2] , pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col=col[1], type='l', ylim=range(g.mat[,-1]))
# for(j in c(3:ncol(g.mat)))
# {
#   lines(data0$wap[o], g.mat[o,j], col=col[j-1])
# }
# abline(v=c(5,10,20))

##########################################################################################
#####plot of each splines scaled by their coef (colors) and the estimated effect (black)
##########################################################################################

par(mfrow=c(2,2))
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  plot(data0$wap, data0$target-listMod[[i]]$coef[1], pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col='grey')
  for(j in c(2:ncol(g.mat)))
  {
    lines(data0$wap[o], g.mat[o,j]*listMod[[i]]$coefficients[j], col=col[j-1])
  }
  g.fit <- predict(listMod[[i]], newdata=data0, type="terms")
  lines(data0$wap[o], g.fit[o], col="black", lwd=2)
}


#################################################################################
############time_id effect
#################################################################################
univ<-function(k, block, bs)
{
  g<- gam(target~s(time_id, k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
}

K<-c(4:40)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%rmse} )

par(mfrow=c(1,1))
col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "ps", "cs"), pch=20, ncol=2, bty='n')

g<- gam(target~s(time_id, k=30, bs='tp'), data=data0)
summary(g)


Kopt<-10
gcr <- gam(target~s(time_id, k=Kopt, bs="cr"), data=data0)
gcr.fit <- predict(gcr, newdata=data0, type="terms")

gtp <- gam(target~s(time_id, k=Kopt, bs="tp"), data=data0)
gtp.fit <- predict(gtp, newdata=data0, type="terms")

gps <- gam(target~s(time_id, k=Kopt, bs="ps"), data=data0)
gps.fit <- predict(gps, newdata=data0, type="terms")

gad <- gam(target~s(time_id, k=Kopt, bs="cr", m=10, xt=list(bs="cr")), data=data0)
gad.fit <- predict(gad, newdata=data0, type="terms")

par(mfrow=c(2,2))
o<-order(data0$time_id)
col<-piratepal("basel", length.out = 5)
listMod<-list(gcr, gtp, gps, gad)
names(listMod)<-c('cr', 'tp', 'ps', 'ad')
for(i in c(1:length(listMod)))
{
  g.mat <- predict(listMod[[i]], newdata=data0, type="lpmatrix")
  plot(data0$time_id, data0$target-listMod[[i]]$coef[1], pch=16, cex=0.25, ylab='',xlab='', main=names(listMod)[i], col='grey')
  for(j in c(2:ncol(g.mat)))
  {
    lines(data0$time_id[o], g.mat[o,j]*listMod[[i]]$coefficients[j], col=col[j-1])
  }
  g.fit <- predict(listMod[[i]], newdata=data0, type="terms")
  lines(data0$time_id[o], g.fit[o], col="black", lwd=2)
}


#################################################################################
############Trend effect
#################################################################################
univ<-function(k, block, bs)
{
  g<- gam(target~s(seconds_in_bucket, k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
}

K<-c(3:20)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )

par(mfrow=c(1,1))
col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKps), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
legend("top", col=col, c("cr", "tp"), pch=20, ncol=2, bty='n')

g<- gam(target~s(seconds_in_bucket, k=4, bs='cr'), data=data0)

summary(g)
plot(g)

#################################################################################
############3 variables model
#################################################################################
gam1<-gam(target~s(seconds_in_bucket, k=3)+s(time_id, k=30,bs='tp')+s(wap,k=5))
summary(gam1)

   
#####wap
univ<-function(k, block, bs)
{
  g<- gam(target~s(seconds_in_bucket,k=5)+s(time_id,k=30,bs='tp')+s(wap,k=k, bs=bs), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
} 
K<-c(3:15)
rmseK<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
col<-piratepal("basel", length.out = 9)
plot(K, rmseK, type='b', pch=20, ylim= range(rmseK), col=col[1])

#####time_id
univ<-function(k, block, bs)
{
  g<- gam(target~s(seconds_in_bucket,k=5)+s(time_id,k=k,bs=bs)+s(wap,k=5, bs='tp'), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
} 
K<-c(10:40)
rmseK<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%rmse} )
col<-piratepal("basel", length.out = 9)
plot(K, rmseK, type='b', pch=20, ylim= range(rmseK), col=col[1])



############################################################################
##############exercice 2: fit a GAM model consedering all predictors
############################################################################
gam1<-gam(target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5), data=data0)
summary(gam1)  

blockRMSE<-function(equation, block)
{
  g<- gam(as.formula(equation), data=data0[-block,])
  forecast<-predict(g, newdata=data0[block,])
  return(data0[block,]$target-forecast)
} 

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)
Block_residuals<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc1<-rmse(Block_residuals)
boxplot(Block_residuals)
plot(Block_residuals, type='l')

plot(data0$wap, Block_residuals, pch=16)
plot(data0$time_id, Block_residuals, pch=16)
plot(data0$ask_size, Block_residuals, pch=16)

gam_prov <- gam(Block_residuals~s(ask_size), data=data0)  
summary(gam_prov)
fit <- getViz(gam_prov, nsim = 50)
plot(sm(fit, 1), n = 400) + l_points() + l_fitLine() + l_ciLine()


equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)
gam2<-gam(equation, data=data0)
summary(gam2)
Block_residuals2<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc2<-rmse(Block_residuals2)
rmseBloc2

plot(data0$imbalance_size, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$imbalance_size))   
summary(gam_prov)

plot(data0$reference_price, Block_residuals2, pch=16)
gam_prov <- gam(Block_residuals2~s(data0$reference_price))   
summary(gam_prov)
plot(gam_prov)

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)+s(reference_price)
gam3<-gam(equation, data=data0)
summary(gam3)
Block_residuals3<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc3<-rmse(Block_residuals3)
rmseBloc3

#####change the reference_price in linear effect
equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)+reference_price
gam4<-gam(equation, data=data0)
summary(gam4)
Block_residuals4<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc4<-rmse(Block_residuals4)
rmseBloc4


plot(data0$bid_size, Block_residuals4, pch=16)
gam_prov <- gam(Block_residuals4~s(data0$bid_size))   
summary(gam_prov)
plot(gam_prov)

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)+reference_price+s(bid_size)
gam5<-gam(equation, data=data0)
summary(gam5)
Block_residuals5<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc5<-rmse(Block_residuals5)
rmseBloc5

plot(gam5$residuals, type='l')

noel = which(abs(data0$Day - 24) <= 3 & data0$Month == 12)
consoNoel = vector("numeric", length(data0$seconds_in_bucket))
consoNoel[noel] = 1
data0 <- data.frame(data0, consoNoel)

plot(data0$seconds_in_bucket, gam5$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$seconds_in_bucket[select], gam5$residuals[select], col='red', pch=20)

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30, bs='cc')+s(wap,k=5)+s(ask_size, k=10)+reference_price+s(bid_size)
+consoNoel
gam6<-gam(equation, data=data0)
summary(gam6)
Block_residuals6<-lapply(block_list, blockRMSE, equation=equation)%>%unlist
rmseBloc6<-rmse(Block_residuals6)
rmseBloc6

plot(data0$seconds_in_bucket, gam6$residuals, type='l')
select<-which(data0$consoNoel==1)
points(data0$seconds_in_bucket[select], gam6$residuals[select], col='red', pch=20)

data0[which(abs(gam6$residuals)>3000), 1:3]

noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
consoNoel = vector("numeric", length(data1$seconds_in_bucket))
consoNoel[noel] = 1
data1 <- data.frame(data1, consoNoel)


ychap6 <- predict(gam6, newdata=data1)
rmse(data1$target-ychap)

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30, bs='cc')+te(seconds_in_bucket, wap, k=c(3, 5))+s(ask_size, k=10)+reference_price+s(bid_size)

gam7<-gam(equation, data=data0)
summary(gam7)
ychap <- predict(gam7, newdata=data1)
rmse(data1$target-ychap)

par(mfrow=c(1,1))
plot(data1$target, type='l')
lines(ychap, col='red')
lines(ychap6, col='blue')

########################################################################################################################
###############################seconds_in_bucket weighted models
########################################################################################################################
####best window
windowForecast<-function(equation, Deb, NbYear)
{
  selec0 <- which((data0$Year>=Deb)&(data0$Year<=Deb+NbYear-1))
  selec1 <- which(data0$Year==Deb+NbYear)
  g<-gam(equation, data=data0[selec0,])
  g$forecast<-predict(g, newdata=data0[selec1,])
  residuals <-data0[selec1,]$target-g$forecast
  return(residuals)
}

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)+reference_price+s(bid_size)+consoNoel

rmseY <- NULL
for(NbYear in c(2:9))
{
  YearList<-c(1996:(2008-NbYear))
  residuals <- lapply(YearList, windowForecast, equation=equation, NbYear)
  rmseY <- c(rmseY, rmse(unlist(residuals)))
}

plot(c(2:9), rmseY, type='b', pch=20)
min(rmseY)



####with exponentiallly decreasing weights 

NbYear <- 6
YearList <- c(1996:(2008-NbYear))
memory <- 1/100
rmseY <- NULL
for(Deb in YearList)
{
  selec0 <- which((data0$Year>=Deb)&(data0$Year<=Deb+NbYear-1))
  selec1 <- which(data0$Year==Deb+NbYear)
  g<-gam(equation, data=data0[selec0,], weights=exp(memory*data0[selec0,]$seconds_in_bucket))
  g$forecast<-predict(g, newdata=data0[selec1,])
  residuals[[Deb-YearList[1]+1]] <-data0[selec1,]$target-g$forecast
  rmseY <- c(rmseY, rmse(residuals[[Deb-YearList[1]+1]]))
}
rmse(unlist(residuals))






##########grille sur memory

memory <- 1/100
weights=exp(memory*data0$seconds_in_bucket)
plot(data0$seconds_in_bucket, weights, type='l')

memory <- 1/200
weights=exp(memory*data0$seconds_in_bucket)
plot(data0$seconds_in_bucket, weights, type='l')



NbYear <- 6
YearList <- c(1996:(2008-NbYear))
memoryGrid <- 1/(c(1:10)*100)
rmseMemo <- array(0, dim=length(memoryGrid))
for(i in c(1:length(memoryGrid)))
{
  rmseY <- NULL
  memory <- memoryGrid[i]
  for(Deb in YearList)
  {
    selec0 <- which((data0$Year>=Deb)&(data0$Year<=Deb+NbYear-1))
    selec1 <- which(data0$Year==Deb+NbYear)
    g<-gam(equation, data=data0[selec0,], weights=exp(memory*data0[selec0,]$seconds_in_bucket))
    g$forecast<-predict(g, newdata=data0[selec1,])
    residuals[[Deb-YearList[1]+1]] <-data0[selec1,]$target-g$forecast
    rmseY <- c(rmseY, rmse(residuals[[Deb-YearList[1]+1]]))
  }
  rmseMemo[i] <- rmse(unlist(residuals))
}

plot(memoryGrid, rmseMemo, type='l')



NbYear <- 6
YearList <- c(1996:(2008-NbYear))
memoryGrid <- 1/(c(1:10)*100)

memoryGrid <- seq(1/200, 1/100, length=10)
rmseMemo <- array(0, dim=length(memoryGrid))
for(i in c(1:length(memoryGrid)))
{
  rmseY <- NULL
  memory <- memoryGrid[i]
  for(Deb in YearList)
  {
    selec0 <- which((data0$Year>=Deb)&(data0$Year<=Deb+NbYear-1))
    selec1 <- which(data0$Year==Deb+NbYear)
    g<-gam(equation, data=data0[selec0,], weights=exp(memory*data0[selec0,]$seconds_in_bucket))
    g$forecast<-predict(g, newdata=data0[selec1,])
    residuals[[Deb-YearList[1]+1]] <-data0[selec1,]$target-g$forecast
    rmseY <- c(rmseY, rmse(residuals[[Deb-YearList[1]+1]]))
  }
  rmseMemo[i] <- rmse(unlist(residuals))
}

plot(memoryGrid, rmseMemo, type='l')







########################################################################################################################
###############################forecasting step
########################################################################################################################
noel = which(abs(data1$Day - 24) <= 3 & data1$Month == 12)
consoNoel = vector("numeric", length(data1$seconds_in_bucket))
consoNoel[noel] = 1
data1 <- data.frame(data1, consoNoel)
gam6.forecast=predict(gam6,newdata=data1)
rmse(data1$target-gam6.forecast)
mape(data1$target,gam6.forecast)

equation <- target~s(seconds_in_bucket,k=3)+s(time_id,k=30)+s(wap,k=5)+s(ask_size, k=10)+reference_price+s(bid_size)+consoNoel
memory <- memoryGrid[which.min(rmseMemo)]
gam6.weighted<-gam(equation, data=data0, weights=exp(memory*data0$seconds_in_bucket))
gam6.weighted.forecast=predict(gam6.weighted,newdata=data1)
rmse(data1$target-gam6.weighted.forecast)
mape(data1$target,gam6.weighted.forecast)


par(mfrow=c(1,1))
plot(data1$seconds_in_bucket, data1$target, type='l')
lines(data1$seconds_in_bucket, gam6.forecast, col='red')
lines(data1$seconds_in_bucket, gam6.weighted.forecast, col='blue')




########################################################################################################################
###############################rolling forecasts 
########################################################################################################################
model <- gam6
ychap <- predict(model, newdata=data1[1,])

for(i in c(1:(nrow(data1)-1)))
{
  data.update <- rbind(data0, data1[1:i,])
  model <-gam(model$formula, data=data.update)
  ychap <- c(ychap, predict(model, newdata=data1[i+1,]))
}
rmse(data1$target-ychap)
mape(data1$target,ychap)
plot(data1$target, type='l')
lines(ychap, col='red')


model <- gam6.weighted
ychap.weighted <- predict(model, newdata=data1[1,])
for(i in c(1:(nrow(data1)-1)))
{
  data.update <- rbind(data0, data1[1:i,])
  model <-gam(model$formula, data=data.update, weights=exp(memory*data.update$seconds_in_bucket))
  ychap.weighted <- c(ychap.weighted, predict(model, newdata=data1[i+1,]))
}
rmse(data1$target-ychap.weighted)
mape(data1$target,ychap.weighted)

plot(data1$target, type='l')
lines(ychap.weighted, col='red')
lines(ychap, col='blue')


plot(cumsum(data1$target-ychap), col='blue', type='l')
lines(cumsum(data1$target-ychap.weighted), col='red')



##########################################################################################
##########avec bam
##########################################################################################
bam6 <- bam(equation, data=data0)

model <- bam6
ychap.bam <- predict(model, newdata=data1[1,])
for(i in c(1:(nrow(data1)-1)))
{
  model <- bam.update(model, data=data1[i,])
  ychap.bam <- c(ychap.bam, predict(model, newdata=data1[i+1,]))
}

rmse(data1$target-ychap.bam)
mape(data1$target,ychap.bam)








