#https://stats.stackexchange.com/questions/275652/rpart-cross-validation

rm(list=objects())
library(rpart)
library(magrittr)
library(party)
library(yarrr)
library(tree)
library(rpart.plot)
library(progress)
library(mgcv)
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
####################################################################################################################################################################################
############################################################package tree
####################################################################################################################################################################################
#parametrage par défaut
data_app<-data0
data1 <- data_test
#eq <- target ~ wap+ NumWeek + imb_s1 + IPI +imb_s11+Time +IPI_CVS+Year
eq <- target~time_id+date_id+reference_price+matched_size+ask_size+imb_s1+wap
treefit0<-tree(eq ,data = data_app)
print(treefit0)


mean(data0$target)

sum((data0$target-mean(data0$target))^2)

test<-print(treefit0)
dev<-test[1]$frame$dev
plot(dev,type='l')

plot(treefit0)
text(treefit0,cex=1)

ychap.tree0<-predict(treefit0,newdata=data1)
mape(data1$target,ychap.tree0)
rmse(data1$target-ychap.tree0)

plot(data1$target[1:20],type='l')
lines(ychap.tree0[1:20],col='red')
treefit0

library(ranger)
rf0<-ranger(eq,num.tree=500,data=data0, importance = 'impurity')
rf0.fitted=predict(rf0,data=data0)
rf0.forecast=predict(rf0,data=data1)
mape(data0$target,rf0.fitted)
mape(data1$target,rf0.forecast)
rmse(data1$target-rf0.forecast)
plot(rf0)
?plot.randomForest
rf0
imp=importance(rf0,type=1)
imp[order(imp,decreasing=TRUE)]



# Installer le package si nécessaire
# install.packages("forecast")

# Charger le package
library(forecast)

# Charger votre série temporelle (remplacez ceci par vos données réelles)
# Exemple de données générées aléatoirement pour illustration

target_ts <- ts(data0$target, frequency = 55)
arima_model <- auto.arima(target_ts)
print(arima_model)

# Prédire les valeurs futures
forecast_values <- forecast(auto_arima_model, h=10)

# Tracer la série temporelle et les prédictions
plot(auto_arima_model)
lines(forecast_values$mean, col="red")# Afficher le graphique


####

treefit0<-tree(target ~imb_s1+IPI+wap+NumWeek+imb_s11+Time ,data = data_app
               ,control = tree.control(nobs=nrow(data_app),
                                       minsize=1, mindev=0.01))
print(treefit0)


#prunning

treefit<-tree(target ~imb_s1+IPI+wap+NumWeek+imb_s11+Time ,data = data_app,
              control = tree.control(nobs=nrow(data_app), minsize=1, mindev=0))
print(treefit)
plot(treefit)
text(treefit,cex=.1)
ychap <- predict(treefit, newdata=data1)
rmse(data1$target-ychap)

treefit.cv <- cv.tree(treefit,FUN = prune.tree, K=10)
plot(tail(treefit.cv$size,10),tail(treefit.cv$dev,10),type='l',xlab="Size",ylab="Deviance")
plot(treefit.cv$size,treefit.cv$dev,type='l',xlab="Size",ylab="Deviance")

size.opt<-10
treefit.prune<-prune.tree(treefit,best=size.opt)
plot(treefit.prune)
text(treefit.prune,cex=.8)

print(treefit.prune)


ychap.tree<-predict(treefit.prune,newdata=data1)
mape(data1$target,ychap.tree)
rmse(data1$target-ychap.tree)

plot(data1$target,type='l')
lines(ychap.tree,col='red')
lines(ychap,col='blue')


####################################################################################################################################################################################
############################################################package rpart
####################################################################################################################################################################################
#default 
rpart0 <- rpart(eq, data = data0)
rpart0.forecast<-predict(rpart0,newdata=data1)
mape(data1$target,rpart0.forecast)
rmse(data1$target-rpart0.forecast)

######arbre overfit
names(data0)
eq <- target ~ wap+ NumWeek + imb_s1 + IPI +imb_s11+Time +IPI_CVS+Year

control <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                         maxcompete = 4, maxsurrogate = 0, usesurrogate = 0, 
                         xval = 10,
                         surrogatestyle = 0, maxdepth = 30)
rpart0 <- rpart(eq, data = data0,control=control)
rpart0.forecast<-predict(rpart0,newdata=data1)
mape(data1$target,rpart0.forecast)
rmse(data1$target-rpart0.forecast)

par(mfrow=c(1,1))
plot(rpart0)
#text(rpart0)
######prunning
plotcp(rpart0)

head(rpart0$cptable)
####rel error: variance intra normalisé à 1 pour 0 split
####cp indicator
####amélioration relative du critère qu'on optimise 
cp <- rpart0$cptable[,"CP"]
rpart0$cptable[1:9,"rel error"]-rpart0$cptable[2:10,"rel error"]
cp[1:9]

####xerror:erreur de cross validation calculée en interne de rpart 
####xstd: std de l'erreur de CV


plot(rpart0$cptable[,"rel error"], type='b', pch=3)
plot(rpart0$cptable[,"xerror"], type='b', pch=3)

plot(rpart0$cptable[1:100,"rel error"], type='l', pch=3)
lines(rpart0$cptable[1:100,"xerror"], col='red')
lines(rpart0$cptable[1:100,"xerror"]+rpart0$cptable[1:100,"xstd"], col='red', lty='dashed')
lines(rpart0$cptable[1:100,"xerror"]-rpart0$cptable[1:100,"xstd"], col='red', lty='dashed')

##rule of thumb
##rel_error + xstd < xerror
test <- rpart0$cptable[,"rel error"]+rpart0$cptable[,"xstd"]-rpart0$cptable[,"xerror"]
plot(test)
abline(h=0)

plot(rpart0$cptable[1:100,"rel error"], type='l', pch=3)
lines(rpart0$cptable[1:100,"xerror"], col='red')
lines(rpart0$cptable[1:100,"xerror"]+rpart0$cptable[1:100,"xstd"], col='red', lty='dashed')
lines(rpart0$cptable[1:100,"xerror"]-rpart0$cptable[1:100,"xstd"], col='red', lty='dashed')
abline(v=which(test<0)[1], lty='dashed')

bestcp <- rpart0$cptable[which(test<0)[1],"CP"]
rpart1 <- prune(rpart0, cp = bestcp)
rpart1.forecast<-predict(rpart1,newdata=data1)
mape(data1$target,rpart0.forecast)
rmse(data1$target-rpart0.forecast)




bestcp <- rpart0$cptable[which.min(rpart0$cptable[,"xerror"]),"CP"]
rpart1 <- prune(rpart0, cp = bestcp)
rpart1.forecast<-predict(rpart1,newdata=data1)
mape(data1$target,rpart0.forecast)
rmse(data1$target-rpart0.forecast)

mape(data1$target,rpart1.forecast)
rmse(data1$target-rpart1.forecast)


pruneForecast <- function(cp)
{
  rpart_test <- prune(rpart0, cp = cp)
  rpart_test.forecast<-predict(rpart_test,newdata=data1)
  return(rmse(data1$target-rpart_test.forecast))
}

rmseCP <- lapply(cp, pruneForecast)%>%unlist
plot(log(cp), rmseCP, type='b', pch=20, cex=0.5)
xmin <- which.min(rpart0$cptable[,"xerror"])
points(log(cp[xmin]), rmseCP[xmin], col='red', pch=20, cex=2)
min(rmseCP)




#########################################
###### xval influence 
##########################################
names(data0)
eq <- target ~ wap+ NumWeek + imb_s1 + IPI +imb_s11+Time +IPI_CVS+Year

vfoldSelec <- function(xval)
{
  control <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                           maxcompete = 4, maxsurrogate = 0, usesurrogate = 0, xval = xval,
                           surrogatestyle = 0, maxdepth = 30)
  rpart0 <- rpart(eq, data = data0,control=control)
  bestcp <- rpart0$cptable[which.min(rpart0$cptable[,"xerror"]),"CP"]
  rpart1 <- prune(rpart0, cp = bestcp)
  rpart1.forecast<-predict(rpart1,newdata=data1)
  return(rmse(data1$target-rpart1.forecast))
}

xval <- c(2:20)
Nsample <- 100
rmseXVAL <- matrix(0, nrow=Nsample, ncol=length(xval))
pb <- progress_bar$new(
  format = "  downtargeting [:bar] :percent in :elapsed",
  total = Nsample, clear = FALSE, width= 60)

for(i in c(1:Nsample))
{
  rmseXVAL[i,] <- lapply(xval, vfoldSelec)%>%unlist 
  pb$tick()
  Sys.sleep(1 / 100)
}

plot(colMeans(rmseXVAL), type='l')
boxplot(rmseXVAL)
lines(colMeans(rmseXVAL), col='red')


###############blockwise cross validation

forecast.cp <- function(mod, cp, data)
{
  rpart_test <- prune(mod, cp = cp)
  rpart_test.forecast<-predict(rpart_test,newdata=data)
  return(data$target-rpart_test.forecast)
}

fitRpart <-function(block)
{
  control <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                           maxcompete = 4, maxsurrogate = 0, usesurrogate = 0, xval = 2,
                           surrogatestyle = 0, maxdepth = 30)
  rpart0 <- rpart(eq, data = data0[-block,],control=control)
  cp <- rpart0$cptable[,"CP"]
  residuals <- lapply(cp, forecast.cp, data=data0[block,], mod=rpart0)
  rmseres <- lapply(residuals,rmse)
  res <- list()
  res$rmseres <- rmseres
  res$cp <- cp
  return(res)
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


test <- fitRpart(block_list[[1]])
plot(test$rmseres%>%unlist, type='l')

cv.residuals <-lapply(block_list, fitRpart)





#col<-piratepal("basel", length.out = length(cv.residuals))
col <-colorRampPalette(c("red","blue"), alpha = TRUE)(length(cv.residuals))


plot(cv.residuals[[1]]$cp%>%log, unlist(cv.residuals[[1]]$rmseres), type='l', col=col[1], ylim=c(1000,10000))
xmin <- which.min(unlist(cv.residuals[[1]]$rmseres))
points(log(cp[xmin]), unlist(cv.residuals[[1]]$rmseres)[xmin], col=col[1], pch=20, cex=2)

for(i in c(2:length(cv.residuals)))
{
  lines(cv.residuals[[i]]$cp%>%log, unlist(cv.residuals[[i]]$rmseres), col=col[i])
  xmin <- which.min(unlist(cv.residuals[[i]]$rmseres))
  points(log(cp[xmin]), unlist(cv.residuals[[i]]$rmseres)[xmin], col=col[i], pch=20, cex=2)
  
}
lines(log(cp), rmseCP, lwd=2)
best <- which.min(rmseCP)
points(log(cp[best]), rmseCP[best], col='black', pch=20, cex=2)
legend('top', legend=c(1:length(cv.residuals)), col=col, lty=1, ncol=3, bty = "n")

plot(log(cp), rmseCP)



err <- 0
r <- lapply(cv.residuals, function(x){range(head(x$cp, 500))})%>%unlist
seqcp <- seq(min(log(r)), max(log(r)), length=1000)

for(i in c(1:length(cv.residuals)))
{
  
  y <- head(unlist(cv.residuals[[i]]$rmseres), 500)
  x <- head(cv.residuals[[i]]$cp,500)
  dat0 <- data.frame(y,x)
  g <- gam(y~s(I(log(x)), k=50), data=dat0)
  dat1 <- data.frame(x=exp(seqcp))
  
  smooth_rmse <- predict(g, newdata=dat1)
  err <- err+smooth_rmse
  
  # plot(log(x), y, type='l')
  # lines(log(x), g$fitted, col='red')
  # lines(seqcp, smooth_rmse, col='blue')
}




err <- err/length(cv.residuals)


plot(log(cp), rmseCP, lwd=2, type='l')
lines(seqcp, err, col='blue')

col <-colorRampPalette(c("red","blue"), alpha = TRUE)(length(cv.residuals))

plot(cv.residuals[[1]]$cp%>%log, unlist(cv.residuals[[1]]$rmseres), type='l', col=col[1], ylim=c(1000,10000))
xmin <- which.min(unlist(cv.residuals[[1]]$rmseres))
points(log(cp[xmin]), unlist(cv.residuals[[1]]$rmseres)[xmin], col=col[1], pch=20, cex=2)

for(i in c(2:length(cv.residuals)))
{
  lines(cv.residuals[[i]]$cp%>%log, unlist(cv.residuals[[i]]$rmseres), col=col[i])
  xmin <- which.min(unlist(cv.residuals[[i]]$rmseres))
  points(log(cp[xmin]), unlist(cv.residuals[[i]]$rmseres)[xmin], col=col[i], pch=20, cex=2)
  
}
lines(log(cp), rmseCP, lwd=2)
best <- which.min(rmseCP)
points(log(cp[best]), rmseCP[best], col='black', pch=20, cex=2)
lines(seqcp, err, lwd=2, col='darkgreen')

points(seqcp[which.min(err)], err[which.min(err)], col='darkgreen', pch=20, cex=2)
legend('top', legend=c(1:length(cv.residuals)), col=col, lty=1, ncol=3, bty = "n")







################################################
######graphical representation
################################################
par(mfrow=c(1,2))
plot(rpart0, main="avant")
plot(rpart1, main="après prunning")
text(rpart1, cex = 0.5)

par(mfrow=c(1,1))
plot(rpart1)
text(rpart1, cex=0.5)

prp(rpart1, box.palette= "Greens")					
show.prp.palettes()



par(mfrow=c(1,1))
plot(data1$target, type='l')
lines(rpart0.forecast, col='red')
lines(rpart1.forecast, col='blue')





##################################################################################party
#Conditional Inference Trees
# ##################################################################################
# 1) Test the global null hypothesis of independence between any of the input variables and the response 
# (which may be multivariate as well). 
# Stop if this hypothesis cannot be rejected. Otherwise select the input variable with strongest association to the resonse. 
# This association is measured by a p-value corresponding to a test for the partial null hypothesis of a single input variable and the response. 
# 2) Implement a binary split in the selected input variable. 
# 3) Recursively repeate steps 1) and 2)
#library(party)
eq <- target ~ wap+ NumWeek + imb_s1 + IPI +imb_s11+Time +IPI_CVS+Year
controls <- ctree_control(teststat = "quad", testtype = "Bonferroni",
                          mincriterion = 0.95, minbucket = 5, 
                          maxsurrogate = 0, mtry = 0, 
                          savesplitstats = TRUE, maxdepth = 0)
mod.ctree<-ctree(target ~imb_s1+IPI+wap+NumWeek+imb_s11+Time , data = data0, controls=controls)
ychap.ctree<-predict(mod.ctree,newdata=data1)
mape(data1$target,ychap.ctree)
rmse(data1$target-ychap.ctree)

plot(mod.ctree)
summary(mod.ctree)


#the variables before the | are passed to the model and the variables after the | are used for partitioning
mod.mob<-mob(target ~wap+imb_s1+IPI+NumWeek+imb_s11+Time | 
               imb_s1+IPI +NumWeek+imb_s11+Time,
             data = data0,model = linearModel)
ychap.mob<-predict(mod.mob,newdata=data1)
mape(data1$target,ychap.mob)
rmse(data1$target-ychap.mob)

col<-piratepal("basel", length.out = 4)
par(mfrow=c(1,1))
plot(data1$target, type='l')
lines(rpart0.forecast, col=col[1])
lines(rpart1.forecast, col=col[2])
lines(ychap.mob, col=col[3])
lines(ychap.ctree, col=col[4])
legend("top", col=col, legend=c("rpart0", "rpart1", "mob", "ctree"), lty=1)



########################################################################################## 
##############################CV  CARET
data_app<-data0
train_control <- trainControl(method="repeatedcv", number=10,repeats = 10)
cpGrid <- expand.grid(cp=seq(0,0.1,length=10))
set.seed(1000)
rpart.CV<-train(eq, data = data_app, method ="rpart",  
                trControl=train_control, metric="RMSE",tuneGrid = cpGrid,minsplit=2)
plot(seq(0,0.1,length=10),rpart.CV$results$RMSE)

rpart1<- rpart(eq , data = data_app, method = "anova",
               control = rpart.control(minsplit = 2,
                                       cp = cpGrid[which.min(rpart.CV$results$RMSE),]))

rpart1.forecast<-predict(rpart1,newdata=data1)
mape(data1$target,rpart1.forecast)

plot(data1$target,type='l')
lines(rpart1.forecast,col='red')

plot(rpart1)
text(rpart1,cex=0.5)

summary(rpart1)



############################################################
test.rpart<-function(cp,eq,data_a,data_b)
{
  rp<-rpart(eq, data = data_a, method = "anova",
            control = rpart.control(minsplit = 2,cp = cp))
  
  return(rmse(data_b$target-predict(rp,newdata=data_b)))
}
cpGrid<-seq(0,0.01,length=10)
eq<-as.formula(eq)
rmse.test<-unlist(lapply(cpGrid,test.rpart,data_a=data_app,data_b=data1,eq=eq))

plot(cpGrid,rmse.test,type='b',col='blue',pch=20,ylim=range(rmse.test,rpart.CV$results$RMSE))
lines(cpGrid,rpart.CV$results$RMSE,col='red',type='b',pch=20)











####################################################################################################################################################################################
############################################################correction de la tendance
####################################################################################################################################################################################
gtrend <- gam(target ~ s(Time, k=5), data=data0)
summary(gtrend)
plot(data0$target, type='l')
lines(gtrend$fitted.values, col='red')

data0$target.detrend <- data0$target-gtrend$fitted.values
data0$target.detrend1 <- data0$wap-gtrend$fitted.values

data1$target.detrend <- data1$target-predict(gtrend, newdata=data1)
data1$target.detrend1 <- data1$wap-predict(gtrend, newdata=data1)


eq <- target.detrend ~ wap+ NumWeek + imb_s1 + IPI +imb_s11+Time +IPI_CVS+Year

control <- rpart.control(minsplit = 2, minbucket = 1, cp = 0, 
                         maxcompete = 4, maxsurrogate = 0, usesurrogate = 0, 
                         xval = 10,
                         surrogatestyle = 0, maxdepth = 30)
rpart0 <- rpart(eq, data = data0,control=control)
rpart0.forecast<-predict(rpart0,newdata=data1)

mape(data1$target,rpart0.forecast+predict(gtrend, newdata=data1))
rmse(data1$target-rpart0.forecast-predict(gtrend, newdata=data1))

plot(data1$target.detrend, type='l')
lines(rpart0.forecast, col='red')

bestcp <- rpart0$cptable[which.min(rpart0$cptable[,"xerror"]),"CP"]
rpart1 <- prune(rpart0, cp = bestcp)
rpart1.forecast<-predict(rpart1,newdata=data1)
mape(data1$target,rpart0.forecast+predict(gtrend, newdata=data1))
rmse(data1$target-rpart0.forecast-predict(gtrend, newdata=data1))

mape(data1$target,rpart1.forecast+predict(gtrend, newdata=data1))
rmse(data1$target-rpart1.forecast-predict(gtrend, newdata=data1))


pruneForecast <- function(cp)
{
  rpart_test <- prune(rpart0, cp = cp)
  rpart_test.forecast<-predict(rpart_test,newdata=data1)
  return(rmse(data1$target-rpart_test.forecast))
}

rmseCP <- lapply(cp, pruneForecast)%>%unlist
plot(log(cp), rmseCP, type='b', pch=20, cex=0.5)
xmin <- which.min(rpart0$cptable[,"xerror"])
points(log(cp[xmin]), rmseCP[xmin], col='red', pch=20, cex=2)
min(rmseCP)

###############################C###############################C###############################C
###############################C5.0
###############################C###############################C###############################C###############################C

treeModel <- C5.0(x = data0[, -c(2,3,4,6, 12,13)], y = as.factor(floor(data0$target)))
#, control = C5.0Control(winnow = F, earlyStopping = F))
#plot(treeModel)
summary(treeModel)
treeModel.forecast <- predict(treeModel, newdata=data1[, -c(2,3,6, 12,13)])%>%as.character%>%as.numeric
plot(data1$target, type='l')
lines(treeModel.forecast, col='red')


?C5.0







