################GOAL: find the best one week ahaed forecasting strategy (weekly data)
################minimiser  RMSE or MAPE for the year 2009


###############Initialisation 
rm(list=objects())
###############packages
library(dygraphs)
library(xts)
library(tidyverse)

###############Import data
data0<-read_csv("/Users/bigmac/Desktop/MDA woohoo/projet ML prévision/optiver-trading-at-the-close/train.csv", col_names =TRUE)
####commande de base associée: 
#data0<-read.table("https://www.math.u-psud.fr/~goude/Materials/Data/data_conso_hebdo0.txt", header =TRUE)
data0$stock_id<-as.factor(data0$stock_id)
summary(data0)

data_stock1 <- subset(data0, stock_id == 1)

data_stock1_day1 <- subset(data_stock1, date_id == 1)
data_stock1_day2 <- subset(data_stock1, date_id == 2)
data_stock1_day3 <- subset(data_stock1, date_id == 3)
data_stock1_day4 <- subset(data_stock1, date_id == 4)

par(mfrow = c(2, 2))
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$target,type='l')
plot(data_stock1_day2$seconds_in_bucket, data_stock1_day2$target,type='l')
plot(data_stock1_day3$seconds_in_bucket, data_stock1_day3$target,type='l')
plot(data_stock1_day4$seconds_in_bucket, data_stock1_day4$target,type='l')

#par(mfrow = c(1, 1))
#columns_plot <- c(15, )
#matplot(data_stock1_day1, type = "l", lty = 1, col = columns_plot, xlab = "X-axis label", ylab = "Y-axis label")
#legend("topright", legend = colnames(data_stock1_day1[, columns_plot]), col = columns_plot, lty = 1)

par(mfrow = c(2,4))
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$target,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$reference_price,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$wap,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$imbalance_buy_sell_flag,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$far_price,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$near_price,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$ask_price,type='l')
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$bid_price,type='l')



data_stock2 <- subset(data0, stock_id == 2)

data_stock2_day1 <- subset(data_stock2, date_id == 1)
data_stock2_day2 <- subset(data_stock2, date_id == 2)
data_stock2_day3 <- subset(data_stock2, date_id == 3)
data_stock2_day4 <- subset(data_stock2, date_id == 4)

par(mfrow = c(2, 2))
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$target,type='l')
plot(data_stock2_day2$seconds_in_bucket, data_stock2_day2$target,type='l')
plot(data_stock2_day3$seconds_in_bucket, data_stock2_day3$target,type='l')
plot(data_stock2_day4$seconds_in_bucket, data_stock2_day4$target,type='l')

#par(mfrow = c(1, 1))
#columns_plot <- c(15, )
#matplot(data_stock2_day1, type = "l", lty = 1, col = columns_plot, xlab = "X-axis label", ylab = "Y-axis label")
#legend("topright", legend = colnames(data_stock2_day1[, columns_plot]), col = columns_plot, lty = 1)

par(mfrow = c(2,4))
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$target,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$reference_price,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$wap,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$imbalance_buy_sell_flag,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$far_price,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$near_price,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$ask_price,type='l')
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$bid_price,type='l')


######################################################################################################
######################################################################################################
par(mfrow = c(1,1))


####serie de la consommation electrique et de la temperature sur un an
par(mar = c(5,5,2,5))
plot(data_stock1_day1$seconds_in_bucket,data_stock1_day1$target, type='l',xlab='Time',ylab='target')
par(new=TRUE)
plot(data_stock1_day1$seconds_in_bucket,data_stock1_day1$wap,col='red',type='l',axes=F,xlab='',ylab='')
axis(side = 4, col='red')
mtext(side = 4, line = 3, 'wap', col='red')
legend("topleft",c("target","wap"),col=c("black","red"),lty=1,ncol=1,bty="n")


par(mar = c(5,5,2,5))
plot(data_stock2_day1$seconds_in_bucket,data_stock2_day1$target, type='l',xlab='Time',ylab='target')
par(new=TRUE)
plot(data_stock2_day1$seconds_in_bucket,data_stock2_day1$wap,col='red',type='l',axes=F,xlab='',ylab='')
axis(side = 4, col='red')
mtext(side = 4, line = 3, 'wap', col='red')
legend("topleft",c("target","wap"),col=c("black","red"),lty=1,ncol=1,bty="n")


par(mar = c(5,5,2,5))
plot(data_stock1_day2$seconds_in_bucket,data_stock1_day2$target, type='l',xlab='Time',ylab='target')
par(new=TRUE)
plot(data_stock1_day2$seconds_in_bucket,data_stock1_day2$wap,col='red',type='l',axes=F,xlab='',ylab='')
axis(side = 4, col='red')
mtext(side = 4, line = 3, 'wap', col='red')
legend("topleft",c("target","wap"),col=c("black","red"),lty=1,ncol=1,bty="n")
####nuages de points


plot(data_stock1_day1$wap, data_stock1_day1$target,pch=16,cex=0.5)
lm.temp<-lm(target~wap+I(wap^2), data=data_stock1_day1)
#lines(data0$Temp,lm.temp$fitted,col='red') ###pas satisfaisant
o<-order(data_stock1_day1$wap)
data_stock1_day1$wap[o]
lines(data_stock1_day1$wap[o],lm.temp$fitted[o],col='red',lwd=2)
lines(data_stock1_day1$wap,lm.temp$fitted,col='blue',lwd=2)


par(mfrow = c(2,3))
plot(data_stock1_day1$imbalance_size, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$imbalance_buy_sell_flag, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$reference_price, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$matched_size, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$far_price, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$near_price, data_stock1_day1$target,pch=16,cex=0.5)

par(mfrow = c(2,2))
plot(data_stock1_day1$bid_price, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$bid_size, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$ask_price, data_stock1_day1$target,pch=16,cex=0.5)
plot(data_stock1_day1$ask_size, data_stock1_day1$target,pch=16,cex=0.5)
par(mfrow = c(1,1))


################correlation analysis
cor(data_stock1_day1$wap,data_stock1_day1$target)



coltypes<-lapply(data_stock1_day1, class)%>%unlist()
cor(data_stock1_day1[, which(coltypes=='numeric')])

acf(data_stock1_day1$target,lag.max=52)
pacf(data_stock1_day1$target,lag.max=52)

#################################################################################

par(mfrow = c(2,4))
plot(data_stock1$time_id, data_stock1$target,type='l')
plot(data_stock1$time_id, data_stock1$reference_price,type='l')
plot(data_stock1$time_id, data_stock1$wap,type='l')
plot(data_stock1$time_id, data_stock1$imbalance_buy_sell_flag,type='l')
plot(data_stock1$time_id, data_stock1$far_price,type='l')
plot(data_stock1$time_id, data_stock1$near_price,type='l')
plot(data_stock1$time_id, data_stock1$ask_price,type='l')
plot(data_stock1$time_id, data_stock1$bid_price,type='l')



par(mfrow = c(2, 2))
plot(data_stock2$time_id, data_stock2$target,type='l')
plot(data_stock2$time_id, data_stock2$target,type='l')
plot(data_stock2$time_id, data_stock2$target,type='l')
plot(data_stock2$time_id, data_stock2$target,type='l')

#par(mfrow = c(1, 1))
#columns_plot <- c(15, )
#matplot(data_stock2_day1, type = "l", lty = 1, col = columns_plot, xlab = "X-axis label", ylab = "Y-axis label")
#legend("topright", legend = colnames(data_stock2_day1[, columns_plot]), col = columns_plot, lty = 1)

par(mfrow = c(2,4))
plot(data_stock2$time_id, data_stock2$target,type='l')
plot(data_stock2$time_id, data_stock2$reference_price,type='l')
plot(data_stock2$time_id, data_stock2$wap,type='l')
plot(data_stock2$time_id, data_stock2$imbalance_buy_sell_flag,type='l')
plot(data_stock2$time_id, data_stock2$far_price,type='l')
plot(data_stock2$time_id, data_stock2$near_price,type='l')
plot(data_stock2$time_id, data_stock2$ask_price,type='l')
plot(data_stock2$time_id, data_stock2$bid_price,type='l')


######################################################################################################
######################################################################################################
par(mfrow = c(1,1))


####serie de la consommation electrique et de la temperature sur un an
par(mar = c(5,5,2,5))
plot(data_stock1$time_id,data_stock1$target, type='l',xlab='Time',ylab='target')
par(new=TRUE)
plot(data_stock1$time_id,data_stock1$wap,col='red',type='l',axes=F,xlab='',ylab='')
axis(side = 4, col='red')
mtext(side = 4, line = 3, 'wap', col='red')
legend("topleft",c("target","wap"),col=c("black","red"),lty=1,ncol=1,bty="n")


par(mar = c(5,5,2,5))
plot(data_stock2$time_id,data_stock2$target, type='l',xlab='Time',ylab='target')
par(new=TRUE)
plot(data_stock2$time_id,data_stock2$wap,col='red',type='l',axes=F,xlab='',ylab='')
axis(side = 4, col='red')
mtext(side = 4, line = 3, 'wap', col='red')
legend("topleft",c("target","wap"),col=c("black","red"),lty=1,ncol=1,bty="n")


####nuages de points


plot(data_stock1$wap, data_stock1$target,pch=16,cex=0.5)
lm.temp<-lm(target~wap+I(wap^2), data=data_stock1)
#lines(data0$Temp,lm.temp$fitted,col='red') ###pas satisfaisant
o<-order(data_stock1$wap)
data_stock1_day1$wap[o]
lines(data_stock1$wap[o],lm.temp$fitted[o],col='red',lwd=2)
lines(data_stock1$wap,lm.temp$fitted,col='blue',lwd=2)


par(mfrow = c(2,3))
plot(data_stock1$imbalance_size, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$imbalance_buy_sell_flag, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$reference_price, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$matched_size, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$far_price, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$near_price, data_stock1$target,pch=16,cex=0.5)

par(mfrow = c(2,2))
plot(data_stock1$bid_price, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$bid_size, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$ask_price, data_stock1$target,pch=16,cex=0.5)
plot(data_stock1$ask_size, data_stock1$target,pch=16,cex=0.5)
par(mfrow = c(1,1))




##############################################################################################################

library(dygraphs)
library(xts)

plot(data_stock1_day1$seconds_in_bucket,data_stock1_day1$target,type='l')
data_stock1_day1$seconds_in_bucket <- as.Date(data_stock1_day1$seconds_in_bucket)
###########transformation des variables a visualiser en objets xts
Target=xts(data_stock1_day1$target,order.by=data_stock1_day1$seconds_in_bucket)
WAP=xts(data_stock1_day1$wap,order.by=data_stock1_day1$seconds_in_bucket)

#####################visualisation des series temporelles conso et temp.
time.series=cbind(Target,WAP)
names(time.series)=c("target","wap")
dygraph(time.series)


###########standardisation des variables a visualiser
#standardisation
Target.sd=Target/sd(Target)
WAP.sd=WAP/sd(WAP)

sd.time.series=cbind(Target.sd,WAP.sd)
names(sd.time.series)=c("Target.sd","WAP.sd")
##dygraph de base
dygraph(sd.time.series)


##avec fenetre de selection
dygraph(sd.time.series)%>% dyRangeSelector()   ##equivalent a ça: dyRangeSelector(dygraph(sd.time.series))

##############################################################################################################


lm1_1<-lm(target~seconds_in_bucket, data=data_stock1_day1)
summary(lm1_1)
par(mfrow = c(1,1))
plot(data_stock1_day1$seconds_in_bucket, data_stock1_day1$target, type='l')
lines(data_stock1_day1$seconds_in_bucket,lm1_1$fitted,col='red')

lm1_2<-lm(target~seconds_in_bucket, data=data_stock1_day2)
summary(lm1_2)
par(mfrow = c(1,1))
plot(data_stock1_day2$seconds_in_bucket, data_stock1_day2$target, type='l')
lines(data_stock1_day2$seconds_in_bucket,lm1_2$fitted,col='red')

lm2_1<-lm(target~seconds_in_bucket, data=data_stock2_day1)
summary(lm2_1)
par(mfrow = c(1,1))
plot(data_stock2_day1$seconds_in_bucket, data_stock2_day1$target, type='l')
lines(data_stock2_day1$seconds_in_bucket,lm2_1$fitted,col='red')


lm1<-lm(target~wap, data=data_stock1)
summary(lm1_1)
par(mfrow = c(1,1))
plot(data_stock1$wap, data_stock1$target, type='p')
lines(data_stock1$wap,lm1$fitted,col='red')


lm1_all<-lm(target ~ wap + imbalance_size*imbalance_buy_sell_flag + reference_price + ask_size + ask_price + bid_size + bid_price, data=data_stock1)
summary(lm1_all)


lm2_all<-lm(target ~ wap + imbalance_size*imbalance_buy_sell_flag + reference_price + ask_size + ask_price + bid_size + bid_price, data=data_stock2)
summary(lm2_all)


##############################################################

p = 4
data_stock1_t <- data.frame(data_stock1)
data_stock1_t$target_until_t <- lapply(seq_len(nrow(data_stock1_t)), function(i) data_stock1_t$target[data_stock1_t$time_id <= data_stock1_t$time_id[i] &  max(data_stock1_t$time_id[i] - p, 0) <= data_stock1_t$time_id]) 
data_stock1_t$wap_until_t <- lapply(seq_len(nrow(data_stock1_t)), function(i) data_stock1_t$wap[data_stock1_t$time_id <= data_stock1_t$time_id[i] &  max(data_stock1_t$time_id[i] - p, 0) <= data_stock1_t$time_id])

df_split_target <- data.frame(do.call(rbind, data_stock1_t[(p+1):nrow(data_stock1),]$target_until_t))
df_split_wap <- data.frame(do.call(rbind, data_stock1_t[(p+1):nrow(data_stock1),]$wap_until_t))

colnames(df_split_target) <- paste("target", 1:ncol(df_split_target))
colnames(df_split_wap) <- paste("wap", 1:ncol(df_split_wap))

data_stock1[(p+1):nrow(data_stock1),]
data_stock1_p <- cbind(cbind(data_stock1[(p+1):nrow(data_stock1),], df_split_target), df_split_wap)


################################################################################



