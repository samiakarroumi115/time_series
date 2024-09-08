#projet de séries chronologiques
#série chronologique des Indices des heures hebdomadaires agrégées des employés
#****************************************************
library(tseries)
CEU <- read.table("C:/Users/user/Desktop/CEU431.txt", header = TRUE);
CEU
head(CEU)#afficher un extrait du tableau

C<-CEU[,2] # Avec C désigne la colonne des CEU4300000016 :Indices des heures hebdomadaires 
#agrégées de tous les employés, transport et entreposage, indice 2007=100, mensuel
C
#dater la série ts= time series.
c.ts <- ts(C, start=2007,deltat=1/12)
c.ts
#******************dater la série********************
Date<-CEU[,1]
D<-as.Date(Date)
library(zoo)
x <- as.yearmon(2007 + seq(0, 179)/12)
x
D<-as.Date(x)
#**********************representation graphique**************
plot(D,C, type="l", col="red" , xlab="observation_date",ylab="CEU")
plot(c.ts, type="l", col="red" )
#**********passage dun modéle multiplicatif à un modéle additif*******
CEUlog<-log10(c.ts)
plot(CEUlog, type="l", col="red" )
#**********************ACF******************************************
plot(acf(CEUlog,lag.max=36,plot=FALSE),ylim=c(-1,1))
#**************test de stationnarité***********
adf.test(CEUlog)
kpss.test(CEUlog)
pp.test(CEUlog)
PP.test(CEUlog)
#****************** différenciation (I-B) de la série*****************
y_dif1=diff(CEUlog,lag=1,differences=1)

plot(acf(y_dif1,lag.max=36,plot=FALSE),ylim=c(-1,1))
#****************test de stationnarité en tendance***************
adf.test(y_dif1)
kpss.test(y_dif1)
pp.test(y_dif1)
PP.test(y_dif1)
#*************************différenciation (I-B^12) saisonniére+ acf et pacf************
y_dif_1_12=diff(y_dif1,lag=12,differences=1)
par(mfrow=c(1,2))
plot(acf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))
plot(pacf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))
#*******test de stationnarité de la série différencier en saisonnalilté****************
adf.test(y_dif_1_12)
kpss.test(y_dif_1_12)
pp.test(y_dif_1_12)
PP.test(y_dif_1_12)
#################Identification, estimation et validation de modèles###########""""""
par(mfrow=c(1,1))
plot(acf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))
plot(pacf(y_dif_1_12,lag.max=36,plot=FALSE),ylim=c(-1,1))

#**************Modèle1************************
model1=arima(CEUlog,order=c(1,1,1),list(order=c(1,1,1),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model1)
#**************Modèle2************************
model2=arima(CEUlog,order=c(1,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model2)
AIC(model2)
#**************Modèle 3************************
model3=arima(CEUlog,order=c(0,1,1),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model3)
#**************Modèle 4************************
model4=arima(CEUlog,order=c(0,1,0),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model4)
AIC(model4)

#on retient le modèle 4
#*************Validation du modèle*******************************************************************
Box.test(model4$residuals,type="Ljung-Box")
Box.test(model4$residuals,type="Box-Pierce")
Box.test(model4$residuals,type="Box-Pierce")$p.value
#*******test de normalité des résidus du modèle4************
shapiro.test(model4$residuals)
jarque.bera.test(model4$residuals)
#*************Prévision*******************
library(forecast)
pred_model4=predict(model4,n.ahead=12)
plot(pred_model4$pred)

pred_model4=forecast(model4,h=12,level=95)
pred=(pred_model4$mean)
pred_l=ts((pred_model4$lower),start=c(2022,1),frequency=12)
pred_u=ts((pred_model4$upper),start=c(2022,1),frequency=12)
ts.plot((CEUlog),pred,pred_l,pred_u,xlab="t",ylab="CEU",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(1,3,2,2))
plot(pred_model4)

#********************Analyse a posteriori: série tronquée*******************
x_tronc=window(CEUlog,end=c(2020,12))
x_a_prevoir=window(CEUlog,start=c(2021,1))
#************verification de l'ajustement du modèle 4 a la série tronquée***************
model4tronc=arima(x_tronc,order=c(0,1,0),list(order=c(0,1,1),period=12),include.mean=FALSE,method="CSS-ML")
coeftest(model4tronc)
# le modèle 4 toujours retenu

# box pierce et ljung box pour tester si les residus sont des bruit blanc
Box.test(model4tronc$residuals,type="Ljung-Box")
Box.test(model4tronc$residuals,type="Box-Pierce")
#shapiro test teste la normalit? des r?sidus 
shapiro.test(model4tronc$residuals)
#******Prévision et IC comparer avec la série reelle********
pred_model4tronc=forecast(model4tronc,h=12,level=95)
pred_tronc=predict(model4tronc,n.ahead=12)
pred_tronc=(pred_model4tronc$mean)
pred_l_tronc=ts((pred_model4tronc$lower),start=c(2021,1),frequency=12)
pred_u_tronc=ts((pred_model4tronc$upper),start=c(2021,1),frequency=12)
par(mfrow=c(1,2))
plot(pred_l_tronc)
plot(pred_u_tronc)
#******************Zoom sur la partie prévision********************
par(mfrow=c(1,1))
ts.plot(window(CEUlog,start=c(2021,1)),pred,pred_l,pred_u,xlab="t",ylab="CEU",col=c(1,2,3,3),lty=c(1,1,2,2),lwd=c(1,3,2,2))
legend("topleft",legend=c("X","X_prev"),col=c(1,2,3,3),lty=c(1,1),lwd=c(3,3))
legend("topright",legend=c("int95%_inf","int95%_sup"),col=c(3,3),lty=c(2,2),lwd=c(2,2))

#***************************RMSE et MAPE***********************
rmse=sqrt(mean((x_a_prevoir-pred_tronc)^2))
rmse
mape=mean(abs(1-pred_tronc/x_a_prevoir))*100
mape


#*********************modélisation automatique*****************

auto.arima(CEUlog, trace=TRUE)
library(forecast)
auto.arima(p.ts, trace=TRUE)

