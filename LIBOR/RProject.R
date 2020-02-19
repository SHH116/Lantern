rm(list=ls())
require(xts)
require(jsonlite)
require(ggplot2)
require(reshape2)

##1-Data Download:
data=read.csv('C:/Users/hajyhass/Downloads/CADShort.csv')
data=read.csv('C:/Users/hajyhass/Downloads/CADLong.csv')
data=read.csv('C:/Users/hajyhass/Downloads/USDLong.csv')
data=read.csv('C:/Users/hajyhass/Downloads/USDShort.csv')


##2-Data Cleaning and preperation:
change=function(x){as.numeric(as.character(x))}
new=apply(data[,2:13],2,change)
sasha <- xts(new,order.by=as.Date(data$DATE))
sasha=na.omit(sasha)
#sasha$mean= rowMeans(subset(sasha, select = c(1:12)))
library(matrixStats)
ep2<-endpoints(sasha, on="years")
ep2
cad_1m_diff_1<-diff(sasha,lag=1)
cad_1m_diff_25<-diff(sasha[,1:12],lag=25)
cad_1m_diff_1=na.omit(cad_1m_diff_1)
cad_1m_diff_25=na.omit(cad_1m_diff_25)


##3-Plot of original time series, the 1-day and 25-day 
USD_LIBOR_Original=sasha
plot(sasha) ##plot(sasha[,1]) form can be used to specifically plot 1M libor
windows(20,10)
plot(cad_1m_diff_1) ##plot(cad_1m_diff_1[,4]) can be used to specifially plot 4M libor
plot(cad_1m_diff_25)


##5-plot of average term strcuture per tenor by year.
plt=period.apply(sasha[,1:12],INDEX=ep2,FUN=mean)
library(PerformanceAnalytics)
plt2=StdDev(sasha[,1:12])
plt2=period.apply(sasha[,1:12],INDEX=ep2,FUN=StdDev)  #we must find a way to replace var with sd
matplot(t(plt), type = "l")
matplot(t(plt2), type = "l")

library(viridisLite)
windows(20,10)
plot.zoo(cad_1m_diff_1, plot.type = 'single', 
         col=viridis(15), main="Differenced Interest Rates", 
         ylab="Diff IR", xlab="Time", xaxt ="n", yaxt="n")
legend(x = "topright", legend = colnames(cad_1m_diff_1),
       col = viridis(15), cex = 0.45, lwd = 3)
# Multiple plots in one piece of code
windows(20,10)
#USD_Diff_1D=cad_1m_diff_1
plot.zoo(x = USD_Diff_1D, plot.type = 'multiple', 
         ylim = c(-0.6, 0.6), cex.axis = 0.7, 
         ylab = 1:30, col = viridis(15))



zoo.rates <- as.zoo(sasha)   ##Had I merged, then I would have used libor instead 
tsRainbow <- rainbow(ncol(zoo.rates))
windows(20,10)
zoo::plot.zoo(sasha, screens = 1, xlab="Time",ylab="LIBOR Rate %",las=2, main="Real LIBOR Over Time by Tenor", plot.type = "single",col =tsRainbow)
grid()
legend(x="topleft", legend=colnames(ts(sasha)), lty=1, cex=.64,bty="n",col =tsRainbow)


windows(20,10)
zoo::plot.zoo(plt2[,1:12], screens = 1, xlab="Time",ylab="LIBOR Rate %",las=2, main="Libor Over Time by Tenor", plot.type = "single",col =tsRainbow)
grid()
legend(x="right", legend=colnames(plt2[,1:12]), lty=1, cex=.64,bty="n",col =tsRainbow)


##graph IR
zoo.rates <- as.zoo(ts(sasha))   ##Had I merged, then I would have used libor instead 
tsRainbow <- rainbow(ncol(zoo.rates))
windows(7,4)
plot(x=zoo.rates,col=tsRainbow, main="Historical Interest Rates", ylab="Rates", xlab="Time", plot.type="single", xaxt ="n", yaxt="n")
axis(1)
pts<- pretty(zoo.rates)
axis(2, at=pts, labels=paste(pts, "%", sep=""))
legend(x="topright", legend=colnames(sasha), lty=1, col=tsRainbow, cex=.64,bty="n")





##4-Fitting the distributions
##Original
library(fitdistrplus)
plotdist(as.numeric(sasha[,1:12]), histo = TRUE, demp = FALSE, breaks = 60)
hist(as.numeric(sasha[,1:12]), breaks = 60, freq = TRUE)
#plotdist(as.numeric(cad_1m_diff_1[,1:12]), histo = TRUE, demp = TRUE, breaks = 60)
#plotdist(as.numeric(cad_1m_diff_25[,1:12]), histo = TRUE, demp = TRUE,breaks = 180)
##1- day fit into laplace
library(LaplacesDemon)
library(vsgoftest)
windows(20,10)
m = median(cad_1m_diff_25[,2])
t = mean(abs(cad_1m_diff_25[,2]-m))
data <- as.numeric(cad_1m_diff_25[,2])
hist(data, freq = FALSE, breaks = 400, xlab = "USD_DIFF_25D", main = "USD 25 Day Differenced fitted into Laplace")
x<-seq(-0.50,0.50,by=0.01)
curve(6*dlaplace(x,m,t), add=TRUE, col="red")

set.seed(1)
data = cad_1m_diff_1[,2]
qqplot(x=qexp(ppoints(100)), y=data, main="Exponential Q-Q Plot USD 1D DIFF",
       xlab="Theoretical Quantiles", ylab= "USD 1D DIFF Quantiles")
qqline(data, distribution=qexp, col='red')
data = cad_1m_diff_25[,2]
qqplot(x=qexp(ppoints(100)), y=data, main="Exponential Q-Q Plot USD 25D DIFF",
       xlab="Theoretical Quantiles", ylab= "USD 25D DIFF Quantiles")
qqline(data, distribution=qexp, col='red')
#25-day t-fit
data <- cad_1m_diff_25[,2]
hist(data, freq = FALSE, breaks = 500)
x<-seq(-2,2,by=0.001)
curve(3*dt(x,5000,log = FALSE), add=TRUE, col="red")
library(fitdistrplus)
fit <- fitdist(as.numeric(cad_1m_diff_25[,2]), "t", start=list(df=5000))
summary(fit)
qqcomp(list(fit))
ppcomp(list(fit))


##but is is really laplace/t? Extra checking
library(lawstat)
library(goft)
##below, low A2,W2,U2,D,V are desirable to confirm that the distribution is actually laplace
laplace.test(as.numeric(cad_1m_diff_1[,6])) # I could never get that either for 1-day or 25-day, so I must change my original time series

o=rnorm(350,0,1)
o=rlaplace(100,0,2)
t.test(as.numeric(cad_1m_diff_25[,6]))
laplace_test(o)
laplace_test(as.numeric(cad_1m_diff_1[,6]))
o=rlaplace(100,0,2)



##But are the distributions coming from stationary data? ADF statonarity test
library(aTSA)
library(tseries)
##original data Stationary?No
for (i in 1:12){
print(adf.test(sasha[,i]))}
##1-lag data stationary? Yes
for (i in 1:12){
print(adf.test(cad_1m_diff_1[,i]))}
##25-lag data stationary? Yes
for (i in 1:12){
print(adf.test(cad_1m_diff_25[,i]))}


##6-Relationship between St.Dev of 1-day and 25 day
##there is a sqrt(time) relationship: sqrt(25) or lag-related relatinoship. No-tenor relatinoship
for (i in 1:12){
print(StdDev(cad_1m_diff_25[,i])/StdDev(cad_1m_diff_1[,i]))
#print(StdDev(cad_1m_diff_25[,i]))
}

##7-Correlations
library(Hmisc)
rcorr(cad_1m_diff_1, type = c("pearson"))
rcorr(cad_1m_diff_25, type = c("pearson"))
rcorr(cad_1m_diff_1, type = c("spearman"))
rcorr(cad_1m_diff_25, type = c("spearman"))
cor(cad_1m_diff_1, method = c("kendall"))
cor(cad_1m_diff_25, method = c("kendall"))
res=cor(cad_1m_diff_1/100, method = c("pearson"))
res2=cor(cad_1m_diff_25/100, method = c("kendall"))
library(corrplot)
windows(20,10)
corrplot(res2, type = "upper", 
         tl.col = "black", tl.srt = 45)


##8-Principal Components Analysis
cad_pca_1=prcomp(cad_1m_diff_1, scale = FALSE)
plot(cad_pca_1, type="l")
summary(cad_pca_1)

cad_pca_25=prcomp(cad_1m_diff_25, scale = FALSE)
plot(cad_pca_25, type="l")
summary(cad_pca_25)
#Summary of the above, the first 2 PCA of 25 lag explain as much behaviour as the first 8 PCA of lag 1.
#Therefore, lag 25 allows larger dimensionality reduction (more attractive for future simulations)

##9-Vasicek
library(sde)
library(stats4)

dcOU <- function (x , t , x0 , theta , log = FALSE ){
  Ex <- theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*t)
  Vx <- theta[3]^2 * (1 - exp(-2*theta[2]*t )) / (2*theta[2])
  dnorm (x , mean=Ex , sd = sqrt(Vx), log = log )
}
OU.lik <- function ( theta1 , theta2 , theta3 ){
  n <- length(X)
  dt <- deltat(X)
  -sum(dcOU(X[2:n],t=dt,x0=X[1:( n-1)] , c ( theta1 , theta2 , theta3 ) , log = TRUE ))}
#set.seed(123)
#X <- sde.sim ( model = "OU" , theta = c(3 ,1 ,2) , N =5000 , delta =1)
trial=as.numeric(cad_1m_diff_1[,12])
X<-trial
mean(X)
m = median(X)
t = mean(abs(X-m))
mle ( OU.lik , start = list ( theta1 =0.4 , theta2 =0.1 , theta3 =0.05) ,method ="L-BFGS-B" , lower = c( -Inf ,-Inf ,0.000001), upper=c(Inf,Inf,Inf)) -> fit
summary (fit)
unname(coef(fit))
#confint(fit)
z<-(sde.sim ( model = "OU" ,X0=0, theta = c(0.00434613, 1.68049081, 0.05577446) , N =1000 , delta =1))
k<-vector()
for (i in 1:250){k[i]<-(cumsum(z[i]+i*mean(X)))}  
plot(ts(cumsum(z)))
plot(sasha)


plot(z)
plot(sasha)
mean(z)
sd(z)
zz=diff(z,lag=1)
laplace.test(zz)
zz<-(sde.sim ( model = "OU" ,X0=0.01, theta = c(-0.0001572787 ,0.0535345800 ,0.0010432412) , N =5000 , delta =1/5000))
zz
plot(zz)
mean(zz)
sd(zz)
##The estimated thetas are statistically insignificant, reduce time duration of the original libor data, or choose a more smooth regime

library(Sim.DiffProc)
fx <- expression( theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*x)) ## drift coefficient of model
gx <- expression( theta[3]^2 * (1 - exp(-2*theta[2]*x )) / (2*theta[2]) ) ## diffusion coefficient of model
fitsde(data = ts(x1), drift = fx, diffusion = gx, start = list(theta1 = 1,
                                                                theta2 = 1,
                                                                theta3 = 1), pmle = "euler")

plot(GBM(N=5000, T=1, t0=0, x0=7,theta=-0.1445057, sigma=0.6677972))
plot(GBM(N=1000,T=1,t0=0,x0=1,theta=4,sigma=2))



##Drift is a constant slope, trend is an exponential slope
##https://stats.stackexchange.com/questions/104215/difference-between-series-with-drift-and-series-with-trend
#no trend no drift
eps <- rnorm(1000,0,1)
plot(eps)
adf.test(eps)
##No Drift No trend
x <- arima.sim(list(order = c(1,0,0),ar = 0.2),n = 100)
adf.test(x)
plot(x)
#drift but no trend
adf.test(co2)
plot(co2)
#no drift no trend
x <- rnorm(1000)  # no unit-root
adf.test(x)
plot(x)
#drift and some trend
y <- diffinv(x)   # contains a unit-root
adf.test(y)
plot(y)
#no drift no trend
t<-rnorm(1000,0,1)  ##staionary
plot(t)
adf.test(t)
##drift but no trend
t<-t+0.3   #non stationary
plot(cumsum(t))
adf.test(cumsum(t))
help("adf.test")
##no drift no trend
d=diff(t,lag=1)
plot(d)
adf.test(cumsum(d))  ##back to stationarity

#drift only
n<-300
drift <- 1
x1    <- rep(0, n)
for(i in seq.int(2, n)){
  x1[i] <- drift + x1[i-1] + eps[i]
}
plot(ts(x1))
jj<-diff(x1,lag=1)
plot(ts(jj))
adf.test(x1)

#trend and drift
trend <- seq_len(n)
x2    <- rep(0, n)
for(i in seq.int(2, n)){
  x2[i] <- trend[i] + x2[i-1] + eps[i]
}
plot(ts(x2))
adf.test(x2)

adf.test(sasha[,1])
adf.test(tester$residuals)
plot(cumsum(tester$residuals))
adf.test(cad_1m_diff_1[,1])
plot(cumsum(cad_1m_diff_1[,1]))

# histogram and CDF plot
#plotdist(serv, histo = TRUE, demp = TRUE)
library(fitdistrplus)
library(actuar)
#windows(7,4)
plotdist(as.numeric(sasha[,1:12]), histo = TRUE, demp = TRUE, breaks = 60)
plotdist(as.numeric(cad_1m_diff_1[,1:12]), histo = TRUE, demp = TRUE, breaks = 60)
plotdist(as.numeric(cad_1m_diff_25[,1:12]), histo = TRUE, demp = TRUE,breaks = 180)
library(e1071)
mean(cad_1m_diff_1[,1])
sd(cad_1m_diff_1[,1])^2
skewness(cad_1m_diff_1[,1])
kurtosis(cad_1m_diff_1[,1])

mean(cad_1m_diff_25[,1])
sd(cad_1m_diff_25[,1])^2
skewness(cad_1m_diff_25[,1])
kurtosis(cad_1m_diff_25[,1])

library("PearsonDS")
#moments <- c(mean = mean(cad_1m_diff_1),variance =0.05,skewness=4,kurtosis=kurtosis(cad_1m_diff_1))
#storage=rpearson(6000, moments = moments)
#plotdist(storage, histo = TRUE, demp = TRUE, breaks = 60)
# Cullen and Frey (Kurtosis vs square of skewness) graph
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}
##



#library(stats)
#est.par <- ebeta(X=cad_1m_diff_1, method="numerical.MLE")
#est.par
#descdist(news, boot = 1000)
#descdist(as.numeric(cad_1m_diff_1[,1]), boot = 1000)
#descdist(as.numeric(news), boot = 1000)
#MAX=max(cad_1m_diff_log_25[,1])
#MIN=min(cad_1m_diff_log_25[,1])
#news<- ((as.numeric(cad_1m_diff_log_25[,1])-MIN)/(MAX-MIN))
#news

#! the st.dev o 25 lag is 5 times larger than 1 lag
#! Are we achieving aynting by having a distribution of returns 5 times wider which carries lower skew and kurtosis?
nrow(cad_1m_diff_1)
nrow(cad_1m_diff_25)
library(fracdiff)
library(tseries)
#plot(news)
plot(sasha)
plot(cad_1m_diff_1)
plot(cad_1m_diff_25)
#the tests below show which data set has reached stationarity
adf.test(sasha[,1], alternative="stationary", k=0)
adf.test(sasha2[,1], alternative="explosive", k=0)
adf.test(cad_1m_diff_1[,1], alternative="stationary")
adf.test(cad_1m_diff_1[,1], alternative="stationary", k=0)


adf.test(cad_1m_diff_1[,1], alternative="explosive", k=0)
adf.test(cad_1m_diff_25, alternative="stationary", k=0)
adf.test(cad_1m_diff_25[,1], alternative="explosive", k=0)
counter=c(1:12)
windows(20,10)
par(mfrow=c(3,4)) 
for (i in counter){acf(sasha[,i])}
windows(20,10)
par(mfrow=c(3,4)) 
for (i in counter){acf(cad_1m_diff_1[,i])}
windows(20,10)
par(mfrow=c(3,4)) 
for (i in counter){acf(cad_1m_diff_25[,i])}
pacf(sasha[,1])  ##only first lag is significant
acf(cad_1m_diff_1[,1])
pacf(cad_1m_diff_1[,1])  ##only first lag might matter
acf(cad_1m_diff_25[,1])
pacf(cad_1m_diff_25[,1])  ##only first lag is significant
# Below the most negative value for aic is desireble, but pay attention that we have not reached stationarity yet

arima(sasha[,1],order=c(1,0,0)) #high ar1 implies non-stationarity
arima(cad_1m_diff_1[,1], order =c(1,0,1))
arima(cad_1m_diff_25[,1], order = c(1,0,1)) #high ar1 implies non-stationarity
#autoarfima(sasha[,1],criterion = "AIC", method = "full", arfima = FALSE)
tester=arima(sasha[,1]/100,order=c(1,0,0))
pacf(tester$residuals)
acf(tester$residuals)
tester$residuals

##Below Computes roughly how much more differencing with need per time series, to acheive stationarity
fdGPH(tester$residuals)
fdGPH(cad_1m_diff_1[,1])  ##requires the least amount of differencing
fdGPH(cad_1m_diff_25[,1])

ftfinal.aic <- Inf
ftfinal.order <- c(0,0,0)
for (p in 1:5) for (d in 0:2) for (q in 1:5) {ftcurrent.aic <- AIC(arima(sasha[,1], order=c(p, d, q)))
if (ftcurrent.aic < ftfinal.aic) {
ftfinal.aic <- ftcurrent.aic
ftfinal.order <- c(p, d, q)
ftfinal.arima <- arima(sasha[,1], order=ftfinal.order)
}
}
ftfinal.arima
acf(resid(ftfinal.arima))
acf(resid(ftfinal.arima)^2)
ft.garch <- garch(cad_1m_diff_1[,1], trace=F)
ft.res <- ft.garch$res[-1]
plotdist(ft.res, histo = TRUE, demp = TRUE, breaks = 60)

acf(ft.res)
acf(ft.res^2)
library(rugarch)
#library(quantmod)
fb1<-ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(2,2)),distribution.model = "std")
fbGarch1<-ugarchfit(spec = fb1,data = cad_1m_diff_1[,1])
fbGarch1

## the line below needs S4 type of R
ugrachforecast(fbGarch1, n.ahead=5,n.roll = 2579, data = cad_1m_diff_1[,1])



a=diffseries(sasha[,1], d=1)  #so 1 differencing in total
b=diffseries(cad_1m_diff_1[,1],d=0.25)  #so 1+0.25=1.25 differencing in total
c=diffseries(cad_1m_diff_25[,1],d=0.0)  #so 1+0.25=1.25 differencing in total


class(a)
plot(ts(a))
plot(ts(b))
plot(ts(c))
acf(ts(a))
pacf(ts(a))
acf(ts(b))
pacf(ts(b))
acf(ts(c))
pacf(c)

adf.test(ts(a), alternative="stationary", k=0)
adf.test(ts(b), alternative="stationary", k=0)
adf.test(ts(c), alternative="stationary",k=0)

fdGPH(a)
fdGPH(b)
fdGPH(c)
library("SMFI5")
est.vasicek(x1, method = "Hessian", days = 300, significanceLevel = 0.95)
practice=(sasha[,1])
practice$newcol <- rep(1/12,nrow(practice))
practice[,1]=practice[,1]
w=est.vasicek(practice,method = "Hessian", days=5000, significanceLevel = 0.95)
w

library(sde)
library(stats4)

dcOU <- function (x , t , x0 , theta , log = FALSE ){
  Ex <- theta[1]/theta[2]+(x0 - theta[1]/theta[2]) * exp (-theta[2]*t)
  Vx <- theta[3]^2 * (1 - exp(-2*theta[2]*t )) / (2*theta[2])
  dnorm (x , mean=Ex , sd = sqrt(Vx), log = log )
}
OU.lik <- function ( theta1 , theta2 , theta3 ){
  n <- length(X)
  dt <- deltat(X)
  -sum(dcOU(X[2:n],t=dt,x0=X[1:( n-1)] , c ( theta1 , theta2 , theta3 ) , log = TRUE ))}
#set.seed(123)
#X <- sde.sim ( model = "OU" , theta = c(3 ,1 ,2) , N =5000 , delta =1)
trial=as.numeric(cad_1m_diff_1[,1]/100)
X<-trial
X
X<-tester$residuals
plot(X)
plot(cad_1m_diff_1[,1])
mean(X)
sd(X)
mle ( OU.lik , start = list ( theta1 =0.01 , theta2 =0.05 , theta3 =0.05) ,method ="L-BFGS-B" , lower = c( -Inf ,-Inf ,0.000001), upper=c(Inf,Inf,Inf)) -> fit
summary (fit)
z<-(sde.sim ( model = "OU" ,X0=0.073, theta = c(0.367739 ,0.0233 ,0.757699) , N =5000 , delta =1/5000))
z
plot(z)
mean(z)
sd(z)
set.seed(123)
sde.sim(X0=0.155, theta=c(0.079908021 ,1.550955223 ,0.063567544), N=1230,delta=1/250, rcdist=rcCIR,method="cdist")->Y
plot(Y, main="Cox-Ingersoll-Ross")
Y
plot(sasha[,1])
prof <- profile(fit)
par(mfrow=c(1,3))
plot(prof)
par(mfrow=c(1,1))
vcov(fit) 
confint(fit)

set.seed(123)
sde.sim(X0=0.071, theta=c(3  ,1 ,2), model="CIR") -> X
plot(X, main="Cox-Ingersoll-Ross")

plot(rcOU(n=5000, Dt=1, x0=0.071, theta=c(0.0028,0.0012,0.067)))

mean(replicate(100,{sim.vasicek(0.002862235,0.002241089 , 0.006629194,0.317,1000, 1/250)
  }
))
library(SMFI5)
sim.vasicek(0.0005751632 ,0.0004146927 ,0.0100214321,0.8117,1000, 1/250)
sd(sasha[,12])

#write.csv(sasha, file = "c:/users/hajyhass/sasha.csv")
#Principal Component Analysis
summary(prcomp(sasha,scale = FALSE))
cad_pca_1=prcomp(cad_1m_diff_1, scale = FALSE)
plot(cad_pca_1, type="l")
summary(cad_pca_1)

cad_pca_25=prcomp(cad_1m_diff_25, scale = FALSE)
plot(cad_pca_25, type="l")
summary(cad_pca_25)

#correlation matrix
library(Hmisc)
rcorr(cad_1m_diff_1, type = c("pearson"))
rcorr(cad_1m_diff_1, type = c("spearman"))
cor(cad_1m_diff_1, method = c("kendall"))
res=cor(cad_1m_diff_1, method = c("kendall"))

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
library(SMFI5)
#out = est.vasicek(practice.vasicek)
sim.vasicek(7.80, 0.0352, 1.3033, 8, 5100, 1/250)
#sim.vasicek(0.025,4.1104,6.829945,13.1875,5834,1/252)


#out = bond.vasicek(0.5,2.55,0.365,0.3,0,3.55,1080,c(1/12,2/12, 3/12,4/12,5/12, 6/12, 1),365);
#methods("plot")

getS3method("plot", "histogram")

library(fitdistrplus)
library(actuar)

#data("groundbeef", package = "fitdistrplus")
#str(groundbeef)
library("MASS")
#fitdistr(ts(c),"cauchy")
fn=fitdist(as.numeric(ts(a)),"norm")
summary(fn)



# histogram and CDF plot
plotdist(as.numeric(ts(a)), histo = TRUE, demp = TRUE, breaks = 60)
hist(ts(c),freq=F, breaks = 180)
plotdist(as.numeric(ts(b)), histo = TRUE, demp = TRUE,breaks=60)
plotdist(as.numeric(ts(a)), histo = TRUE, demp = TRUE,breaks=60)
#plotdist(as.numeric(cad_1m_diff_1[,1]), histo = TRUE, demp = TRUE)

# Cullen and Frey (Kurtosis vs square of skewness) graph
windows(20,10)
descdist(as.numeric(cad_1m_diff_1[,2]), boot = 1000)##original 0.875 day differenced looks beta 
descdist(as.numeric(ts(b)), boot = 1000)##1-day, 1.125 day differenced looks beta
descdist(as.numeric(ts(c)), boot = 1000)##25-day, 1.875 day differenced does not fit into any distribution 

#fw = fitdist(serv, "weibull")
#class(fw)
#summary(fw)

#fg = fitdist(serv, "gamma")
#fln = fitdist(serv, "lnorm")
#t-test example
mean(as.numeric(ts(c)))
sd(as.numeric(ts(a)))
t.test(as.numeric(ts(a)),mu=0, conf.level = 0.95)
wilcox.test(as.numeric(ts(a)), mu=0, conf.level = 0.95)


library("metRology")
mean(ts(c))
sd(ts(c))
ft = fitdist(dput(as.numeric(ts(a)),""),"t.scaled",start=list(df=5000,mean= -7.146551e-19,sd=0.3496666))
ft

fitdistr(cad_1m_diff_25, "t", start = list(m=-7.146551e-19,s=0.3496666, df=5000), lower=c(-1, 0.001,1))

library(fitdistrplus)
fit <- fitdist(as.numeric(cad_1m_diff_25[,1]), "t", start=list(df=2))
summary(ft)
qqcomp(list(ft))

library(VarianceGamma)
fit<-vgFit(ts(b), freq = NULL, breaks = NULL, paramStart = NULL,
      startMethod = "Nelder-Mead", startValues = "SL",
      method = "Nelder-Mead", hessian = FALSE,
      plots = TRUE, printOut = TRUE,
      controlBFGS = list(maxit = 200),
      controlNM = list(maxit = 1000), maxitNLM = 1500)
print(x, digits = max(3, getOption("digits") - 3))
summary(fit)

fc= fitdist(as.numeric(ts(c)), "cauchy")
summary(fc)
fn= fitdist(as.numeric(ts(c)), "norm")
class(fn)
summary(fn)
flo= fitdist(as.numeric(ts(c)), "logis")
summary(flo)
MAX=max(ts(b))
MIN=min(ts(b))
news<- ((as.numeric(ts(b))-MIN+0.001)/(MAX-MIN+0.002))
news

fb= fitdist(news, "beta")
class(fb)
summary(fb)

hist(ts(c), breaks = 180)
#den
denscomp(fc,plotstyle="graphics", main="denscomp Version")
#hist(ts(c),freq=F, breaks = 180)
#par(mfrow = c(2, 2))
plot.legend <- c("normal", "logistic", "cauchy")
#plot.

denscomp(list( fn,flo,fc),  legendtext = plot.legend)
denscomp(fc,  legendtext = "beta")
#denscomp(fn,  legendtext = "beta")
qqcomp(list( fn), legendtext = "normal")
qqcomp(list( flo), legendtext = "logistic")
qqcomp(list( fb), legendtext = "beta")
qqcomp(list(ft), legendtext = "t")
qqcomp(list( fc), legendtext = "cauchy")
#qqcomp(list(fit),legendtext = "VG")
cdfcomp(list( fn, flo,fc), legendtext =  plot.legend)
cdfcomp(fb,legendtext = "beta")
cdfcomp(ft,legendtext = "t")
ppcomp(list(fn), legendtext = "normal")
ppcomp(list(flo), legendtext = "logistic")
ppcomp(list(ft), legendtext = "t")
ppcomp(list(fb), legendtext = "beta")
ppcomp(list(fc), legendtext = "cauchy")


quantile(fn, probs = 0.05)
quantile(flo, probs = 0.05)
quantile(fn, probs = 0.9975)
quantile(flo, probs = 0.9975)
gofstat(list( fn, flo),
        fitnames = c("Normal", "Logistic"))
gofstat(fb,
        fitnames = c("beta"))
gofstat(fc, fitnames = 'cauchy')

##parametric bootstrap
bendo.n = bootdist(fn, niter = 1001)
summary(bendo.n)
bendo.l = bootdist(flo, niter = 1001)
summary(bendo.l)
bendo.b = bootdist(fb, niter = 1001)
summary(bendo.b)
plot(bendo.n)
plot(bendo.l)
plot(bendo.b)

# non-parametric bootstrapping
bendo.n.np = bootdist(fn, bootmethod="nonparam", niter = 1001)
summary(bendo.n.np)
plot(bendo.n.np)
bendo.l.np = bootdist(flo, bootmethod="nonparam", niter = 1001)
summary(bendo.l.np)
plot(bendo.l.np)
bendo.b.np = bootdist(fb, bootmethod="nonparam", niter = 1001)
summary(bendo.b.np)
plot(bendo.b.np)

# quantile estimation
quantile(a, probs = 0.05)
quantile(fb, probs = 0.05)
quantile(bendo.b, probs = 0.05)
quantile(bendo.b.np, probs = 0.05)

quantile(a, probs = 0.05)
quantile(flo, probs = 0.05)
quantile(bendo.l, probs = 0.05)
quantile(bendo.l.np, probs = 0.05)



