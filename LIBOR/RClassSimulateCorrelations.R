rm(list = ls())
zoo.rates <- as.zoo(sasha)
tsRainbow <- rainbow(ncol(zoo.rates))
A=res2    

##A comes from actual correlation matrix of original data 12 by 12 package Hmisc
A 

L= t(chol(A))
L
L%*%t(L)
#U= array(0,c(1000,12))
#for (i in 1:12) U[i,]= rnorm(1000,0,1)

#k<-vector()
#U=array(0, c(251,12))
#for (i in 1:12) U[,i]= sde.sim ( model = "OU" ,X0=0, theta = c(-0.0059613,4.3134577,0.1956697) , N =250 , delta =1/250)
#U
m
t
adj_t=rnorm(250,t/2,0.009)
p=array(0,c(251,12))
for (i in 1:12){
  for (j in 1:251){p[,i][j]<-rlaplace(250,t,0.01)}}

l=array(0,c(1001,12))
for (i in 1:12){
  for (j in 1:1001){l[,i][j]<-rlaplace(1000,m,abs(adj_t))}
}
l

U=array(0, c(1001,12))
k<-array(0, c(1001,12))
for (i in 1:12) {U[,i]=(sde.sim ( model = "OU" ,X0=0, theta = c(0.00434613, 1.68049081, 0.05577446) , N =1000 , delta =1))
  #for (j in 1:1001){k[,i][j]<-(cumsum(U[,i][j]+j*mean(X)))+l[,i][j]}}
  for (j in 1:1001){k[,i][j]<-U[,i][j]}}
U
k
k=t(k)
cor(t(k),t(k))
cor_version_ofU=L%*%k
cor_version_ofU
j=t(cor_version_ofU)
#for (i in 1:12)
{windows(20,10)
#for (i in 1:12)
#plot(ts(j[,1:12]))

zoo::plot.zoo(ts(j), screens = 1, xlab="Time (Days)",ylab="Interest Rate in %",las=2, main="Forecasted LIBOR Under Moderate Shock", plot.type = "single",col =tsRainbow)
grid()
legend(x="bottomleft", legend=colnames(sasha[,1:12]), lty=2, cex=.64,bty="n",col =tsRainbow)}
##Below, Checking the computation was right:
cor(t(cor_version_ofU),t(cor_version_ofU))


## instead of the rnorm, use the ditr and parameters that represent each tenor
##windows(20,10)
##for (q in 1:12){windows(20,10)
  ##plot(cumsum(j[,q]))}


