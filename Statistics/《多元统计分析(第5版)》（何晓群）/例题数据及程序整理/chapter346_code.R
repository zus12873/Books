##############   例3-4计算样本距离
a1=c(6901.6,2321.3,4632.8,1558.2,3447.0,3018.5,2313.6,802.8)
a2=c(8467.3,1903.9,7385.4,1420.7,5100.9,3452.3,1691.9,645.3)
a3=c(5067.7,1746.6,3753.4,1430.2,1993.8,2078.8,1524.5,492.8)
a4=c(5777.3,1776.9,3752.6,1329.1,2517.9,2322.1,1583.4,479.9)
a5=c(5975.7,1963.5,3809.4,1322.1,3064.3,2352.9,1750.4,614.9)
X=rbind(a1,a2,a3,a4,a5)
D=matrix(NA,nrow=5,ncol=5)
for (i in 1:5){
  for(j in 1:5)
  {
    D[i,j]=sqrt(t(X[i,]-X[j,])%*%(X[i,]-X[j,]))
  }
}
D
#######################


########  3.7.3 模糊聚类法
library(foreign)
mydata<-read.spss("3.7.1Asia.sav") 
#读取E盘下变量名为3.7.1Asia.sav的SPSS原始数据
X<-as.data.frame(mydata)  #转换数据格式
Z<-data.frame(scale(X[,2:7]),row.names=X[,1]) #对数据进行标准化，并将各行命名为相应的国家
library(cluster)
fresult<-fanny(Z,3)
summary(fresult)
plot(fresult)
##########################


##############  例4-3计算Fisher线性判别函数
library(foreign)
mydata<-read.spss("例3-5.sav")
data<-as.data.frame(mydata)
a1<-which(data[,1]=="北京    ")
a2<-which(data[,1]=="上海    ")
b1<-which(data[,1]=="广东    ")
b2<-which(data[,1]=="西藏    ")

x1<-data[a1,]
x2<-data[a2,]
x<-rbind(x1,x2)

y1<-data[b1,]
y2<-data[b2,]
y<-rbind(y1,y2)

others<-data[-c(a1,a2,b1,b2),]

xbar<-colMeans(x[,2:9])
tbar<-colMeans(others[,2:9])

xbar-tbar
xbar+tbar
sigmax<-cov(x[,2:9])
sigmat<-cov(others[,2:9])
sigma<-(1/27)*(sigmax*1+sigmat*26)
isigma<-solve(sigma)

(xbar-tbar)%*%isigma #判别函数
(xbar-tbar)%*%isigma%*%(xbar+tbar)/2#中点
(xbar-tbar)%*%isigma%*%(xbar-tbar)#马氏距离

xrd<-as.matrix(x[,2:9])
trd<-as.matrix(others[,2:9])
yrd<-as.matrix(y[,2:9])

yx<-(xbar-tbar)%*%isigma%*%t(xrd)
yt<-(xbar-tbar)%*%isigma%*%t(trd)
yy<-(xbar-tbar)%*%isigma%*%t(yrd)
################################################

##############  例4-2
x<-read.csv("例4-2.csv")
x<-x[,-1]
a<-as.matrix(x[1:6,2:5])
mua<-as.matrix(colMeans(a))
b<-as.matrix(x[7:14,2:5])
mub<-as.matrix(colMeans(b))
c<-as.matrix(x[15:20,2:5])
muc<-as.matrix(colMeans(c))
sigma<-(cov(a)*5+cov(b)*7+cov(c)*5)/17
invs<-solve(as.matrix(sigma))
invs
#for the first group
onea<-matrix(1,nrow=6,ncol=1)
da1<-(a-onea%*%t(mua))%*%invs%*%t(a-onea%*%t(mua))
da2<-(a-onea%*%t(mub))%*%invs%*%t(a-onea%*%t(mub))
da3<-(a-onea%*%t(muc))%*%invs%*%t(a-onea%*%t(muc))
diag(da1)##第一组的各样本距离组一的距离
diag(da2)##第一组的各样本距离组二的距离
diag(da3)##第一组的各样本距离组三的距离
##根据上面输出结果可以知道第一组的各样本距离哪个组的距离是最小的
#for the second group
oneb<-matrix(1,nrow=8,ncol=1)
db1<-(b-oneb%*%t(mua))%*%invs%*%t(b-oneb%*%t(mua))
db2<-(b-oneb%*%t(mub))%*%invs%*%t(b-oneb%*%t(mub))
db3<-(b-oneb%*%t(muc))%*%invs%*%t(b-oneb%*%t(muc))
diag(db1)
diag(db2)
diag(db3)
#for the third group
onec<-matrix(1,nrow=6,ncol=1)
dc1<-(c-onec%*%t(mua))%*%invs%*%t(c-onec%*%t(mua))
dc2<-(c-onec%*%t(mub))%*%invs%*%t(c-onec%*%t(mub))
dc3<-(c-onec%*%t(muc))%*%invs%*%t(c-onec%*%t(muc))
diag(dc1)
diag(dc2)
diag(dc3)
##for china mainland and aomen
oney<-matrix(1,nrow=2,ncol=1)
y<-as.matrix(x[21:22,2:5])
dy1<-(y-oney%*%t(mua))%*%invs%*%t(y-oney%*%t(mua))
dy2<-(y-oney%*%t(mub))%*%invs%*%t(y-oney%*%t(mub))
dy3<-(y-oney%*%t(muc))%*%invs%*%t(y-oney%*%t(muc))
diag(dy1)
diag(dy2)
diag(dy3)
##########################################################

