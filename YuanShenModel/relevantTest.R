##relevant Test
x=c()
y=c()
x_mean=0
##null hypothesis is x_mean=0
N=1000;
##sequence length: N
error_Type_I=0;
error_Type_II=0;
for(i in 1:N){
##x_mean=rbinom(1,size=1,prob=0.5)
##x1=rnorm(4,mean=x[i])
pValue=2*(1-pt(abs(mean(x1[i,])*2/sd(x1[i,])),df=3))
if(pValue<0.15 && x[i]==0){error_Type_I=error_Type_I+1}
##Type I Error And Type II Error
if(pValue>0.15 && x[i]==1){error_Type_II=error_Type_II+1}
}
error_Type_I
error_Type_II
##total_error_cnt\appr=270

x_mean=0

total_error=0
for(i in 3:(N-2)){
x2=c(x1[i,],x1[i-1,],x1[i-2,])
x3=c(x1[i+1,],x1[i,],x1[i-1,])
x4=c(x1[i+2,],x1[i+1,],x1[i,])
pValue2=abs(mean(x2)*sqrt(12)/sd(x2))
pValue3=abs(mean(x3)*sqrt(12)/sd(x3))
pValue4=abs(mean(x4)*sqrt(12)/sd(x4))
judge_parameter=2;
if((pValue2>judge_parameter)+(pValue3>judge_parameter)+(pValue4>judge_parameter)>=2){judge_result=1
}else{judge_result=0}
if(judge_result!=x[i]){total_error=total_error+1}
}
total_error
##Generating 01 Sequence
N=1000
x=c(1)
for(i in 1:N){
a=runif(1)
if(a<0.95){x=c(x,x[i])
}else{x=c(x,1-x[i])}
}

cov(x,y)
xt=c()
for(i in 1:(N/4)){
x1=x[i:(i+3)]
xt=c(xt,mean(x1)*2/sd(x1))
}
##cov(x,y)\appr=0.22
##sorting x and y synchronously to get
##the empirical joint distribution
##this distribution is approximate
library(MASS)
den3d <- kde2d(x, y)
##den3d is the empirical pdf

setwd('D:/BiologyBasic/Reading/YuanShenModel')
x<-read.table('RNA_2.txt')
##0 is single-chain
y=c()
for (i in 1:length(x)){
if(x[i]!=0){y=c(y,1)
}else{y=c(y,0)}
}
x<-read.csv('Book1.csv',header=FALSE,sep=",")
for (i in 1:length(x$V1)){
x$V1[[i]]=y[x$V1[[i]]]
}
error=0
for (i in 1:length(x$V1)){
  if((x$V2[[i]]<0.05 && x$V1[[i]]==1)||((x$V2[[i]]>0.05 && x$V1[[i]]==0))){
	error=error+1}
}
rho=c(1)
N=298
for(i in 1:7){
rho=c(rho,cor(y[1:(N+1-i)],y[(i+1):(N+1)]))
}
st=array(seq(length=48,from=0,to=0),dim=c(24,2))
last=y[1];
cnt=0;
for(i in 2:length(y)){
current=y[i]
cnt=cnt+1
if(current!=last){
if(last==1){st[cnt,1]=st[cnt,1]+1
}else{
st[cnt,2]=st[cnt,2]+1}
cnt=0
}
last=current
}