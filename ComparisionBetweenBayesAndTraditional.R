##Generating 01 Sequence
##sequence length: N
N=1000
x=c(1)
for(i in 1:(N-1)){
a=runif(1)
if(a<0.95){x=c(x,x[i])
}else{x=c(x,1-x[i])}
}
##Generating Testing Sequence from x
x1=array(seq(length=4*N,from=0,to=0),dim=c(N,4))
for(i in 1:N){
x1[i,]=rnorm(4,mean=x[i])
}


##traditional testing method
pValue_diff=0
x2=array(seq(length=42,from=0,to=0),dim=c(14,3))
for(j in 1:14){
error_Type_I=0;
error_Type_II=0;
pValue_diff=pValue_diff+0.05
for(i in 1:N){
pValue=2*(1-pt(abs(mean(x1[i,])/sd(x1[i,])),df=3))
if(pValue<pValue_diff && x[i]==0){error_Type_I=error_Type_I+1}
##Type I Error And Type II Error
if(pValue>pValue_diff && x[i]==1){error_Type_II=error_Type_II+1}
}
x2[j,]=c(error_Type_I,error_Type_II,error_Type_I+error_Type_II)
}
##Bayes testing method
error_Type_I=0;
error_Type_II=0;
for(i in 1:N){
if(mean(x1[i,])>0.5 && x[i]==0)
{
error_Type_I=error_Type_I+1;
}
if(mean(x1[i,])<0.5 && x[i]==1)
{
error_Type_II=error_Type_II+1
}
}
c(error_Type_I,error_Type_II,error_Type_I+error_Type_II)