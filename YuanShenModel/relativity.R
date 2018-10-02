##Generating 01 Sequence
N=1000
p=0.95
x=c(1)
for(i in 1:N){
a=runif(1)
if(a<p){x=c(x,x[i])
}else{x=c(x,1-x[i])}
}
##simulated result
rho=c(1)
for(i in 1:7){
rho=c(rho,cor(x[1:(N+1-i)],x[(i+1):(N+1)]))
}
##theoretical result
rho_t=c(1)
for(i in 1:7){
rho_t=c(rho_t,(2*p-1)^i);
}
##plotting
plot(0:7,rho_t,xlab='position',col='red',ylab='probability')
points(0:7,rho,col='blue')
legend('topright',c('theory','simulation'),col=c('red','blue'),pch=1)
