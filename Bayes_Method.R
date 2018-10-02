setwd('...')
x1<-scan('cy_D1.rt',what=numeric(0),n=1e6)
x2<-scan('cy_D2.rt',what=numeric(0),n=1e6)
x3<-scan('cy_N1.rt',what=numeric(0),n=1e6)
x4<-scan('cy_N2.rt',what=numeric(0),n=1e6)
control<-(x1+x2)/2;
##plot(x=log(control),y=log(x3),pch=20,col='red',ylab='log(case)')
##abline(lm(log(x3)~log(control)),col='blue')
my_coff=lm(log(x3)~log(control))
intercept=my_coff$coefficients[1]
slope=my_coff$coefficients[2]
x_case_1=log(x3)-(intercept+slope*log(control))
x_case_2=log(x4)-(intercept+slope*log(control))
x_case<-c(x_case_1,x_case_2)
#calculate the following data and use them to solve the equation,
#see matlab code ac.m
#mean(x_case)=0.031
#mean(x_case*x_case)=0.508
#mean(x_case*x_case*x_case)=-0.207
#mean(x_case*x_case*x_case*x_case)=0.779


##solve the nonlinear equation system we can get 
mu_1=-0.409
mu_2=0.471
sigma_1=0.5061
sigma_2=0.1209

n=2
## n is the number of sampling per position
my_y=c()
for(i in 1:length(x_case_1)){
xbar=(x_case_1[i]+x_case_2[i])/2;
p1=log(sigma_1)+n*(xbar-mu_1)^2/sigma_1^2;
p2=log(sigma_2)+n*(xbar-mu_2)^2/sigma_2^2;
if(p1>p2){
#reject the null hypothesis,"theta=0", theta=0 represents
##double chain
my_y=c(my_y,1);#1 is single chain
}
else{
my_y=c(my_y,0);#accept theta=0,
}
#0 is double chain
}


## next we compare my_y to the results got by icshape
x<-read.table('cy.icshape')
cut_off_value=0.6;
y=c()
for (i in 1:length(x)){
if(x[[i]]!=x[[1]]){
if(x[[i]]>cut_off_value){y=c(y,1)#1 single
}else{y=c(y,0)}
}
}
single_chain_density=sum(y)/length(y)
print(c(cut_off_value,single_chain_density))
#single_chain density is near
