N=10
##N is the repeat time of experiment
x=rnorm(N,mean=0,sd=1)
## x is the sample vector
x_bar=mean(x)
## x_bar is the sample mean 
S_x=sqrt(sum((x-x_bar)^2)/(length(x)-1))
## S_x is the sample variance
## you can also use S_x=sd(x) to calculate the sample variance
alpha=0.95
##alpha is the confidence level of the interval estimation
S_bar_x=S_x/sqrt(N)
## the sample variance of x_bar is smaller than that of x.
v=N-1
##use t distribution to calibrate the estimated interval
##v is the degree of freedom of t distribution
alpha_p=alpha+(1-alpha)/2
##alpha_p is the probability P(T<calibrated_coefficient)
##where T~t(df=v)
calibrated_coefficient=qt(alpha_p,df=v)
##qt the quantile function of t distribution with degree of freedom v
##we have confidence level alpha that
##E(x) falls into interval
##(x_bar-calibrated_coefficient*S_bar_x,x_bar+calibrated_coefficient*S_bar_x)

##to verity this statement numerically, we can repeat the above rountine for
##a large number of times and calculate the proportion that 0=E(x) falls
##into that interval
proportion=0;
for(i in 1:1000){
x=rnorm(N,mean=0,sd=1);
left_interval_point=mean(x)-sd(x)*calibrated_coefficient/sqrt(N)
right_interval_point=mean(x)+sd(x)*calibrated_coefficient/sqrt(N)
if(left_interval_point<0&&right_interval_point>0){proportion=proportion+1}
}
##I run the above control flow and get proportion/1000=0.96;
##very near alpha=0.95.
##from this experiment we can know that from single N times experiment we can 
##not even guarantee that E(x) falls into [left_interval_point,right_interval_point]
##sometimes we are so unlucky to encounter the 5% special cases.