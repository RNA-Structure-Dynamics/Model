##normal hypothesis testing
N=10
x=rnorm(N)
testing_value=(mean(x)-0)*sqrt(N)/sd(x)
pValue=2*(1-pt(abs(testing_value),df=N-1))
##a small pValues means H1 is true