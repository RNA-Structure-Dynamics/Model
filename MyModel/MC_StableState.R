##Markov Chain Stable State
##Transfer Matrix is P=[[1/3,1/3,1/3],[1/2,1/2,0],[1/4,3/4,0]]
##state space E={1,2,3}
state=1

cumulative=array(c(1/3.0,1/2.0,1/4.0,2/3.0,1,1),c(3,2))
state_statistics=c(0,0,0)
for(j in 1:1000){
state=1
for(i in 1:10){
a=runif(1)
if(a<cumulative[state,1]){state=1
}else if(a<cumulative[state,2]){state=2
}else{state=3}
}
state_statistics[state]=state_statistics[state]+1
}
state_statistics=state_statistics/1000.0
##theoretical value for state_statistics=[2/5,7/15,2/15]

