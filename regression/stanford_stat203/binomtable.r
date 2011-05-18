data = read.table("NFL.txt",header=TRUE,sep="\t")
attach(data)

fulldata = matrix(nrow=0,ncol=3)
for(i in 1:nrow(data)){
	XX = rep(Distance[i],Attempts[i])
	ZZ = rep(Z[i],Attempts[i])
	YY = rep(0,Attempts[i])
	YY[1:Success[i]] =1 
	fulldata = rbind(fulldata,cbind(XX,ZZ,YY))
}