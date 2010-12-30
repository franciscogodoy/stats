
fulldata<-read.csv("iris.data",header=F)
data<-as.matrix(fulldata[,1:4])
mean = c(mean(data[,1]),mean(data[,2]),mean(data[,3]),mean(data[,4]))
meandata = matrix(c(rep(mean[1],150),rep(mean[2],150),rep(mean[3],150),rep(mean[4],150)),150,4)

D = data - meandata
S = t(D) %*% D
U = eigen(S)$vectors
Dp = D %*% U

varValues = c(var(Dp[,1]),var(Dp[,2]),var(Dp[,3]),var(Dp[,4])

for(row in 1:150)
{
    if (fulldata[row,5] == "Iris-setosa")
    {
        points(Dp[row,1], Dp[row,2], pch=16, col="red")
    }
    if (fulldata[row,5] == "Iris-virginica")
    {
        points(Dp[row,1], Dp[row,2], pch=16, col="green")
    }
    if (fulldata[row,5] == "Iris-versicolor")
    {
        points(Dp[row,1], Dp[row,2], pch=16, col="blue")
    }
}

