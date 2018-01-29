#1a
A <- matrix( c(1,5,-2,1,2,-1,3,6,-3),nr=3)
A%*%A%*%A
#1b
A[,3] <- A[,2]+A[,3]
#2
tmp <- matrix(c(10,-10,10), b=T, nc=3, nr=15)
crossprod(tmp)
#3
matE <- matrix(0,nr=6,nc=6)
matE[abs(col(matE)-row(matE))==1 ] <- 1
#4
outer(0:4,0:4,"+")
#5a 
outer(0:4,0:4,"+")%%5
#5b
outer(0:9,0:9,"+")%%10
#5c
outer(0:8,0:8,"-")%%9
#6
A <- matrix(0,nr=5, nc=5)
A <- abs(col(A)-row(A))+1
solve(A,c(7,-1,-3,5,17))
#7a
set.seed(75)
aMat <- matrix( sample(10, size=60, replace=T), nr=6)
apply(aMat,1,function(x){sum(x>4)})
#7b
which(apply(aMat, 1, function(x){sum(x=7) == 2}))
#7c
aMatColSums <- colSums(aMat)
cbind(rep(1:10,rep(10,10)),rep(1:10,10))
#8a
sum(outer((1:20)^4,4:8,"/"))
#8b
sum( (1:20)^4 / (3 + outer(1:20,1:5,"*")))
#8c
sum(outer(1:10,1:10,function(i,j){(i>=j)*i^4/(3+i*j)}))

