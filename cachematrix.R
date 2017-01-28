##R Assignment- Week 3- Caching the Inverse of a Matrix
##This function creates a list with the set and get functions for both input matrix x and its inverse matrix Inv.
makeCacheMatrix<-function(x=matrix())  #Initialized the makeCacheMatrix() with x parameter
  {
  n<-nrow(x)                           ##n is no. of row of x. Inv should have similar dim as x and since x should be a square matrix, nrow=ncol
 Inv<-matrix(data=NA,nrow=n,ncol=n)    ##NA value initialized for the inverted matrix
 set<-function(y=matrix(),n) 
 {
   x<<-y
   Inv<<-matrix(data=NA,nrow=n,ncol=n)}
 get<-function() x
setinverse<-function(inv_matrix) Inv<<- inv_matrix
getinverse<-function() Inv
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
##This function calculates inverse of the input matrix. If object exists for the same value of x, the cached value is displayed.
cacheSolve<-function(x){
  Inv<-x$getinverse()                 
  if (!is.na(Inv[1]))                   ##Checking only 1st value of the matrix for NA, since if one value is NA all will be NA, as initialized
  {print("Getting cached data")
  return(Inv)}
  M<-x$get()
  Inv<-solve(M)
  x$setinverse(Inv)
  Inv
}
