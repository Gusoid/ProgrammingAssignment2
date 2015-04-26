## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=as.matrix()){
  ma<-x
  invma<-NULL
  setma<-function(y=as.matrix()){
    ma<<-y
    invma<<-NULL
  }
  getma<-function()ma
  setinv<-function(inv) invma<<-inv #invma<<-solve(ma)
  getinv<-function()invma
  list(setma=setma,getma=getma,setinv=setinv,getinv=getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x,...){
  inv<-x$getinv()
  if(!is.null(inv)){
    message("ya viene")
    return(inv)
  }
  ma<-x$getma()
  inv<-solve(ma)
  x$setinv(inv)
  inv
}