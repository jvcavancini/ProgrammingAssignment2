## This script is a method to save matrix and its inverse in the memory, so you can access it without needing to
## recalculate
## Just put your matrix in makeCacheMatrix function and use cacheSolve() in your matrix

## This function creates an object with a matrix and its inverse
## Returns a list in the end

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(m_inv) inv<<-m_inv
  getinverse<-function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function is equivalent to solve() for a matrix, but it stores the solution of your matrix to take less
## computation

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(is.null(inv)){
    m<-x$get()
    inv<-solve(m,...)
    x$setinverse(inv)
    return(inv)
  }
  else {
    message("getting cached data")
    return(inv)
  }
}
