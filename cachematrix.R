## This project saves a matrix, with the function makeCacheMatrix,
## in a form that when the inverse is solved for, with the function cacheSolve,
## the output is returned from the cache instead of being calculated again.

## makeCacheMatrix creates a list of functions. These functions are:
## 1. get - retrieve the matrix
## 2. setInv - set the Inverse of the matrix
## 3. getInv - retrieve the inverse from the cache
## 4. set - set the matrix initially

makeCacheMatrix <- function(x = matrix()) {
  Inv<- NULL
  set<- function(y) {
    x<<-y
    Inv<- NULL
  }
  get <-function() x
  setInv<- function(solve) Inv<<- solve
  getInv<- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve uses the list created in makeCacheMatrix. If the Inverse of the
## matrix has been solved for already, the Inverse is retrieved from the cache
## and a message is displayed. If the inverse has not yet been calculated, the 
## inverse is calculated and saved in the cache and returned.

cacheSolve <- function(x, ...) {
  Inv<- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
  data<-x$get()
  Inv<-solve(a=data,...)
  x$setInv(Inv)
  Inv
}
