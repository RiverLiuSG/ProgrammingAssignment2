## Description of makeCacheMatrix() function
## It creates the object of matrix with following 4 functions
## set(), get (), setinv(), getinv()
## set: set the value of matrix
## get: get the value of matrix
## setinv(): set the value of the inverse of matrix
## getinv(): get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y                         
    i <<- NULL
  }
  get <- function() x               
  setinv <- function(inv) i <<- inv 
  getinv <- function() i            
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Description of cacheSolve() function
## It stores the inverse of matrix into cache "i"
## if it's already available we could simply retrieve it
## otherwise we should use solve() function to get the inverse matrix and store it into the cache

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {    
    message("getting cached data")
    return(i)          
  }
  data <- x$get()
  i <- solve(data, ...)   
  x$setinv(i)
  i                       
}
