## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  inv_cache <-NULL
  set <- function(y){
    m <<- y
    inv_cache <<- NULL
  }
  
  get <- function() m
  
  setinv <- function(inv) inv_cache <- inv
  getinv <- function() inv_cache
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if(!is.null(inv)){
    print("Getting cached inverse.")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
