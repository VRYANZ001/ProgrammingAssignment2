## Cache matrix able to invert, invert the matrix 


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())  {
  n <- NULL
  set <- function(z) {
    x <<- z
    n <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) n <<- solve
  getinverse <- function() n
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix


cacheSolve <- function(x, ...) {
  n <- x$getinverse()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinverse(n)
  n
}
