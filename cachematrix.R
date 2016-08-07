## Return a matrix that is the inverse of 'x' from cache if already stored or calculate if not

## Creates a special "matrix" that stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of the special "matrix" created by makeCacheMatrix, but first checks to see if it has already been calculated. 
## If so, it gets the inverse and skips the computation.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
