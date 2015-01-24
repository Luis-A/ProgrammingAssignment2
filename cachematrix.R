## Put comments here that give an overall description of what your
## functions do

## This function receives a matrix object and returns a list
##    of 4 operations it can be applied to it: get, set, 
##    getInverse and setInverse. The applicatin of the <-- 
##    operator on inv makes easy to reach the same name object 
##    on the parent environment of the calling environmrnt.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
}


## This function returns the inverse of an object x that stores
##    a makeCacheMatrix one. First, it test if this object has a
##    valid inverse value already computado. In this case, it 
##    returns that value. In other case, it calculates the inverse
##    of the data stored in x, stores the result in x and return
##    this just computed value.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
