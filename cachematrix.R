## Put comments here that give an overall description of what your
## functions do

## Creating the Matrix with cached inverse

makeCacheMatrix <- function(x = matrix()) {

	cachedMatrix <- NULL
	set <- function(y) {
               x <<- y
               cachedMatrix  <<- NULL
      }
      get <- function() x
      setInverse <- function(matrixInverse) cachedMatrix <<- matrixInverse
      getInverse <- function() cachedMatrix 
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
	
}


## Calling the makeCacheMatrix 
cacheSolve <- function(x, ...) {
        calculatedInverse <- x$getInverse ()
        if(!is.null(calculatedInverse )) {
                message("getting cached data")
                return(calculatedInverse )
        }
        data <- x$get()
        calculatedInverse  <- solve(data)
        x$setInverse(calculatedInverse )
        calculatedInverse 
}
