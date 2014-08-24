## This function creates Inverse of Invertible Matrix and caches result set such that cache 
## result will be reused for next call instead of calculating again


## This function calculates Inverse of Invertible Matrix and stores the resultset in Cache

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## This function checks if the Matrix Inverse is already cached and resuses if availabe 
## else it calculates using the previous function

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      matrix <- x$get()
      m <- solve(matrix, ...)
      x$setinverse(m)
      m
      
}
