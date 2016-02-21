## These functions are used to demonstrate how variables can be 
## stored outside of the current environment for later use
## These functions create a special object that 
## stores a matrix and caches its inverse.


makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		  x <<- y
		  inverse <<- NULL
	}
	  get <- function() x
	  setsolve <- function(solve)  inverse <<- solve
	  getsolve <- function() inverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve= getsolve)

}

## Use the cached inverted matrix from function above if it exists
## If the inverse has already been calculated use the one in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getsolve()
	  if(!is.null(inverse)){
		   message("getting cached data")
		   return(inverse)
	  }
	  data <- x$get()
	  inverse <- solve(data, ...)
	  x$setsolve(inverse)
	  inverse

}
