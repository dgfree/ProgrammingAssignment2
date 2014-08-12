## Caches matrix inverse for easier retrieval and 
## uses less memory

## makeCacheMatrix creates the matrix whose
## inverse is cached when set

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get=get, 
		 setinverse = setinverse,
		 getinverse = getinverse)
}


## retrieves the inverse. If the inverse is not stored, calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
        	message("getting cached data")
        	return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}