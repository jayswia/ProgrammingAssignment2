## Matrix inversion is usually a costly computation and there may be some benefit to caching the 
## inverse of a matrix rather than computing it repeatedly. Below are two functions that address this.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) im <<- inverse
        getinverse <- function() im
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
