
##makeCacheMatrix: special "matrix" object that can cache its inverse
##cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 


## makeCacheMatrix: able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialization for the cached inverse data
        m <- NULL
        ## Method to set the matrix
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        ## Method the get the matrix
        get <- function() {
                x
        }
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
                m <<- inverse
        }
        ## Method to get the inverse of the matrix
        getInverse <- function() {
                m
        }
        
        ## Return a list of the methods
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##cacheSolve: If the inverse has already been calculated, the cachesolve should retrieve it from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        ## Return the inverse if exist
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse 
        m <- solve(data,...)
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m
}
