##makeCacheMatrix function that creates a matrix entity 
##The entity can also store its own inverse to speed up calculations
##Useful in cases that require inverse-matrix on a regular basis

## The four methods (mentioned in the list) should be self explanatory

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse = matrix()) 
        {       m <<- inverse
                m
        }
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve checks if the matrix-entity has an existing inverse
## if the inverse matrix is not available
## it will be calculated and stored in the entity

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
