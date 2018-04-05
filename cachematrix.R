## The purpose of these functions is provide a way to cache the lengthy operation
## of computing the inverse of a matrix

## This function constructs the matrix cache object
## The created object contains functions to get the cached version of the inverse
## and to set the cached version of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function returns the inverse of the matrix making use of the cached version
## if it has already been computed, otherwise the inverse is computed and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## Example usage
#A <- matrix( c(5, 1, 0,
#               3,-1, 2,
#               4, 0,-1), nrow=3, byrow=TRUE)

#V <- makeCacheMatrix(A)

#inv1 <- cacheSolve(V)

#inv2 <- cacheSolve(V)

# inv1 & inv2 should be equivalent
# A %*% inv1
# should give identity matrix

