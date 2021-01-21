## Functions below create a special object that stores a matrix and caches its inverse.

## Function creates a special "matrix" object for caching its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) inv <<- inverse
        
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function computes the inverse of the special "matrix" created by 
## makeCacheMatrix function above. If the inverse has already been calculated and the 
## matrix has not changed, then it should get the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        m <- solve(x$get())
        x$setInverse(m)
}

