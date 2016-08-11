## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there are some benefits to cache the inverse of a matrix.
## Below are a pair of function that are used to create a special object that store a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
            I <- NULL
            set <- function(y) {
            x <<- y
            I <<- NULL
            }
            get <- function() x
            setInverse <- function(inverse) I <<- inverse
            getInverse <- function() I
            list(set = set,
                 get = get,
                 setInverse = setInverse,
                 getInverse = getInverse)
}


## Below function computes the inverse of the special "matrix" created by "makeCacheMatrix" above. if the inverse has already been
## calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
         I <- x$getInverse()
         if(!is.null(I)) {
                message("getting cached data")
                return(I)
         }
         mat <- x$get()
         I <- solve(mat, ...)
         x$setInverse(I)
         I
}
