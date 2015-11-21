## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(mat = matrix()) {
        inv_mat <- NULL
        set <- function (y) {
                mat <<- y
                inv_mat <<- NULL
        }
        get <- function() mat
        setinv <- function(inv) inv_mat <<- inv
        getinv <- function() inv_mat
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_mat2 <- x$getinv()
        if (!is.null(inv_mat2)) {
                message("getting cached data")
                return(inv_mat2)
        }
        data <- x$get()
        inv_mat2 <- solve(data)
        x$setinv(inv_mat2)
        inv_mat2
        ## Return a matrix that is the inverse of 'mat'
}
