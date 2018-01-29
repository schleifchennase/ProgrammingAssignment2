## The functions take a matrix and solve compute its inverse if the inverse
## of this matrix has not already been cached

## The makeCachematrix function creates a set of functions that caches a matrix,
## makes it possible to retrieve this matrix, caches its inverse and makes it
## possible to retrieve its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<-NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list (set=set, get=get, setinv=setinv, getinv = getinv)
}


## This functions checks, if already the inverse of the matrix has been 
## computed. If it has been already computed it retrieves the the inverse
## from the cache. If not, it computes the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(x,...)
        x$setinv(i)
        i
}
