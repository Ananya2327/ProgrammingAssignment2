## In order to demonstrate caching in R that saves computational time, I will be creating two functions makeCacheMatrix and cachesolve that will 
## take matrix as an input and calculate its inverse, if the inverse has already been calculated then the inverse will be 
##retrieved from the cache.

## makeCacheMatrix will create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m_inv <- NULL
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m_inv <<- solve
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## cacheSolve will compute the inverse of the special "matrix" returned by makeCachematrix. If the inverse has already
##been calculated then it will retrieve its inverse from the cache.

cacheSolve <- function(x, ...) {
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}
