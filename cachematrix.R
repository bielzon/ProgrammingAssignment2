## This function creates a special "matrix" object that can cache its reverse.
## It is really a list containing functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    ## 1. Set the value of the matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    ## 2. Get the value of the matrix
    get <- function() x
    ## 3. Set the inverse
    setinverse <- function(inverse) i <<- inverse
    ## 4. Get the inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the function above.
## If the inverse has already been calculated (and the matrix has not changed), then
## this function retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
    ## It first checks to see if the inverse matrix has been already calculated
    i <- x$getinverse()
    if(!is.null(i)) {
        ## If so, it gets the mean from the cache and skips the computation
        message("getting cached data")
        return(i)
    }
    ## Otherwise, it calculates the inverse of the data and sets the value of the inverse
    ## in the cache via the setinverse function
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## It returns the inverse matrix
    i
}
