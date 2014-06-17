## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## cache a square matrix 'x' and its inverse.
        im <- NULL
        set <- function(y) {
                if (class(y) == 'matrix' && nrow(y) == ncol(y)) {
                        x <<- y
                        im <<- NULL
                } else {
                        x <<- NULL
                        im <<- NULL
                        stop("It is not a square matrix.")
                }
        }
        get <- function() x
        setinvm <- function(invm) im <<- invm
        getinvm <- function() im
        set(x)
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinvm()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinvm(im)
        im
}
