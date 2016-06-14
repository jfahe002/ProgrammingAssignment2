## The two functions are intended to cache inverses of matrices in order to avoid
## wasting time on computations that have already been performed

## This function creates a list of functions that store the matrix being inversed
## and initialize a space for the inverse to be saved in the cache

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
          x <<- y
          inv  <- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, getinv = getinv)
}


## This function actually does the computation for the inverse assuming that
## it isn't already in the cache and then stores it in the cache using the getinv
## function in the list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## In summary, after setting the makeCacheMatrix function to a list variable with the matrix you want to inverse
## as a parameter, the cacheSolve function, with the list variable as a parameter, should either immediately
## output the inverse of the matrix or display the message "getting cached data" before outputting the inverse