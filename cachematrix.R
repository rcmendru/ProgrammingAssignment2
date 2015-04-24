## Put comments here that give an overall description of what your
## functions do
## The functions in this script provide inverse of a matrix, that is cached.

## Write a short comment describing this function
## This function takes a matrix as an argument and returns a list of four functions
## to get and set the matrix to be inversed and get and set the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) invx <<- inv
        
        getinv <- function() invx
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function takes cachematrix as an argument and returns the inverse of the matrix
## If the inverse matrix was already cached, the cached matrix would be returned
## If the inverse matrix was not already cached, it would be computed and returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data)
        x$setinv(invx)
        invx
}
