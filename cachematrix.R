## These functions speed up calculating inverse matrices by caching them.

## This function will take a matrix as input and return a list containing 
## a function to set a new matrix, a function to return the matrix, a function 
## to set the inverse matrix and a function to return the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
		s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function will take the "cached" matrix from the makeCacheMatrix function
## and if the inverse matrix is not yet calculated, will calculate the inverse and 
## else will return the cached inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
