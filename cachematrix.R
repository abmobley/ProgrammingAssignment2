## These functions create a special matrix capabale of caching its inverse
## and provide access to that cached inverse.

## Creates a special matrix that can cache its inverse
## Returns:
##   A list of the following 4 functions:
##    1. set - sets the value of the matrix
##    2. get - gets the value of the matrix
##    3. setInverse - sets the value of the inverse of the matrix
##    4. getInverse - gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    # set function sets the value of the matrix and sets any previously
    # cached inverse to NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## Evaluates solve function on a special matrix created using the 
## makeCacheMatrix function.

cacheSolve <- function(x, ...) {
    # if the inverse has already been calculated and cached simply return it
    if(!is.null(i)) {
        message("using cached inverse")
        return(i)
    }
    # since inverse is null, get the matrix and calculate the inverse
    m <- x$get()
    i <- solve(m, ...)
    # cache the inverse
    x$setInverse(i)
    # return the calculated inverse
    i
}
