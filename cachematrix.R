## These exercise aims to show how an R object can be stored in a cache to reduce time-consuming computations.
## This is done by assigning a value to an object in an environment that is different from the
## current environment using the `<<-` operator.
#
## Two functions are created, the first one to cache the value (inverse matrix) and the second to execute  
## the inverse matrix or retrieve it from the cache if it exists already.

#
## The 'makeCachMatrix' function creates a special "vector", which stores a list containing a to 
## set and get the value of the vector and the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getinverse = getinverse)
}


## 'cacheSolve' function return a matrix of the special "vector", that is the inverse of 'x',
## which was created with the function makeCacheMatrix. It first checks to see if the inverse matrix has already been
## calculated and if so, it retrieves from the cache and skips the computation, which should speed up the computation.
## If the cache is empty, it calculates the inverse matrix of the data and sets the value of the inverse matrix 
## in the cache via the setmatrix function.


cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}
