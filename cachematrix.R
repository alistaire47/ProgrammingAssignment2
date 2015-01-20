## The first of this pair of functions allows the creation of an object in which
## to cache the inverse of a matrix, and a function to evaluate such objects.
## When first called, it will evaluate and store the inverse of the matrix;
## subsequent calls will return the cached inverse instead of recomputing it.
## For repeated calls, cacheSolve() will evaluate more quickly than solve().

## makeCacheMatrix(x = matrix()) is a function that takes a matrix, x, and
## creates an object in which to store the inverse of a matrix. It returns a
## list of functions to set() and get() the matrix and get and set the inverse
## (setinverse() and getinverse()).

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set,
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## cacheSolve(x) is a function that takes an object, x, created by the
## makeCacheMatrix() function, and caches (on the first call) and returns (on
## all calls) the inverse of the matrix with which the object was created.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
