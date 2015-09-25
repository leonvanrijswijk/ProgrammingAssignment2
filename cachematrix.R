## With these two functions, you can store the results of a matrix
## inversion in a cache. Redundant inversions are omitted by checking
## the cache before calculating a new matrix inverse.

## makeCacheMatrix allows the user to provide a matrix by taking an 
## optionalargument. The function sets the value for the cached inverse 
## matrix to null and stores the functions set, get, setinverse, and 
## getinverse in a list. The set and setinverse function can be used to 
## set the matrix or the inverse matrix manually.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## cacheSolve first checks for a cached inverse matrix (i).If available,
## the function returns the cached value. If not available, the 
## function solves for the given matrix x.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
