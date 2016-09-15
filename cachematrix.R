## Put comments here that give an overall description of what your
## functions do

## Function that caches matrix in memory exploiting lexical scoping

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix()
    set <- function(y) {
        x <<- y
        i <<- matrix()
    }
    get <- function() x
    setinverse <- function(inverse) i<<- inverse
    getinverse <- function() i
    list(set = set, 
         get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Function that retrieves cached inverse of matrix if it exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if (!all(is.na(i))) {
        message("Getting cached data")
        return(i)
    }
    myMatrix <- x$get()
    i <- solve(myMatrix, ...)
    x$setinverse(i)
    i
}
