## The below two functions cache the inverse of a matrix
## This is submitted as a part of the R assignment at Coursera
## Sumbitted by Madhumitha Mohan

## makeCacheMatrix funtion return a list containing funtions to set/get/setinverse/getinverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve funtion calculates the inverse of the matrix. It verifies if the inverse has already been calculated.
## If calculated previously, it fetches the value from cache and returns it without computing again.
## Else, it computes the inverse and returns it.

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