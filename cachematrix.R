## The makeCacheMatrix and cacheSolve functions below cache and calculate the inverse of a matrix

## The makeCacheMatrix creates a list to:-

##    1. set the value of the matrix (set() function)
##    2. get the value of the matrix (get() function)
##    3. set the value of the inverse of the matrix (setinverse() function)
##    4. get the value of the inverse of the matrix (getinverse() function)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The cacheSolve calculates the inverse of the matrix created in the makeCacheMatrix function

## First the cacheSolve function checks if the inverse of the matrix has already been calculated. 

## If the inverse has already been calculated, the computation is skipped and it gets the inverse of the matrix from the cache (getinverse()). 

## If the inverse has not already been calculated, the inverse of the matrix is calculated and set in the cache (setinverse()). 

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
