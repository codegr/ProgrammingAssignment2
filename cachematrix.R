## This program creates functions for computing the inverse of a matrix and
## caching the result. If the inverse of a matrix is requested for a second
## time, it will be fetched from the "cache" instead of being calculated
## again.
## We consider that the matrix that is given as input is always inversible.

## The makeCacheMatrix function takes as input a matrix and returns a list, 
## with four nested (internal) functions as members. The "invCache" variable
## is the "cache" that stores the inversed matrix. This variable is located 
## in the parent environment of the getInverse function, i.e we make use
## of the "function closure" feature of R.

makeCacheMatrix <- function(x = matrix()) {
    invCache<-NULL
    
    set <- function(y) {
        x <<- y
        invCache <<- NULL
    }  
    
    # for enhanced readability, I am not following the function definition 
    # style (everything in one line) that is used in the assignment's example
    get <- function(){
        x
    }
    
    setInverse <- function(inverse) { 
        invCache <<- inverse
    }
    
    getInverse <- function() { 
        invCache
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Returns a matrix that is the inverse of 'x'.
## The cacheSolve function takes as input a cached matrix, i.e. an object 
## (list) that has been returned by makeCacheMatrix. It checks if the 
## inverse has been cached and if not, it calculates the inverse with the 
## solve function and caches it in the invCache variable

cacheSolve <- function(x, ...) {
    
    invCache<-x$getInverse()
    if(!is.null(invCache)){
        message("getting cached data")
        return(invCache)
    }
    data<-x$get()
    invCache<-solve(data, ...)
    x$setInverse(invCache)
}

## test function. We use 2 matrices and show that the cache is used 
## after the first time we ask for the inversed
test <- function(){
    testMatrix1 <- matrix( c(1,2,5,7,9,5,15,2,5), nrow=3, ncol=3)
    testMatrix2 <- matrix( c(1,2,5,7,9,5,15,2,45), nrow=3, ncol=3)
    
    testCachedMatrix1<-makeCacheMatrix(testMatrix1)
    testCachedMatrix2<-makeCacheMatrix(testMatrix2)
    
    message("1st run")
    print(cacheSolve(testCachedMatrix1))
    message("2nd run - will be fetched from cache")
    print(cacheSolve(testCachedMatrix1))
    message("3rd run")
    print(cacheSolve(testCachedMatrix2))
    message("4th run - will be fetched from cache")
    print(cacheSolve(testCachedMatrix2))
    message("5th run - will be fetched from cache - it still exists although we calculated a second inverse")
    print(cacheSolve(testCachedMatrix1))
    
}





