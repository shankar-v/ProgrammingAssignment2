## The program here computes the inverse of the matrix
## The cached value of the solution is returned if the inverse was
## already calculated. Otherwise, the program calculates the inverse
## and also caches it.

## makeCacheMatrix supports get, set, setinverse, and getinverse 
library(Matrix)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix. If the inverse
## has already been calculated, it just returns the caches value
## Otherwise, it calculates using the solve function and caches and then
## returns the inverted matrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    
}

testMatrixInverse<-function() {
    myMatrix <- matrix(1:4, nrow=2, ncol=2)
    listFuncs <- makeCacheMatrix(myMatrix)
    cacheSolve(listFuncs)
}