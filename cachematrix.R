## These functions are intended to save computation
## work in the calculation of inverses of matrixes
## (an operation that has a computational complexity 
## higher than O(n^2), so it increases fast with the
## dimension of the matrix)

## They use caching to save the result, so the computation 
## is performed only the first time an inverse of a matrix 
## has to be computed

## Any subsequent calls after the first, use
## the cached value instead of calculating it again

## If there isn't any request for the inverse of the matrix,
## it never gets calculated

## To calculate the inverse of a matrix m follow these steps

## 1. Create a cache object and save it in a variable:
## myMatrix<- makeCacheMatrix(m)

## 2. Calculate the inverse with the cacheSolve function:
## cacheSolve(myMatrix)


## makeCacheMatrix is used to create the object that stores
## the matrix and its inverse. It defines getters and setters
## for the matrix and its inverse

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


## cacheSolve is used to get the inverse of a matrix,
## after an  instance of the makeCacheMatrix object
## was created for that matrix
## It first checks if the inverse is present in the cache
## If that is the case, it retrieves it from the cache,
## otherwise, it calculates it and then saves it in the cache

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
