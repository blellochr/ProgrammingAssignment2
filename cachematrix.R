## These functions together solve for the inverse of a matrix and cache the result.
## Cached reset can reset by first re-running makeCacheMatrix with new matrix as argument 
## or entering matrix using x$set function 
## and then rerunning cacheSolve function.

## The makeCachematrix function resets a cached inverse matrix to null and establihses the matrix x used in cacheSolve
## It runs 4 functions including:
## 1. set which allows you to reset matrix
## 2. get which gets matrix (x)
## 3. setinverse which allows entry of a solution for inverse matrix, which comes from running CacheSolve if null
## 4. getinverse, which gets inverse matrix (i)


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


## The cacheSolve function sovlves and returns the inverse of the matrix provided by the makeCacheMatrix 
## unless the inverse already exists, at which point, it just returns cached inverse

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