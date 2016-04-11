## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## The following functions are used to cache the inverse of a matrix.

## The first function creates a list containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the value of the inverse for the matrix
## d. get the value of the inverse for the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(inverse) {
                x <<- inverse
                inver <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inver <<- inverse
        getinverse <- function() inver
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The following function calculate the inverse of the special "matrix". It 
## checks to see if the inverse has already been calculated. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via 
## the setinverse function.

cacheSolve <- function(x, ...) {
        inver <- x$getinverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data)
        x$setinverse(inver)
        inver
}

## Test run:
## > x <- rbind(c(1,0.5,0.5), c(0.5,1,0.5), c(0.5,0.5,1))
## > m <- makeCacheMatrix(x)
## > m$get()
##      [,1] [,2] [,3]
## [1,]  1.0  0.5  0.5
## [2,]  0.5  1.0  0.5
## [3,]  0.5  0.5  1.0

## Calculate the inverse for the first run
## > cacheSolve(m)
##      [,1] [,2] [,3]
## [1,]  1.5 -0.5 -0.5
## [2,] -0.5  1.5 -0.5
## [3,] -0.5 -0.5  1.5

## Retrieve cached data for the second run
## > cacheSolve(m)
## getting cached data
##      [,1] [,2] [,3]
## [1,]  1.5 -0.5 -0.5
## [2,] -0.5  1.5 -0.5
## [3,] -0.5 -0.5  1.5
