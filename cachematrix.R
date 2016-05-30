## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set=set, get= get, 
         setinverse=setinverse, getinverse=getinverse)
}


## This function calculates inverse of the matrix created using function above.
## It first checks if the inverse has already been calculated,
## if the inverse exists in the cache then it skips the calculation else 
## it calculate inverse and add it to the cached via setinverse function.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data.")
      return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}

## TEST
## > x <- matrix(1:4, 2, 2)
## > m <- makeCacheMatrix(x)
## > m$get()
##        [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > m$getinverse() #Not cached first run, will return NULL 
## NULL
## > cacheSolve(m)
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)  #Will return cached data
## getting cached data.
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5