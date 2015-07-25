# Matrix inversion is usually a costly computation and there may be some benefit to
# caching the inverse of a matrix rather than compute it repeatedly. 

# To cache the inverse of a matrix, below two functions have been used.
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not 
# changed), then the cachesolve should retrieve the inverse from the cache.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

# Here is the code for makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


# cacheSolve function returns the inverse of the matrix. This function assumes that 
# the matrix is always invertible. Computing the inverse of a square matrix can be 
# done with the solve function in R.

# Here is the code for cacheSolve
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
