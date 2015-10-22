##
# The following function creates a special "matrix" object that can:
#   1. set the value of a matrix
#   2. get the value of a matrix
#   3. set the value of inverse of a matrix
#   4. get the value of inverse of a matrix
#
# Arguments: - x: a square invertible matrix
#
# Output: a "matrix" object containing the functions described above
#
# Usage:
#   > m = makeCacheMatrix(rbind(c(1, 2), c(-2, 1)))
#   > m$get()
#            [,1] [,2]
#       [1,]    1    2
#       [2,]   -2    1
#   > m$getinverse()
#   > NULL
#   > y = rbind(c(4, 5), c(-4, 5))
#   > m$set(y)
#   > m$get()
#            [,1] [,2]
#       [1,]    4    5
#       [2,]   -4    5
##

makeCacheMatrix <- function(x = matrix()) 
{
    # stores the cached value and initializes to NULL
    inv <- NULL
    
    # create the matrix in the working environment
    set <- function(y) 
    {
        # the <<- operator assigns a value to an object
        x <<- y     
        inv <<- NULL
    }
    
    # get the value of the matrix
    get <- function() x
    
    # invert the matrix and store in cache
    setinverse <- function(inverse) inv <<- inverse
    
    # get the inverted matrix from cache
    getinverse <- function() inv
    
    # return the created functions to the working environment
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. This function assumes that the matrix is always invertible.
#
# Arguments: - x: a "matrix" object in chache memory created with makeCacheMatrix()
#
# Output: the "matrix" object in cache memory actualized with its inverse 
#
# Usage:
#   > m$set(rbind(c(1,1,1),c(1,-1,1),c(1,1,-1)))
#   > cacheSolve(m)     #first iteration
#   > Inverse matrix is not in memory. Computing if exist...
#             [,1] [,2] [,3]
#       [1,]  0.0  0.5  0.5
#       [2,]  0.5 -0.5  0.0
#       [3,]  0.5  0.0 -0.5
#   > cacheSolve(m)     #second iteration
#   > Inverse matrix is already in memory...
#             [,1] [,2] [,3]
#       [1,]  0.0  0.5  0.5
#       [2,]  0.5 -0.5  0.0
#       [3,]  0.5  0.0 -0.5
##


cacheSolve <- function(x, ...) 
{
    # attempt to get the inverse of the matrix stored in cache
    inv <- x$getinverse()
    
    # return inverted matrix from cache if it exists
    if(!is.null(inv)) 
    {
        message("Inverse matrix is already in memory...")
        return(inv)
    }
    
    # else create the matrix in working environment
    message("Inverse matrix is not in memory. Computing if exist...")
    
    # create a matrix in working environment from the cached data
    data <- x$get()
    
    # make sure matrix is square and invertible
    inv <- solve(data, ...)
    
    # copy the computed inverted matrix from the working environment to the cache
    x$setinverse(inv)
    
    # display inverse matrix in console
    return(inv)
}
