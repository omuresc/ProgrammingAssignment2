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
    inv <- NULL
    set <- function(y) 
    {
        x <<- y     # the <<- operator assigns a value to an object
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##
# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function. This function assumes that the matrix is always invertible.
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
    inv <- x$getinverse()
    if(!is.null(inv)) 
    {
        message("Inverse matrix is already in memory...")
        return(inv)
    }
    message("Inverse matrix is not in memory. Computing if exist...")
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
