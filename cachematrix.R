# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly. This 
# pair of functions can cache the inverse of a matrix.

# Take an invertible matrix as input, return a list that contains 4 functions
# (1.set the matrix; 2.get the matrix; 3.get the inverse of the matrix; 4.set
# the inverse of the matrix).
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    SetInverseMatrix <- function(inv) inv <<- inv
    GetInverseMatrix <- function() inv
    list(set = set, get = get,
         SetInverseMatrix = SetInverseMatrix,
         GetInverseMatrix = GetInverseMatrix)
}

# Take a list created with the makeCacheMatrix function as input. Check if the
# inverse of the matrix has been cached, if so, it gets the inverse of the matrix
# from the cache, print "getting cached data" then return it. Otherwise, it
# calculates the inverse of the matrix and set the result in the cache,then it.
cacheSolve <- function(x, ...) {
    inv <- x$GetInverseMatrix()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$SetInverseMatrix(inv)
    inv
}
