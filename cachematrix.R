# Caching the inverse of a matrix
# '<<-' is used to assign a value to the object in the parent environment

# makeCacheMatrix function defined below makes a special kind of a "matrix".
# This "matrix" is really a list that does the following things:
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the given matrix
# 4. gets the value of the inverse of the matrix
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
# After making the special "matrix" of makeCacheMatrix type, you can set the value of matrix by using $set()
# myMatrix<-makeCacheMatrix() -- This initializes your myMatrix, which has the capability to cache its inverse
# myMatrix$set(matrixM) -- This sets the vaule of matrix as matrixM, for which we need to cache the inverse



# cacheSolve function defined below calculates the inverse of the special "matrix" of makeCacheMatrix type
# it first checks if the inverse has been calculated already. If so, skips the computation and gets the inverse
# Otherwise calculates the inverse and sets it in the cache for quick future retreival
cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
