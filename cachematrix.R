## The functions defined below make a special kind of a matrix which does thw following things.
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of inverse of the given matrix
## 4. gets the value of the inverse of the matrix
## When it calculates the value of the inverse it stores/caches it. If not cached it 
## calculates the inverse of the matrix as a new entity.

## makeCacheMatrix function initializes the matrix and calculates its inverse and stores/caches 
## it using setinverse() function.

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

## This function calculates the inverse of a matrix.
## If already calculated and stored in cache it just displays it instead of calculating it again and again

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
