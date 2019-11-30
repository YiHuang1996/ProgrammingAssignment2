## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        ## return: a list containing functions to
        ##              1. set the matrix
        ##              2. get the matrix
        ##              3. set the inverse
        ##              4. get the inverse
        ##         this list is used as the input to cacheSolve()
        m.inv <- NULL
        set <- function(y) {
                x <<- y
                m.inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) m.inv <<- inverse
        getinv <- function() m.inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## Return a matrix that is the inverse of 'x'
        m.inv <- x$getinv()
        if(!is.null(m.inv)){
                 message("getting cache data")
                 return(m.inv)
        }
        data <- x$get()
        m.inv <- solve(data, ...)
        x$setinv(m.inv)
        m.inv
}
