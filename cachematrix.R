## this function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(matrix) m <<- matrix
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)}



## this function computes the inverse of the special matrix returned by makeCacheMatrix. if the inverse has already been calculated, the function cacheSolve should retrieve the inverse from the cache (assuming the matrix hasn't change)

cacheSolve <- function(x, ...) {
 
 m <- x$getmatrix()
 
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
