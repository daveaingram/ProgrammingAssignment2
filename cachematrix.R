## Project 2 for R Programming. Caching a solved matrix.

## Set up a cached matrix where the inverse can also be cached.
makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL
        set <- function(y) {
                x <<- y
                solved <<- NULL
        }
        get <- function() x
        setSolved <- function(solved) solved <<- solved
        getSolved <- function() solved
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved)
}

## Solve the matrix. Must be passed an instance of makeCacheMatrix
## Example: 
##  cm <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2)) 
##  cacheSolve(cm)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solved <- x$getSolved()
        if(!is.null(solved)) {
                message("getting cached matrix")
                return(solved)
        }
        m <- x$get()
        solved <- solve(m, ...)
        x$setSolved(solved)
        solved
}