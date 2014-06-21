## Project 2 for R Programming. Caching a solved matrix.

## Set up a cached matrix where the inverse can also be cached.
## Example: cm <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2)) 
makeCacheMatrix <- function(x = matrix()) {
        solved <- NULL # Initialize the solved variable
        set <- function(y) {
                x <<- y # resets x to be a new value
                solved <<- NULL # resets solved to null
        }
        get <- function() x # return the original matrix
        setSolved <- function(solved) solved <<- solved # set the solved matrix
        getSolved <- function() solved # get the solved matrix
        list(set = set, get = get,
             setSolved = setSolved,
             getSolved = getSolved) # returns all of the enclosed functions
}

## Solve the matrix. Must be passed an instance of makeCacheMatrix
## Example: 
##  cm <- makeCacheMatrix(matrix(1:4, ncol=2, nrow=2)) 
##  cacheSolve(cm)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        solved <- x$getSolved()
        ## If solved is already set, return the cached version
        if(!is.null(solved)) {
                message("getting cached matrix")
                return(solved)
        }
        ## Otherwise, we solve the matrix, setting the cache for next time
        m <- x$get()
        solved <- solve(m, ...)
        x$setSolved(solved)
        solved
}