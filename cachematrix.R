## The following functions create a special "matrix" that has the ability to cache the value of the inverse of the "matrix"

## Create a special "matrix' that is really a list containing functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse (using Solve)
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function(){
        x
    }
    set_inverse <- function(inverse){
        inv <<- inverse
    }
    get_inverse <- function(){
        inv
    }
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## Calculate the inverse of the "matrix"
## First, check if the inverse has been calculated, if not, calculate the value

cacheSolve <- function(x, ...) {
    inv <- x$get_inverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$set_inverse(inv)
    inv
}
