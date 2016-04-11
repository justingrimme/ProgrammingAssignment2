# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        # use `<<-` to assign a value to an object in an environment 
        # different from the current environment. 
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, 
         setinverse=setinverse, 
         getinverse=getinverse)
}


## Return: Inverse of the original matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    # if the inverse exists
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # it does not exist, create the inverse
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
}