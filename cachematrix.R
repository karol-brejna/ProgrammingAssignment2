makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
    	return(x)
    }
    setinverse <- function(inverse) {
    	invM <<- inverse
    }
    getinverse <- function() {
    	return(invM)
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    
    return(inv)
}
