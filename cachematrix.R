## this function creates a list of four functions
## its input is a matrix x

makeCacheMatrix <- function(x = matrix()) {

        # t is the inverse matrix, and is set to NULL
        t <-NULL
        
        # this function sets matrix x, to a new matrix, y, and resets the inverse, t, to NUL
        set <- function(y) {
                x <<- y
                t <<- NULL
                }
        
        # this function returns matrix x
        get <- function() x
        
        # this function sets the inverse, t, to inverse
        setinverse <- function(inverse) t <<- inverse
        
        # this function returns the inverse, t
        getinverse <- function() t
        
        # this statement creates a list of all four functions and names them accordingly
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## this function accepts as input the list of four functions created by the "makeCacheMatrix" function 
## and returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        # t is fetched by the "getinverse" function
        t <- x$getinverse()
        # if t is not NULL, print a message and return matrix t and end
        if (!is.null(t)) {
                message("getting cached data")
                return (t)
        }
        # if t is NULL, fetch matrix x, and compute its inverse 
        data <- x$get()
        t <- solve(data, ...)
        # set t to inverse and print it 
        x$setinverse(t)
        t
}
