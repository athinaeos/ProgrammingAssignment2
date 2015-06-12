## Part I: Cache Matrix
## this function creates a list of four functions
## its input is a matrix x; we assume that x is invertible
## as an example: xx <- makeCacheMatrix(x) is a list with four elements, the four functions defined below

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
## Part II: Retrieve the inverse from the cache if it exists, or compute it and store it in the cache
## this function accepts as input the list of four functions created by the "makeCacheMatrix" function 
## and returns a matrix that is the inverse of 'x'
## as example: cacheSolve(xx), returns the invesrse of matrix x, where xx is the list of the four functions created in Part I.
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
