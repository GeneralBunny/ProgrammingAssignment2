##Return the inverse of a matrix, assuming the matrix is invertible.

## makeCacheMatrix() creates a special "matrix" object that can cache 
##its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y){
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) I <<- Inv
        getInv <- function() I
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


##cacheSolve() returns the inverse of the special "matrix"
##returned by makeCacheMatrix() above. The input argument of cacheSolve() 
##must be the type of makeCacheMatrix()

cacheSolve <- function(x, ...) {
        I <- x$getInv()
        if(!is.null(I)){
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data)
        x$setInv(I)
        I
}
