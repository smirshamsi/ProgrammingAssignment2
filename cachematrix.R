## This file contains two main functions which take a matrix 
##and then prints out its inverse.


## This function takes a matrix as input, initiates the matrix 
##variable and its inverse and returns a list with input and output variables 
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) i <<- inverse
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function takes a list as input, checks if the inverse is defined; 
## if "yes", prints the inverse from cache, 
## if "no" calculates the inverse and then prints it out. 
cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInv(i)
        i
}
