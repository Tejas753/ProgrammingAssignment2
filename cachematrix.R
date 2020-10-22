## Catching the inverse of the matrix


##The first function, makeCacheVector creates a special "matrix",
##which is really a list containing a function to:
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
     m<-NULL
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- solve
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function computes the inverse of special 'matrix' returned by makeCaxhematrix
##if the above inverse is already calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
     

