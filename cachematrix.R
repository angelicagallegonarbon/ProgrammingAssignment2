#The function "makeCacheMatrix" creates a matrix, removes prior inverse results from the cache, and defines the value of the inverse (the inverse is calculated, or retrieved from the cache when it is already available). Afterwards it creates a list of the previous functions included in the function.
makeCacheMatrix <- function(x = matrix()) {
    a <-  NULL
    set<- function (y) {
        x  <<- y
        a <<- NULL
}
get <- function() x
setinverse <- function(inverse) a <<- inverse
getinverse <- function() a
list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#The function "cacheSolve" gives the value of the inverse. It retrieves it from the cache when it is possible. If not, it calculates the inverse, and returns the inverse calculated.
cacheSolve <- function(x, ...) {
     a <- x$getinverse()
     if (!is.null(a)) {
          message("getting cached data")
          return(a)
  }
  data <- x$get()
  a  <- solve(data, ...)
  x$setinverse(a)
  a
}
