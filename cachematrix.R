## The following two functions create cache of inverse of the matrix provided and returns value if same matrix is
## entered again else compute inverse of new matrix and saves in the cache created.

## makeCacheMatrix creates a special list of function which has elements set, get, setinverse, getinverse
## These functions respectively set value of the matrix, retrieves last set value of matrix, sets inverse of the given
## matrix and gets the last computed inverse of the matrix set.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function takes matrix and checks the cache for its inverse. If it is present there, it returns the 
## inverse of the matrix or else computes the inverse of the matrix set

cacheSolve <- function(x, ...) {
        minverse <- x$getinverse()
          if(!is.null(minverse)) {
            message("getting cached data")
            return(minverse)
          }
          data <- x$get()
          m <- solve(data)
          x$setinverse(m)
          m
        ## Return a matrix that is the inverse of 'x'
}
