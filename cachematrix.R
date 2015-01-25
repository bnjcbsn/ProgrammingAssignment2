## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## create a wrapping function so that results are cached
## when function(s) in the list are called, they operate
## and exist with a static environment
## much credit to
## https://class.coursera.org/rprog-010/forum/thread?thread_id=364#post-2000


## new_matrix_env<- makeCacheMatrix(matrix)
## cacheSolve(new_matrix_env)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- matinv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
