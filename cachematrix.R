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
  ## the next four sets are the functions that inhabit this environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  ## populates the list that will associate with 
  ## 'x=matrix' in the specific environment
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check cached value of x$getinverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## calls get function in x env
  data <- x$get()
  ## calls the defined 'solve' function in x env using prev data
  m <- solve(data, ...)
  ## calls setinverse which applies solve to the data
  ## in initial call to makeCacheMatrix(x)
  x$setinverse(m)
  m
}
