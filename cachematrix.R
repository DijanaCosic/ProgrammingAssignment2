## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
testCacheSolve<-FALSE

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) minv <<- inv
  getinv <- function() minv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
##c<-makeCacheMatrix(matrix(1:4,2,2))
##cat(c$get(),"\n")
#cat(c$getinv())
#cat(c$set(matrix(5:8,2,2)))
#cat(c$get())

# ## This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then cacheSolve should retrieve the inverse 
# from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}

if( testCacheSolve ) {
  d<-makeCacheMatrix(matrix(1:4,2,2))
  e<-cacheSolve(d)
  cat("1 ",d$getinv(),"\n")
  d$set(matrix(5:8,2,2))
  cat("2 ",d$getinv(),"\n")
  e<-cacheSolve(d)
  cat("3 ",d$getinv(),"\n")
}
