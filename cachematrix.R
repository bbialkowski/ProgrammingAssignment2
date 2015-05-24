## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
         ##clean out the cache (i.e. set it to NULL)
         m <- NULL
         ##define function to return the original
         get <- function() {
                  x
         }
         ##define function to change the matrix and clear the cache
         set <- function(y) {
                  x <<- y
                  m <<- NULL                  
         }
         getinverse <- function() {
                  m
         }
         ##define function where the inverse of matrix is cached
         inverse <- function(inv) {
                  m <<- inv
                  }
         ##return a list of the functions so they are available
         list(get = get, set = set, getinverse=getinverse, inverse = inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         m <- x$getinverse()
         ##check if already cached. If so, return the inverse from the cache
         if (!is.null(m)){
                  message("getting cached data")
                  return(m)
         }
         ##if not cached, calculate with else function
         else {
              data <- x$get()       ##get the original matrix
              m <- solve(data)     ##calculates the inverse of the matrix
              x$inverse(m)          ##caches the inverse of the matrix
              m                  
              }
}        
                  