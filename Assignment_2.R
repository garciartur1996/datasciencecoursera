#Creation of two functions to cache the inverse of a matrix (in order to make it more efficient computationally speaking instead of compute it repeteadly

#The "makeCacheMatrix" function has the objective of creating a special matrix object that can cache its inverse

makeCacheMatrix <- function(l = matrix()) {
  m <- NULL
  set <- function(y){
    l <<- y
    m <<- NULL
  }
  get <- function() l
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#This function computes the inverse of x returned by the special matrix in the first function. If the inverse has already been calculated (and the matrix has not changed) then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(l, ...) {
  ## Return a matrix that is the inverse of 'l'
  m <- l$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- l$get()
  m <- solve(mat,...)
  l$setInverse(m)
  m
}