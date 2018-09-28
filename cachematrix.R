## these functions store an input matrix and its inverse in global variables, and can decide whether
## the inverse of a matrix needs to be computed or can be retrieved from cache.

## this function stores the input and inverse matrices.
## store the output of this function in a variable, then feed this variable into
## the cacheSolve function.
## usage example:
## mat <- matrix(1:4, 2, 2)
## special <- makeCacheMatrix(mat)
## cacheSolve(special)
## repeating cacheSolve(special) will result in retrieving cached data

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                         ## clear the cache variable
  ## "set" subfunction stores the input matrix globally in x and clears the cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x               ## subfunction retrieves the stored input matrix
  setinv <- function(inv) m <<- inv ## store matrix inverse in cache, computed by cacheSolve
  getinv <- function() m            ## retrieve cached inverse that has been computed by chacheSolve
  ## list of named subfunctions within makeCacheMatrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## this function computes the inverse matrix or retrieves it from cache

cacheSolve <- function(x, ...) {
  m <- x$getinv()       ## copies the result of the getinv() function from the "special matrix" list
  if(!is.null(m)) {
    ## if the content of the cache is not empty, then this cache is printed
    message("getting cached inverse matrix...")
    return(m)           ## return cached inverse
  }
  ## retrieve input matrix in case no cached data available (uninterrupted by return)
  newmat <- x$get() 
  m <- solve(newmat)    ## caculate inverse
  x$setinv(m)           ## store newly calculated inverse in cache
  m                     ## returns newly cached inverse
}
