## Put comments here that give an overall description of what your
## functions do
#Write the following functions:


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse 
#has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Write the following functions:

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
#already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#_______________________________________________________________________________________________________________________________

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## Write a short comment describing this function
#makeMatrix is a function that creates a list of functions: Set, Get, Setinv and Get inv. 
#Get is a simple function that simply stores x. E.g., a <- b, get <- x
#Set is a function that changes for whatever the input is. If we change the input, we use set to show this.
#setinv and getinv are the same as get and set, but with the values calculated by cachesolve. 
#They don't calculate anything, they just store values.
makeMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}
#First of all, cacheSolve assigns locally to m the getinv of the input. This is supposed to be NULL.
#This will only be different of NULL if the matrix has already been calculated. If it's already been
#calculated, it simply prints whats in cache.
#If not, data gets the matrix to be inverted, and m inverts data.
#Then we can print the results, as well as assign set the inverse to x$setinv
cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      return(m)
}
