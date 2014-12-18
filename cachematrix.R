# makeCacheMatrix and cacheSolve make, store, and return inverted matrices. When a matrix is entered, it will search the cache to see if it has calculated the inverse before. If it hasn't, it will do so and store it in memory. If it has and nothing has changed, it will return the previously stored solution.

#This function makes the submitted matrix available in a special form for the cache to invert, solve, and store.

makeCacheMatrix <- function(x = numeric()) {   #creates function of functions
  cache <- NULL                                #creates empty variable for future cache
  setMatrix <- function(newValue) {            #assigns matrix with new value to x variable
    x <<- newValue
    cache <<- NULL                             #empties the cache for next round after storing matrix
  }
  getMatrix <- function() {                    #returns matrix prepared for inversion
    x
  }
  cacheInverse <- function(solve) {            #caches as a matrix to be inverted
    cache <<- solve
  }
  getInverse <- function() {                   #returns special matrix as cache
    cache
  }
  list(setMatrix = setMatrix,                  #returns list of functions in makecacheMatrix
       getMatrix = getMatrix,
       cacheInverse = cacheInverse,
       getInverse = getInverse)
}


#This function solves the prepared matrix from makeCacheMatrix and stores it in memory. If it has already solved the matrix, it will return the previous solution.

cacheSolve <- function(y, ...) {               #function to solve or return invertible matrix
  inverse <- y$getInverse()                    #pulls the stored cache value
  if(!is.null(inverse)) {                      #if it finds a value in cache, return it
    return(inverse)
  }
  data <- y$getMatrix()                        #else: invert matrix and store as cache
  inverse <- solve(data)
  y$cacheInverse(inverse)                      #add to cache
  inverse                                      #return output as inverse
}