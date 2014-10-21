### Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) { # Creates a special "matrix" object
                                            # that can cache its inverse.   
  m <- matrix()
  set <- function(y) { # set the matrix
    x <<- y
    m <<- matrix()
  }
  get <- function() x # get the matrix
  setinverse <- function(solve) m <<- solve # set the inverse
  getinverse <- function() m # get the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) { # Calculates the inverse 
                                 # of the special "matrix" 
                                 # created with the above 
                                 # function (makeCacheMatrix).
  m <- x$getinverse()
  if(!is.matrix(m)) { # First checking if the inverse has 
                      # already been calculated.
    message("getting cached data")
    return(m)
  }
  data <- x$get() # If inverse has already been calculated, 
                  # it gets the inverse from the cache and
                  # skips computation...
  m <- solve(data, ...)
  x$setinverse(m) # ... else, it calculates the inverse of 
                  # the data and sets the value of the inverse
                  # in the cache via the setinverse function. 
  m
}
