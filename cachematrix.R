# From the assignment description

# The first function, makeVector creates a special "vector", which is really a 
# list containing a function to

# set the value of the vector
# get the value of the vector
# set the value of the mean
# get the value of the mean

makeVector <- function(x = numeric()) {
  
  # Initialize m as 'null'.
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  # internal functions
  
  get <- function() x
  
  setmean <- function(mean) m <<- mean
  
  getmean <- function() m
  
  # Return this list.
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}


# From the assignment description

# The following function calculates the mean of the special "vector" created 
# with the above function. However, it first checks to see if the mean has 
# already been calculated. If so, it gets the mean from the cache and skips the 
# computation. Otherwise, it calculates the mean of the data and sets the value 
# of the mean in the cache via the setmean function.

cachemean <- function(x, ...) {
  
  # Assign the cached value to 'm'
  
  m <- x$getmean()
  
  # if m is not null, return the cached value
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise this means 'm' is null, so call the function get in 'x' and assign
  # the value to 'data'
  
  data <- x$get()
  
  # Since m is null, generate a value for m from the vector 'data'
  
  m <- mean(data, ...)
  
  x$setmean(m)
  
  # return 'm' as the value for this function
  
  m
}

## This will Make a cached version of the matrix x

makeCacheMatrix <- function(x=matrix()) {

  # initialize the inverse matrix to NULL
  invMatrix  <- NULL
  
  set <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  
  # Internal function to generate the inverse of the matrix
  
  get <- function()x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix   
  
  # Return the inverse as a list
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  
  # set the inverse Matrix to the value in 'getInverse'. 
  
  invMatrix <- x$getInverse()
  
  # If we have run this prior, there will already be a value in it.
  
  if(!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  # Otherwise we set the inverse
  
  mat <- x$get()
  invMatrix <- solve(mat,...)
  x$setInverse(invMatrix)
  invMatrix
}

test_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
test_matrix$get()
test_matrix$getInverse()
cacheSolve(test_matrix)
test_matrix$getInverse()




