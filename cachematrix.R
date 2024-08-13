
# I will write two functions. The first function will create a matrix and cache the inverse of the matrix.
# The second function will take the output of the first function (cached inverse) and return the inverse of 
# the special matrix.

## The following function will create a matrix and cache the inverse of the matrix.
# To do this, I need to complete the following steps:
# 1. Declare function
# 2. Create empty matrix 'x'
# 3. Fill matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                                       # inverse 'inv' of matrix 'x' set to empty
  set <- function(y) {
    x <<- y                                         # Assign a new value of 'y' to the matrix 'x' in the parent environ.
    inv <<- NULL                                    # reset to NULL as it needs to be recalculated
  }
  # 'get' is the function we will use to get the inverse 'inv'
  get <- function() x                               # simply return the current value of 'x'
  # setmean is a function to set the value of the mean 'm'
  setinverse <- function(solve) inv <<- solve   # Assign the provided 'mean' to 'm' in the parent environ.
  # getmean is a function to get the value of the mean 'inv'
  getinverse <- function() inv                      # simply return the current value of 'm'
  # return a list of the functions to interact with 'x' and 'inv'
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function will take the matrix from 'makeCacheMatrix' and do the following:
# 1. Check if the inverse has already been computed
# 2. IF yes, AND the inverse has not changed
# 3. Then return cached inverse


# Function to calculate and cache the inverse of the special matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()                             # retrieve the cache mean from the special vector object
  if(!is.null(inv)) {                               # Check if the mean is already cached (i.e., not NULL)
    message("getting cached data")                  # inform user cached data is being used
    return(inv)                                     # return the cached mean value
  }
  # If the mean is not cached, retrieve the vector data
  data <- x$get()
  inv <- solve(data, ...)                           # calculate the mean of the vector data, passing addition args.
  x$setinverse(inv)                                    # cache the calculated mean in the special vector object
  inv                                               # return the calculated mean
}


# Let us give it a spin:

mat <- makeCacheMatrix()
mat$set(matrix(c(1, 2, 3, 4), 2, 2))

invrs <- cacheSolve(mat)
print(invrs)

# Then, we test if cacheSolve retrieves the cached data by running invrs again:

invrs <- cacheSolve(mat)
print(invrs)

