# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to calculate and to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL   # m will store the cached inverse matrix
  set <- function(y) # Setter for the matrix
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x   # Getter for the matrix
  setinverse <- function(inverse) m <<- inverse     # Setter for the inverse
  getinverse <- function() m     # Getter for the inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  # Return the matrix with the defined functions

}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()  # Getting the inverse
  if(!is.null(m))      # If the inverse is already calculated, return it
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()    # If the inverse is not yet calculated, we calculate it
  m <- solve(data)
  x$setinverse(m)     # Cache the inverse
  m    # Return it

}