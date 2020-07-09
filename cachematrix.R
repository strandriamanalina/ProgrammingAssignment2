## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the solution as a matrix
  solution <- NULL
  
  # Setter function that sets
  set <- function(y) {
    x <<- y
    solution <<- NULL
  }
  get <- function() x
  setsolution <- function(solve) solution <<- solve
  getsolution <- function() solution
  

  # stores the named setters and getters inside a list
  list(set = set, get = get,
       setsolution = setsolution,
       getsolution = getsolution)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Attempts to get the inverse matrix
  solution <- x$getsolution()

    # checks if the solution is not empty, if not empty, prints the cached solution
  if(!is.null(solution)) {
    message("getting cached data")
    return(solution)
  }
  
  # if the solution is empty, gets the matrix from the input
  mat <- x$get()
  
  # calcultes andd returns the inverse & returns the value of the solution to the parent environment
  solution <- solve(as.matrix(mat))
  x$setsolution(solution)
  solution
}

m <- matrix(c(1,4,2,18,12,29, 34, 1, 2), 3, 3)
nrow(m)
ncol(m)
m <- makeCacheMatrix(m)
cacheSolve(m)


solve(matrix(c(1,4,2,18,12,29, 34, 1, 2), 3, 3))


