## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the solution as a matrix
  solution <- NULL
  
  # Setter function
  set <- function(y) {
    x <<- y # Assign the input argument to the x object in the parent environment
    solution <<- NULL # Assigns NULL to solution. So this clears any value stored in solution before the execution of cacheSolve() 
  }
  get <- function() x # This gets the value of x from the parent environment
  setsolution <- function(inv) solution <<- inv # Another setter function : assigns the inverse to solution when setsolution(solution) is called
  getsolution <- function() solution # This just gets the solution
  
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
  # Attempts to get the inverse matrix and assigning it to solution
  solution <- x$getsolution()

    # checks if the solution is not empty: if not empty, prints the cached solution
  if(!is.null(solution)) {
    message("getting cached data")
    return(solution) 
  }
  
  # if the solution is empty, gets the matrix to be solved from the input
  m <- x$get()
  
  # calculates and returns the inverse
  solution <- solve(m, ...)
  x$setsolution(solution) # assigns the calculated solution to the parent environment
  solution # returns the solution
}

# Testing 1
n1 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(n1)
cacheSolve(myMatrix_object)

# testing 2
n2 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
myMatrix_object <- makeCacheMatrix(n2)
cacheSolve(myMatrix_object)




