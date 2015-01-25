## R Functions to create and use inverted matrices for coursea course programming assignment

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## It is a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean
makeCacheMatrix <- function(ori_matrix = matrix()) {
  ## inv_matrix is a variable which shall store inverse matrix
  inv_matrix <- NULL
  
  # Setter for the matrix
  set <- function(y) {
    ori_matrix <<- y
    inv_matrix <<- NULL
  }
  # Getter for the matrix
  get <- function() ori_matrix
  
  # Setter for the inverse
  setinverse <- function(inverse) inv_matrix <<- inverse
  
  # Getter for the inverse
  getinverse <- function() inv_matrix
  
  # Return the matrix with our newly defined functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(cache_matrix, ...) {
  
  ## Get in cache inverse
  inv_matrix <- cache_matrix$getinverse()
    
  ## check if the we have cached matrix in cache
  if(!is.null(inv_matrix)) {
    message("Getting cached inv_matrix")
    return(inv_matrix)
  }
  
  ## there's no cached matrix available.
  ## inverted matrix inv_matrix not created yet, So create
  matrix_to_inverse <- cache_matrix$get()
  
  ## solve is used
  inv_matrix <- solve(matrix_to_inverse)
  
  ## call set
  cache_matrix$setinverse(inv_matrix)
  
  inv_matrix
  
}