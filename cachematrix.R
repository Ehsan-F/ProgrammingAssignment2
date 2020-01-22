## Put comments here that give an overall description of what your
## functions do

#--- This function creates a set of functions that:
#- Create a matrix
#- Get and display the user-defined matrix
#- Calculate the inverse of the user-defined matrix
#- Get and display the calculated inverse matrix

makeCacheMatrix <- function(sq_matrix = matrix()) {
  inv_matrix <- NULL
  
  set.matrix <- function(matrix) {
    sq_matrix <<- matrix
    inv_matrix <<- NULL
  }
  
  get.matrix <- function() sq_matrix
  set.inverse <- function(inverse) inv_matrix <<- inverse
  get.inverse <- function() inv_matrix
  list(set.matrix = set.matrix, get.matrix = get.matrix,
       set.inverse = set.inverse, get.inverse = get.inverse)
}



#--- Once we have set our matrix using the set.matrix function from above,
#--- we can either use the set.inverse function from above, or use the function below
#--- to calculate the inverse matrix.
#--- This function also caches the inverse such that if we run it again, it won't recalculate the mean,
#--- rather it will pull it out of the cache of the envrionment of the MakeCacheMatrix function.
cacheSolve <- function(x, ...) {
  
  #- Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get.inverse()
  
  if (!is.null(inv_matrix)) {
    message("getting cached matrix")
    return(inv_matrix)
  }
  
  data <- x$get.matrix()
  inv_matrix <- solve(data)
  x$set.inverse(inverse = inv_matrix)
  inv_matrix
}

