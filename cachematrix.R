## The two functions below allow users to set up 
## and execute matrix inversion using cached data 

## The initial function is makeCacheMatrix() and is defined below.
## The function sets up an object that contains 4 functions and two 
## objects (x and m). 

## The set() function sets values of the matrix 
## input (m) and object to hold the inverse (m).

## The get() function returns the input matrix x

## The setinverse() function sets value of the inverse
 
## The getinverse() function returns the value of the inverse (m)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

## The function below checks to see if an inverse is available within 
## the makeCacheMatrix object environment. If available, that value of 
## m is returned.

## If not, data is pulled from the object environment using the x$get() function

## The inverse is then calculated and stored as an object m. 
## That value of m is then stored within the environment of the input object
## from the makeCacheMatrix function to be cached for later use, using the 
## x$setinverse(m) function

## Finally, m is returned after calculation. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Below is a suggested test of the function. 
# test input matrix 
test_input <- matrix(c(1,2,3,4), ncol = 2, nrow = 2)
# test Matrix object
test_Matrix_obj <- makeCacheMatrix(test_input)
# test final function
cacheSolve(test_Matrix_obj)




