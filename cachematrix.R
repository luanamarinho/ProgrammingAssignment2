##Assignment on closures and lexical scoping in R

##Similarly to the example, the following function will create
##a list object that can store the cached value of the inverse of a square matrix


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) 
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

test <- makeCacheMatrix()
test
#$set
#function (y) {
#  x <<- y
#  inv <<- NULL
#}
#<environment: 0x00000000036c0750>

#$get
#function () 
#  x
#<environment: 0x00000000036c0750>

#$setInverse
#function () 
#  inv <<- solve(x)
#<environment: 0x00000000036c0750>

#$getInverse
#function () 
#  inv
#<environment: 0x00000000036c0750>


#The following function will calculate the inverse of the matrix
#returned by makeCacheMatrix$get above. If the inverse has already been
#calculated and the matrix remained the same, then the cachesolve function
#should retrieve the inverse from the cache. If the inverse has not been
#calculated, data gets the matrix stored within makeCacheMatrix,
#m calculates the inverse, and x$setinverse(m) stores it in the object m in
#cacheSolve.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv2 <- solve(data, ...)
  inv2
}

#Assingning a matrix to test the function
b <- matrix(c(2,2,3,5),2,2)
aux1 <- makeCacheMatrix(b)
aux1$get()
#      [,1] [,2]
#[1,]    2    3
#[2,]    2    5
aux1$setInverse()
aux1$getInverse()
#      [,1]  [,2]
#[1,]  1.25 -0.75
#[2,] -0.50  0.50

environment(makeCacheMatrix)
#<environment: R_GlobalEnv>
environment(aux1$set)
#<environment: 0x0000000004b5f5a8>
ls(environment(aux1$set))
#[1] "get"        "getInverse" "inv"        "set"        "setInverse" "x" 

aux2<- cacheSolve(aux1)
#getting cached data
aux2
#      [,1]  [,2]
#[1,]  1.25 -0.75
#[2,] -0.50  0.50


#Another test

c<-matrix(1:4,2,2)
aux3<-makeCacheMatrix(c)
aux4<- cacheSolve(aux3)
aux4
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

aux3$get()
#     ,1] [,2]
#[1,]    1    3
#[2,]    2    4

aux3$getInverse()
#NULL

aux3$getInverse
#function() inv
#<environment: 0x0000000005155ad0>