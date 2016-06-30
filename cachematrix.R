  ##              https://github.com/zekiandzeki/ProgrammingAssignment2
  ##              SHA-1 hash: 27987823fcf81d46a5e2186391addd87c079b879
  ##              This is part of the Coursera - R Programming course
  ##              Week 3 Assignment 2: Lexical Scoping
  ##              caching the inverse of a matrix
 
   ##      The makeCacheMatrix function is a spectial "matrix" that returns a list of functions and
   ##      it store a matrix and a chached value ofthe invese of the marics.
 
   ##      It consistes the following Steps:
             #  setMatrix      to set the value of a matrix
             #  getMatrix      to get the value of a matrix
             #  cacheInverse   to get the cahced value (inverse of the matrix)
             #  getInverse     to get the cahced value (inverse of the matrix)
 
 makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
   set <- function(y) {
     x <<- y
     i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(
     set = set,
     get = get,
     setinverse = setinverse,
     getinverse = getinverse)
 }
 
 
 ##      The cacheSolve function calculates the inverse of the special "matrix" that created with
 ##      makeCacheMatrix function. This functin primarly checks to see if the inverse function
 ##      is calculated and skips the computation if it able to get the inverse from the cache whereas
 ##      itt calculates the inverse the inverse of the matrix and sets the value of the inverse in the
 ##      cache through the setinverse function.
 
 cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   i <- x$getinverse()
   if(!is.null(i)) {
     message("getting cached data")
     return(i)
   }
   data <- x$get()
   i <- solve(data, ...)
   x$setinverse(i)
   i
 }
