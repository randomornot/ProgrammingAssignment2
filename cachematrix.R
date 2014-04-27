## makeCacheMatrix is a function which takes matrix as an input and gives a list as an output, where
## its elements are set, get, setinv and getinv. 
## cacheSolve function takes list returned by makeCacheMatrix as input, checks if the inverse is already 
## in cache, and if not it computes it and stores it in cache by entering it in the setinv column of list


makeCacheMatrix <- function(x = matrix()) {


    i <- NULL
    set <- function(y) {  
      x <<- y                   ##Here matrix X is being cached in the memory 
      i <<- NULL                
    }
    get <- function() x
    setinv <- function(inv) i <<- inv  ##We are defining these so that we can add these elements to list when it is not cached
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)


}



cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  i <- x$getinv()  ## assign value of inverse in list x, to check whether we have cached it or not
  if(!is.null(i)) {     ## if we have cached it, we will have non 0 inverse value defined in line 15 above
    message("getting cached data")
    return(i)
  }
  data <- x$get()  ##In case not cached till now, getting the matrix whose inverse to be found
  i <- solve(data, ...) ## finding the inverse using solve function
  x$setinv(i)  ## Setting inverse value in the list where it gets cached because of <<- operator used in 15 line
  i
  

}
