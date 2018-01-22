## Matrix inversion could be a costly computation. 
##So we have construct function that respectively create a matrix, 
##inverse the created matrix and then put the result in cache.
##Then, we have construct a function done to call the result stored in cache.

## This function create a matrix as a special object able to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## Define the variable containing the inverse as null
    inv <- NULL
    ## Define the matrix
    set <- function(y){
      x <- y
      inv <- NULL
    }
    ## get the matrix
    get <- function() x
    
    ## define the inverse 
    setinverse <-function(inverse) inv <- inverse
    
    ## get the inverse
    getinverse <- function()inv
    
    ## Define list containing set and get inverse
    list( set = set , get = get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## This function return the inverse of the matrix in two ways :
## If the inverse has already been evaluated, it returns this 
## result as it is stored
## If the inverse is not availbale, its calculation is 
## performed, then stored, and at the end return

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Get inv
    inv <- x$getinverse()

    ## setup what to do depending on the content of inv
    if(!is.null(inv)) {
        message("Getting inversed matrix.")
        return(inv)
    }
    
    ## Get the matrix and compute its inverse
    matrix <- x$get()
    inv <- solve(matrix,...)
    
    ## Return the inverse
    message("Calculating inversed matrix.")
    return(inv)
}

