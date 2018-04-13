## cachematrix.R
# "An object is data with functions. A closure is a function with data" - John D. Cook

# Second programming assignment

# makeCacheMatrix 
# special matrix object that can cache its own inverse

#1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse
# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
      x <<- y
      inv <<- y
    }
    get <- function() x
    setinv <- function(ninv) inv <<- ninv
    getinv <- function() inv
    # functions stored in list
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


#cacheSolve
# calculates inverse matrix. If inverse has been calculated,
# it gets the inverse from the cache and skips computation. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  
    if(!is.null(inv)){
      message("Fetching cached data")
      return(inv)
    }
    dat <- x$get()
    inv <- solve(dat, ...)
    x$setinv(inv)
    inv
    
}
