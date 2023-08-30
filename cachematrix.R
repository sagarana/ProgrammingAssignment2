# This pair of functions stores an invertible matrix (makeCacheMatrix),
# then returns the inverse of the matrix and stores it also for
# future use (cacheSolve). In subsequent calls, cacheSolve retrieves
# the stored inverse unless the source matrix has changed. This reduces 
# computational load since the inverse is only calculated when necessary.

# makeCacheMatrix stores the matrix that will be used later in cacheSolve.
# It also creates a list with four other functions (set, get, setinv, and getinv) 
# that may be used later on their own, or by cacheSolve.

makeCacheMatrix <- function(x = matrix()) { # takes a matrix as argument
        
        # lines 16 and 24 clear previously stored values of 'inv' when 
        # the function is run (line 16) or when the 'set' function 
        # assigns a new matrix (line 24).
        inv <- NULL
        
        # 'set' assigns a new matrix. This is not strictly required but 
        # makes it simpler to change the matrix later. It uses the super-
        # assignment operator (<<-) to save x to the environment outside 
        # the 'set' function so it can be accessed later.
        set <- function(y) {      
                x <<- y
                inv <<- NULL
        }
        
        # 'get' returns the stored matrix
        get <- function() x       

        # 'setinv' assigns a new value to inv. It also uses the super-assignment
        # operator to store inv where it can be accessed later
        setinv <- function(new_inv_value) inv <<- new_inv_value  
        
        # 'getinv' returns the currently stored inv value
        getinv <- function() inv
        
        # lines 39-42 create a list containing the four functions, so
        # they can be called later on their own or by cacheSolve
        list(set = set, 
             get = get, 
             setinv = setinv,
             getinv = getinv) 
}

# cacheSolve returns the inverse of the matrix stored by makeCacheMatrix.
# It uses 'getinv' from makeCacheMatrix to check whether the inverse is 
# already stored. If 'inv' is NULL it uses the:
        # - 'get' function to retrieve the stored matrix (line 63)
        # - 'solve' function to calculate the inverse (line 64)
        # - 'setinv' function to assign the (line 65)

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
                
                # The 'return' function stops the processing. Anything
                # below this line is only run if 'inv' was NULL and needs
                # to be calculated
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        message("calculating inverse")
        inv
}


# Below is console input/output to demonstrate how it works.

# 1. Use 'makeCacheMatrix' to store the matrix and functions as 'test'

## > test <- makeCacheMatrix(matrix(c(0.5,-0.25,-1,0.75),2,2))

# 2. Confirm that 'test' stored the matrix correclty
 
## > test$get()
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75

# 3. Run cacheSolve on 'test' to calculate the inverse for the first time

## > cacheSolve(test)
## calculating inverse
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4

# 4. Run cacheSolve on 'test' again; it should retrieve a stored value instead of 
# recalculating

## > cacheSolve(test)
## getting cached data
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4

# 5. Use 'set' to assign a new matrix to 'test'

## test$set(matrix(c(1,3,5,6),2,2))

# 6. Confirm that 'test' now stores the new matrix

## > test$get()
## [,1] [,2]
## [1,]    1    5
## [2,]    3    6

# 7. Run cacheSolve on 'test' again; the matrix changed so it should re-calculate
# instead of retrieving the stored value

## > cacheSolve(test)
## calculating inverse
## [,1]       [,2]
## [1,] -0.6666667  0.5555556
## [2,]  0.3333333 -0.1111111
