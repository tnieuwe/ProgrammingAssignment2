## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function returns a special "matrix" object, which is actually a list of
# the matrix and a holder to cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #make objec m
    m <- NULL
    #Make a function called set() that takes an argument y and puts it in x
    #it also makes m Null if it is not. This is basically used to assign a
    #value to the matrix, aka, normally what you input, but you can change the
    #value of the object by running object_out$set(new_matrix)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #Make a function get that just returns the matrix you put in via set or
    #the function itself (as the output is a list and not a matrix object)
    get <- function() x
    #The bottom functions are the same as those above, but do not initially
    #have a value when you run this function on a matrix.
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    #Return a list of these functions as your new object with set() allowing 
    #you to change the held matrix and get() returning the held matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Write a short comment describing this function
#The below function takes an object made by makeCacheMatrix (a list) and
#will first, check if there is a cached inverse matrix, and if there is not
#it will solve() the matrix and cache the result. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #Pull the value from getsolve
    m <- x$getsolve()
    #Test if the value is not null, if it is not null return value and print
    #that you used cached data
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    #if above is not true
    #Get the data stored in the object
    data <- x$get()
    #Solve the data including whatever other arguments you want
    m <- solve(data, ...)
    #Set the inverted matrix into the cache
    x$setsolve(m)
    #print out the inverted matrix
    m
}

#P.S. Reviewer
#I'm sorry I have so many comments, but I felt like the prompt for this 
#exercise told me next to nothing about how the function worked and so
#I commented each step of the function for my own personal gain so I was sure
#I had a full understanding of what was going on. Otherwise this is an exercise
#in copy and pasting, not coding. So sorry for the extra text. 