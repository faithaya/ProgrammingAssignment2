# Assignment: Caching the Inverse of a Matrix through Lexical Scoping

# Goal: Write a pair funtion that cache the inverse of a matrix
# 
# Note: Scoping is the mechanism within R that determines how R finds sympbols
# (.ie programming lanauge elements) to retrieve their values during the execution
# of an R script. 
# 
# Note: Lexical Scoping is used to retrieve values from object based on the way functions are
# nested when they are written.
# 
# Note: Cache is due to how the code is built at design time, not how the code is called 
# at runtime.Cache is a way to store objects in memory to accelerate subsequent access
# to the same object.



# 1.MakeCacheMatrix: This function creates a special "matrix" object
# that cache its inverse through makeVector.

# Key Concept: makeVector() builds a set of functions AND returns the functions 
# within a list to the parent environment
# makeVector creates a special "vector" which makes a list containing:
#    a) Set the value of the vector
#    b) Get the value of the vector
#    c) Set the value of inverse
#    d) get the value of the inverse

# STEP 1: Initialize Objects
makeCacheMatrix <- function(x= matrix()){   
  #inialized 'x' as a function argument
  
  a <- NULL # 'a' is set to NULL and initialize it as an object within makeVector() environment
  #  to be used later in the code    
  #Define the "behavior" of the functions in the makeVector()
  
  # STEP 2: Define the "Behaviors" or Functions for Objects of Type MakeVector()
  set <- function (b) #set() takes an argument named 'b'  
  {x <<- b #assigns the 'b' value to the 'x' object in the parent environment
  a <<- NULL} # assigns the value of NULL to the 'a' object in the parent environment
  
  get <- function () x #defines the getter for the vector x
  setinverse <- function(inverse)a <<- inverse #defines the setter for the inverse 'a'
  # since 'a ' is defined in the parent environment and we need to access it after setinverse()
  # completes, the code uses the <<- form of the assignment operator to assign the input argument
  # to the value of 'a' in the parent environment.
  getinverse <- function() a # defines the getter for the inverse ''a' 
  
  # Step 3: Create a New Object by Returning a List()
  # Assigns each of the 4 functions as an element within a list() and returns it to the 
  # parent environment
  
  #Each element is in the list is named which allows the use of $ from of the extract operator
  list (set=set , #gives the 'set' to the set() function defined above
        get=get, #gives the name 'get' to the get() function defined above
        setinverse=setinverse , #gives the name 'setinverse' to the setinverse () function defined above
        getinverse=getinverse ) } #gives the name 'getinverse' to the getinverse () function defined above


# 2. This function computes the inverse of the special "matrix"
# returned by makeCahcematrix above. If the inverse has already been calculated 
# (and the matrix has not changed,) then the cachesolve shold retrieve
# the inverse from the cache.


# Step 1: Initialize objects : with a single argument'x' and an ellipse '...' to alow additional
# arguments in the function
cacheSolve<- function (x,...){
  
  #Step 2: Retrieve the inverse
  # the function attempts to retrieve the inverse from the object passed in as an argument.    
  a<- x$getinverse() #calls the 'getinverse' on the input object 'x'
  if(!is.null(a)) { #checks to see where there is NULL. If the value is not equal to NULL
    message("getting cached data") #we have a valid , cache inverse
    return(a) # and can reurn to the parent environment
  }
  data<- x$get() # if the result of '!is.null(a)' is FALSe, cachemean() gets the vector from the
  a<- solve(data, ...) #input object 'x' to set the inverse in the input object, and 
  x$setinverse(a) 
  a #returns the value of the inverse to the parent environment by printing the inverse object
}
