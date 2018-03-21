## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix gets a matrix as an input, 
  #Sets and gets the value of the matrix,
  #Set the inverse Matrix and get the inverse Matrix. The matrix object
  #Should be able to cache its own object 
 
##Takes Matrix as the input
makeCacheMatrix <- function(x = matrix()) {
    InvM <- NULL                             ## Assing value to variable
  
         setM <- function(y) {              ##set the value of the Matrix
          x <<- y
          InvM <<- NULL
        }
      
      getM <- function() x                          ##get the value of the Matrix and assigne it to getM
      setInv <- function(inverse) InvM <<- inverse  ##Assign the vaue of invertiable Matrix to setInv  
      getInv <- function() InvM                     ##Update the value of invertiable Matrix to getInv
        list(setM = setM,                           ##List all the assign values
             getM = getM,
             setInv = setInv, 
             getInv = getInv)

}


## Write a short comment describing this function

## cacheSolve function takes the output of the makeCacheMatrix as an input and checks the inverse matrix has any values. 
## In case there value it return the value if not it return null. 

cacheSolve <- function(x, ...) 
  {
  invM <- x$getInv()                      ##assign the value of getinverse to inverse Matics variable
    if(!is.null(invM))                    ##Check for null values
      {
           message("Getting Cached Invertible Matrix") ##return output as a message if the condition satisifies
            return(invM)                             ## return the value of invM
      }
  
    MatData <- x$getM()                   ##get the matrix data from makeCacheMatrix function 
    invM <- solve(MatData, ...)           ##Inverse the Matrix data using solve function
      x$setInv(invM)                      ##set the value of inverse matrix
      return(invM)                        ##return the output of the inverse matrix
}


## ------------------Test-------------------##

Test1 <- matrix(c(3,8,8,5),2,2)       ##assign matrix value
Test1                                 ##test the values assigned

CMatrix <- makeCacheMatrix(Test1)     ##assign the matix to makeCacheMatrix function 
starttime=Sys.time()                  ##create and assign the system time to the variable
cacheSolve(CMatrix)                   ##run the fucntion cacheSolve by assigning Matrix value
duration = Sys.time()-starttime       ##capture the value to time difference between start and end and assign it to duration
print(duration)                       ##print the value

##repate the above description again to capture the cache timing. 
start.time=Sys.time()
cacheSolve(CMatrix)
duration = Sys.time()-starttime
print(duration)
