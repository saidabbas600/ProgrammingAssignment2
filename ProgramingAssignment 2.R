## Function to create a special "vector" object that can cache its mean
makeCacheVector <- function(x = numeric()) {
  m <- NULL  # Initialize the cached mean to NULL
  
  # Function to set the value of the vector
  set <- function(y) {
    x <<- y    # Use <<- to assign to x in the parent environment
    m <<- NULL # Reset the cached mean since the vector has changed
  }
  
  # Function to get the value of the vector
  get <- function() x
  
  # Function to set the cached mean
  setmean <- function(mean) m <<- mean
  
  # Function to get the cached mean
  getmean <- function() m
  
  # Return a list of the four functions
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Function to compute the mean of the special "vector" object
cacheMean <- function(x, ...) {
  m <- x$getmean()  # Attempt to get the cached mean
  
  # Check if the cached mean is already calculated
  if (!is.null(m)) {
    message("getting cached data")
    return(m)  # Return the cached mean if available
  }
  
  # If the cached mean is not available, compute the mean
  data <- x$get()
  m <- mean(data, ...)  # Compute the mean
  x$setmean(m)          # Cache the newly computed mean
  m                     # Return the computed mean
}

## Usage Example

# Create a special "vector" object
myVector <- makeCacheVector(c(1, 2, 3, 4))

# Compute and cache the mean
cacheMean(myVector) # First time calculation

# Retrieve the cached mean
cacheMean(myVector) # Retrieves cached data
