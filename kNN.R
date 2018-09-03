#kNN Machine Learning Algorithm, classifying every row of an input data.frame 
#(test_data) based on a data.frame of already available points (train_data)
kNN <- function(test_data, train_data, k_value) {
  #Create empty prediction vector for the results
  predictionOutput <- c()
  
  #Outer loop that iterates over the rows in test_data (the data to classify)
  for(i in c(1:nrow(test_data))) { 
    #Creating empty vectors for intermediary results
    eu_dist = c() #eu_dist & eu_char empty vector
    eu_char = c()
    #Variables to store the sum of the k-nearest classifiers
    class0 = 0
    class1 = 0
    
    #Step 1 (Slide: 65): Compute the distance from the new point(s) in the 
    #data.frame test_data to all other points (train_data).
    for(j in c(1:nrow(train_data))){
      #Calculating the euclidean distance between the to-classify point and 
      #all already available points.
      eu_dist <- c(eu_dist, euclideanDist(test_data[i,], train_data[j,]))
      #Creating a vector from the existing data converting the 0, 1 values into
      #characters.
      eu_char <- c(eu_char, as.character(train_data[j,'Y'])) 
    }
    
    #Step 2: (Slide: 76): Choose the number of neighbors.
    #Merge the eu_char & eu_dist columns into one data.frame.
    eu <- data.frame(eu_char, eu_dist)
    #Sorting eu dataframe to get top K neighbors.
    eu <- eu[order(eu$eu_dist),]
    #Extracting top k neighbors.
    eu <- eu[1:k_value,] 
    
    #Step 3 (Slide 77): Record the class labels of the k nearest neighbors.
    for(k in c(1:nrow(eu))){ 
      if(as.character(eu[k,"eu_char"]) == "0"){
        class0 = class0 + 1 
      }
      else class1 = class1 + 1 
    }
    
    #Step 4: (Slide: 82) Majority voting. Compares the number of neighbors with
    #class label class0 or class1.
    #If the majority of neighbors are class0, then the output vector 
    #"predictionOutput" will contain a 0 at this position.
    if(class0 >= class1) {
      predictionOutput <- c(predictionOutput, "0") 
    }
    else if(class0 < class1){
      #If the majority of neighbors are class1, then the output vector 
      #"predictionOutput" will contain a 1 at this position.
      predictionOutput <- c(predictionOutput, "1") 
    }
  }
  #Returning the predictionOutput vector.
  return(predictionOutput)
}

euclideanDist <- function(a, b){
  d = 0
  #Iterating over the X1 and X2 coordinates (positions 3 and 4 in the 
  #data.frame)
  for (i in c(3:(length(a)))) 
  {
    d = d + (a[[i]]-b[[i]])^2
  }
  d = sqrt(d)
  return(d)
}

train_data <- data.frame(index = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
                         Y = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L, 0L),
                         X1 = c(2, 2.8, 1.5, 2.1, 5.5, 8, 6.9, 8.5, 2.5, 7.7),
                         X2 = c(1.5, 1.2, 1, 1, 4, 4.8, 4.5, 5.5, 2, 3.5))
train_data[,3:4] <- scale(train_data[,3:4])

#V1 and Y are used as placeholders, they are not required by the algorithm.
test_data <- data.frame("index"="?", "Y"="?", "X1"=-0.55, "X2"=-0.80)

prediction <- kNN(test_data, train_data, 5)
prediction