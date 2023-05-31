library(proxy)

library(recommenderlab)

library(reshape2)

movies <- read.csv("movies.csv", header = TRUE, stringsAsFactors=FALSE)   #Data
ratings <- read.csv("ratings.csv", header = TRUE)         

movies_new <- movies[-which((movies$movieId %in% ratings$movieId) == FALSE),] #Check only the movies present in both data sets

nrow(movies)

nrow(movies_new)

#Difference Occurs

movie_recommendation <- function(input,input2,input3) {
  
  #Inputs are just Movie Names
  row_num <- which(movies_new[,2] == input) ## finding row number
  row_num2 <- which(movies_new[,2] == input2)
  row_num3 <- which(movies_new[,2] == input3)
  
  userSelect <- matrix(NA,10325)
  userSelect[row_num] <- 5 #hard code first selection to rating 5
  userSelect[row_num2] <- 4 #hard code second selection to rating 4
  userSelect[row_num3] <- 3 #hard code third selection to rating 3
  userSelect <- t(userSelect)
  
  ratingmatrix <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingmatrix <- ratingmatrix[,-1]
  
  colnames(userSelect) <- colnames(ratingmatrix)
  
  ratingmatrix_new <- rbind(userSelect,ratingmatrix)
  ratingmatrix_new <- as.matrix(ratingmatrix_new)
  
  #Convert rating matrix into a sparse matrix
  ratingmatrix_new <- as(ratingmatrix_new, "realRatingMatrix")
  
  #Create Recommender Model. "UBCF" stands for user-based collaborative filtering
  recommender_model <- Recommender(ratingmatrix_new, method = "UBCF",param=list(method="Cosine",nn=5))
  recommender <- predict(recommender_model, ratingmatrix_new[1], n=10)
  
  recom_list <- as(recommender, "list")
  no_result <- data.frame(matrix(NA,1))
  recom_result <- data.frame(matrix(NA,10))
  
  
  if (as.character(recom_list[1])=='character(0)'){
    no_result[1,1] <- "There are no similar movies in the dataset based on the movies you have selected. Please select different combinations.!!"
    colnames(no_result) <- "No results"
    return(no_result) 
  } else {
    for (i in c(1:10)){
      recom_result[i,1] <- as.character(subset(movies, 
                                               movies$movieId == as.integer(recom_list[[1]][i]))$title)
    }
    colnames(recom_result) <- "User-Based Collaborative Filtering Recommended Titles"
    return(recom_result)
  }
}
