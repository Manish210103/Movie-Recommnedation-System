library(recommenderlab)

library(ggplot2)                       

library(data.table) #For converting the Data into more Usable form 

library(reshape2)

#For importing the data setting the Working Directory
#setwd("C:\\Users\\TEMP.CS2K16.003\\Downloads\\Movie-Recommendation-System-master\\Movie-Recommendation-System-master")

#Loading the Movie and Rating Data

movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE) 

head(movie_data)

rating_data <- read.csv("ratings.csv")

head(rating_data)

#Eliminating the '|' symbol and converting the genre into a sparse matrix

movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE) #Taking the genres as a data frame

head(movie_genre)
#Converting the genres into a matrix like data frame 

movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', type.convert=TRUE), stringsAsFactors=FALSE) 

head(movie_genre2)

colnames(movie_genre2) <- c(1:10) #Set names for the matrix like object

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")


genre_mat1 <- matrix(0,10330,18) #Creating a matrix of 10330 rows(observations) and 18 columns(list of genres)

genre_mat1[1,]<-list_genre

colnames(genre_mat1) <- list_genre #Set Column names as genres

head(genre_mat1)

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) #Set Values as 1 for the present genre
    genre_mat1[index+1,gen_col] <- 1
  }
}

genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
}

head(genre_mat2)

#Create a matrix to search for a movie by genre and year

years <- as.data.frame(movie_data$title, stringsAsFactors=FALSE)## getting the movie names into years

head(years)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))## getting the last characters in a string
}

#Separating the years

years <- as.data.frame(substr(substrRight(substrRight(years$`movie_data$title`, 6),5),1,4))

head(years)

#Building the Original Matrix (Search Matrix by changing the genre in movie_data to the modified genre sparse matrix)

SearchMatrix <- cbind(movie_data[,1], substr(movie_data[,2],1,nchar(movie_data[,2])-6), years, genre_mat2)

colnames(SearchMatrix) <- c("movieId", "title", "year", list_genre)

head(years)

#Write it into a file

write.csv(SearchMatrix, "search.csv")

# Example of search an Action movie produced in 1995:

subset(SearchMatrix, Action == 1 & year == 1995)$title


##Creating a user profile

binary_rating<-rating_data

# ratings of 4 and 5 are mapped to 1, 
# representing likes, and ratings of 3 
# and below are mapped to -1, representing 
# dislikes:

for(x in 1:nrow(binary_rating))
{
  if(binary_rating[x,3]>3){
    binary_rating[x,3]<-1
  }
  else{
    binary_rating[x,3]<-0
  }
}

# convert binaryratings matrix to the correct format:

binaryrating_new <- dcast(binary_rating, movieId~userId, value.var = "rating", na.rm=FALSE)

for (i in 1:ncol(binaryrating_new)){
  binaryrating_new[which(is.na(binaryrating_new[,i]) == TRUE),i] <- 0 #remove na
}
binaryratings_new = binaryrating_new[,-1] #remove movieIds col. Rows are movieIds, cols are userIds

head(binaryrating_new)
#Remove rows that are not rated from movies dataset

movieId <- length(unique(movie_data$movieId)) #10329

ratingmovieIds <- length(unique(rating_data$movieId)) #10325

movies_new <- movie_data[-which((movie_data$movieId %in% rating_data$movieId) == FALSE),]

rownames(movies_new) <- NULL

#Remove rows that are not rated from genre_matrix2

genre_matrix3 <- genre_mat2[-which((movie_data$movieId %in% rating_data$movieId) == FALSE),]

rownames(genre_matrix3) <- NULL

# calculate the dot product of the genre matrix and 
# the ratings matrix and obtain the user profiles

#Calculate dot product for User Profiles

result = matrix(0,18,668) # Here, 668=no of users/raters, 18=no of genres

for (c in 1:ncol(binaryratings_new)){

    for (i in 1:ncol(genre_matrix3)){
  
        result[i,c] <- sum((genre_matrix3[,i]) * (binaryratings_new[,c])) #ratings per genre
  }
}

#Convert to Binary scale

for (c in 1:ncol(result)){
  for (i in 1:nrow(result)){
    if (result[i,c] < 0){
      result[i,c] <- 0
    }
    else {
      result[i,c] <- 1
    }
  }
}

head(result)

## Assume that users like similar items, and retrieve movies 
# that are closest in similarity to a user's profile, which 
# represents a user's preference for an item's feature.
# use Jaccard Distance to measure the similarity between user profiles
## Jaccard distance calculats the similarity among clusters
# The User-Based Collaborative Filtering Approach


#Create ratings matrix. Rows = userId, Columns = movieId
ratingmat <- dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingmat <- as.matrix(ratingmat[,-1]) #remove userIds

ratingmat <- as(ratingmat, "realRatingMatrix")

head(ratingmat)

# Determine how similar the first four users are with each other
# create similarity matrix
similarity_users <- similarity(ratingmat[1:4, ], 
                               method = "cosine", 
                               which = "users")
as.matrix(similarity_users)
#image(as.matrix(similarity_users), main = "User similarity")

# compute similarity between
# the first four movies
similarity_items <- similarity(ratingmat[, 1:4], method =
                                 "cosine", which = "items")
as.matrix(similarity_items)
#image(as.matrix(similarity_items), main = "Item similarity")

# Exploring values of ratings:
vector_ratings <- as.vector(ratingmat@data)
unique(vector_ratings) # what are unique values of ratings

table_ratings <- table(vector_ratings) # what is the count of each rating value
table_ratings

# Visualize the rating:
vector_ratings <- vector_ratings[vector_ratings != 0] # rating == 0 are NA values
vector_ratings <- factor(vector_ratings)

qplot(vector_ratings) + 
  ggtitle("Distribution of the ratings")

# Exploring viewings of movies:
views_per_movie <- colCounts(ratingmat) # count views for each movie

table_views <- data.frame(movie = names(views_per_movie),
                          views = views_per_movie) # create dataframe of views
table_views <- table_views[order(table_views$views, 
                                 decreasing = TRUE), ] # sort by number of views

ggplot(table_views[1:6, ], aes(x = movie, y = views)) +
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_x_discrete(labels=subset(movies, movies$movieId == table_views$movie)$title) +
  ggtitle("Number of views of the top movies")

#Visualizing the matrix:
image(ratingmat, main = "Heatmap of the rating matrix") # hard to read-too many dimensions
image(ratingmat[1:10, 1:15], main = "Heatmap of the first rows and columns")
image(ratingmat[rowCounts(ratingmat) > quantile(rowCounts(ratingmat), 0.99),
                colCounts(ratingmat) > quantile(colCounts(ratingmat), 0.99)], 
      main = "Heatmap of the top users and movies")


#Normalize the data
ratingmat_norm <- normalize(ratingmat)
image(ratingmat_norm[rowCounts(ratingmat_norm) > quantile(rowCounts(ratingmat_norm), 0.99),
                     colCounts(ratingmat_norm) > quantile(colCounts(ratingmat_norm), 0.99)], 
      main = "Heatmap of the top users and movies")

#Create UBFC Recommender Model. UBCF stands for User-Based Collaborative Filtering
recommender_model <- Recommender(ratingmat_norm, 
                                 method = "UBCF", 
                                 param=list(method="Cosine",nn=30))

model_details <- getModel(recommender_model)
model_details$data

recom <- predict(recommender_model, 
                 ratingmat[1], 
                 n=10) #Obtain top 10 recommendations for 1st user in dataset

recom

#recc_matrix <- sapply(recom@items, 
#                      function(x){ colnames(ratingmat)[x] })
#dim(recc_matrix)

recom_list <- as(recom, 
                 "list") #convert recommenderlab object to readable list

#Obtain recommendations
recom_result <- matrix(0,10)
for (i in 1:10){
  recom_result[i] <- as.character(subset(movies, 
                                         movies$movieId == as.integer(recom_list[[1]][i]))$title)
}


# Evaluation:
evaluation_scheme <- evaluationScheme(ratingmat, 
                                      method="cross-validation", 
                                      k=5, given=3, 
                                      goodRating=5) #k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, 
                               method="UBCF", 
                               n=c(1,3,5,10,15,20))
eval_results <- getConfusionMatrix(evaluation_results)[[1]]
