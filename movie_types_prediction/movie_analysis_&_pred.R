library(dplyr)
library(haven)


# loading the dataset
movie <- read_sas("movies.sas7bdat")
View(movie)
str(movie)

#checking for colnms
names(movie)

#selecting useful variables and renaming
movie <- movie %>% select(Type, Year, Domestic__, Worldwide__)
View(movie)

movie <- movie %>% rename(movie_type = Type, domestic_sale = Domestic__, 
                          worldwide_sale = Worldwide__)
View(movie)


# checking fior data type
sapply(movie, class)

# change year datatype
movie$Year <- as.integer(movie$Year)

sapply(movie, class)

# getting the movie types
uni <- unique(movie$movie_type)
uni


#------------------------------------------------------------
sta <- movie %>% group_by(movie_type) %>% summarise(mean = mean(domestic_sale), sd = sd(domestic_sale, na.rm = TRUE))
sta

library(tidyverse)
p <- movie %>% ggplot(aes(movie_type, domestic_sale, fill = movie_type)) + geom_boxplot()
p


#------------------Clustering Analysis------------------------

# scatter plot
plot(domestic_sale ~ worldwide_sale, movie)
with(movie,text(domestic_sale~worldwide_sale, labels = movie_type, pos =4, cex=.20))


# normalization
# normalizing variable is by substracting mean from the obsevations and divide by standard deviation

movie_dat <- movie[, 3:4] # taking only the quantitative columns
movie_dat
# scale the data
movie_data_scal <- scale(movie_dat)
movie_data_scal



# calculate the eclidean distance
distance <- dist(movie_data_scal)
distance

#----Hierarchical clustering using dendrogram------------
# cluster Dendrogram with complete linkage:
hc.c <- hclust(distance)
hc.c
plot(hc.c, cex = 0.85, main = "", xlab = "")

plot(hc.c, labels = movie$movie_type, hang = -1)  # cluster dendrogram


# silhouette plot
library(cluster)
plot(silhouette(cutree(hc.c,3), distance))


#-------K-Means clustering-------------------------

# determine the value of K where k is optimal, i.e cal how amny clusters we need
# wss= within sum of squares
library(factoextra)

fviz_nbclust(movie_data_scal, kmeans, method = "wss") + labs(subtitle = "elbow plot")

# kmeans
km <- kmeans(movie_data_scal, centers = 3)
print(km)

km$cluster

#visualize the clusters
km.cluster <- km$cluster
rownames(movie_data_scal) <- paste(movie$movie_type, 1:dim(movie)[1], sep = "_")
fviz_cluster(list(movie_data_scal, cluster = km.cluster))

table(km.cluster, movie$movie_type)


library(ade4)
s.class(movie_data_scal, fac = factor(km.cluster), add.plot = TRUE)
#--------------------------------------------------------------------------
#putting the cluster indentifier as part of the original dataset

movie_dat$cluster <- km.cluster
movie_dat


# building the predictive model
library(caret)
set.seed(20000)
movie_dat$cluster <- factor(movie_dat$cluster)
trainingindex <- createDataPartition(movie_dat$cluster, p=0.8, list = FALSE)
trainingset <- movie_dat[trainingindex,]
testset <- movie_dat[-trainingindex,]
trainingset


set.seed(2344)
control <- trainControl(method = "cv", number = 10, p=.9)
fit_knn_cv <- train(cluster ~., method = "knn", data = trainingset,
                      tuneGrid = data.frame(k=seq(3,33,2)), 
                      trControl = control,
                      preProc = c("center", "scale"))

fit_knn_cv
plot(fit_knn_cv, highlight = TRUE)

# looking at variable importance
varImp(fit_knn_cv)


#apply model for prediction
pred <- predict(fit_knn_cv, testset)
pred

# confusion matrix

test_cm <- confusionMatrix(pred, testset$cluster)
test_cm

test_cm$overall["Accuracy"]

# random forest
install.packages("randomForest")
library(randomForest)
set.seed(2333)
contro1 <- trainControl(method = "cv", number = 10, p=.9)
fit_rf <- randomForest(cluster ~., data = trainingset)

fit_rf
varImp(fit_rf)
plot(fit_rf)

# apply the model to make prediction
pred1 <- predict(fit_rf, testset)
pred1

#confusion matrix
confu <- confusionMatrix(pred1, testset$cluster)
confu
# overall accuracy
confu$overall["Accuracy"]
