library(imager)
library(MASS)
library(rvest)
#Problem 1
data(iris) #Loading The dataset

boxplot(iris[, 1:4], col = iris$Species, main = "boxplots of all continuous variables based on the column Species", xlab = "Variables")

plot(iris$Sepal.Length, iris$Petal.Length, col = iris$Species, pch = 16, main = "Scatterplot of Sepal.Length and Petal.Length", xlab = "Sepal.Length", ylab = "Petal.Length")

#Problem 2
flip <- function(image) {
  col.mat <- as.array(image[, ,1, ])
  dims <- dim(col.mat)
  f <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      f[i, j, ] <- col.mat[dims[1] - i + 1, j, ]
    }
  }
  flipped_image<-as.cimg(f)
  return(flipped_image)
}

loaded_image <- load.image("dog.jpeg")
flipped_image <- flip(loaded_image)
plot(flipped_image)

#Problem 3
data(ships)
?ships
accidents_count <- table(ships$incidents)
boxplot(accidents_count, main = "Number of Accidents by Ship Type",
        xlab = "Ship Type", ylab = "Number of Accidents")
#Problem 4
html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
questions <- html %>% html_elements(".s-post-summary--content-title .s-link") %>% html_text()
views <- html %>% html_elements(".is-supernova .s-post-summary--stats-item-number") %>% html_text()
answers <- html %>% html_elements(".has-answers .s-post-summary--stats-item-number") %>% html_text()
votes <- html %>% html_elements(".s-post-summary--stats-item__emphasized .s-post-summary--stats-item-number") %>% html_text() 
stats_data <- data.frame("Questions" = questions, "Views" = views, "Answers" = answers, "Votes"=votes)

