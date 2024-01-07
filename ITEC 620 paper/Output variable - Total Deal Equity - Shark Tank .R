# Importing data
full <- read.csv(file.choose())

# Clean and prepare the data
# Dummy
library(fastDummies)
full <- dummy_cols(full, select_columns='Industry')
full <- dummy_cols(full, select_columns='Pitchers.Gender')

full <- as.data.frame(full)

# Data cleanup
full<-full[,-(match("Industry", colnames(full)))]
full<-full[,-(match("Pitchers.Gender", colnames(full)))]
full<-full[,-(match("Industry_Uncertain/Other", colnames(full)))]
full<-full[,-(match("Pitchers.Gender_Mixed Team", colnames(full)))]
full<-full[,-(match("Pitchers.State", colnames(full)))]

# NA check
sum(is.na(full))

# Column name transformation
colnames(full) <- make.names(colnames(full))

# Partitioning

# Inputs
y <- 'Total.Deal.Equity' # input y column name
training_size <- 0.6
set.seed(12345)

#Program
full <- as.data.frame(full)
ycol <- match(y, colnames(full))
training.rows <- sample(1:nrow(full), training_size*nrow(full))

training <- full[training.rows,]
training.x <- full[training.rows, -ycol]
training.y <- full[training.rows, ycol]

test <- full[-training.rows,]
test.x <- full[-training.rows, -ycol]
test.y <- full[-training.rows, ycol]  


#MLR Interaction
MLRInteractionmodel <- lm(Total.Deal.Equity ~ . + Original.Ask.Amount*Original.Offered.Equity -Got.Deal -Valuation.Requested, data=training)
summary(MLRInteractionmodel)

# With only the significant independent variables
MLRInteractionmodel <- lm(Total.Deal.Equity ~ Original.Ask.Amount + Original.Offered.Equity + Total.Deal.Amount + Number.of.sharks.in.deal + Royalty.Deal + Loan, data=training)
summary(MLRInteractionmodel)

# knn
# What is its RMSE on the test set?
library(FNN)
max.k = round(sqrt(nrow(full)),0)
best.k <- -1
RMSE <- -1
best.RMSE <- 999999
set.seed(12345)
full <- scale(full)

for (i in 1:max.k) {
  full.knn <- knn.reg(training.x, test.x, training.y, k=i)
  RMSE <- sqrt(mean((full.knn$pred - test.y)^2))
  if (RMSE < best.RMSE) {
    best.k <- i
    best.RMSE <- RMSE
  }
}
best.k
knn.rmse <- best.RMSE
knn.rmse

full.knn <- knn.reg(training.x, test.x, training.y, k=best.k)
summary(full.knn)

# Regression Tree
library(tree)
full <- full[,-1]
training <- training[,-1]
training.x <- training.x[,-1]
test <- test[,-1]
test.x <- test.x[,-1]

tree <- tree(Total.Deal.Equity ~ . -Got.Deal -Valuation.Requested, data=training)

# What is the RMSE of this model on the test set?
tree.predictions <- predict(tree, test.x)
tree.rmse <- sqrt(mean((tree.predictions - test.y)^2))
tree.rmse
plot(tree)
text(tree)
