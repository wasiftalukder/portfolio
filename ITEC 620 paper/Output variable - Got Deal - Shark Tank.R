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

#DESCRIPTIVE ANALYTICS
summary(full)

# Correlation
cor(full)
plot(cor(full))
cor(full[, c("Original.Ask.Amount", "Total.Deal.Amount", "Valuation.Requested","Loan","Number.of.sharks.in.deal","Got.Deal")])

# Outcome variable: Got Deal
# Partitioning

# Inputs
y <- 'Got.Deal' # input y column name
training_size <- 0.6
set.seed(12345)

#Program
full <- as.data.frame(full)
full$Got.Deal <- full$Got.Deal > 0.5
full$Loan <- full$Loan > 0.5
ycol <- match(y, colnames(full))
training.rows <- sample(1:nrow(full), training_size*nrow(full))

training <- full[training.rows,]
training.x <- full[training.rows, -ycol]
training.y <- full[training.rows, ycol]

test <- full[-training.rows,]
test.x <- full[-training.rows, -ycol]
test.y <- full[-training.rows, ycol]

# Classification tree
library(tree)

best.mindev <- -1
error.rate <- -1
best.error.rate <- 99999999
for (i in seq(from=0.0005, to=0.05, by=0.0005)) {
  tree <- tree(Got.Deal ~ . -Loan -Royalty.Deal -Total.Deal.Amount -Valuation.Requested -Total.Deal.Equity -Number.of.sharks.in.deal, data=training, mindev=i)
  tree.predictions <- predict(tree,test.x)
  tree.classifications <- round(tree.predictions, 0)
  error.rate <- 1 -(sum(tree.classifications == test.y) / nrow(test))
    if (error.rate < best.error.rate) {
    best.mindev <- i
    best.error.rate <- error.rate
  }
}
best.mindev
tree.error.rate <- best.error.rate
tree.error.rate
prediction.accuracy <- 1- tree.error.rate
prediction.accuracy

tree <- tree(Got.Deal ~ . -Loan -Royalty.Deal-Total.Deal.Amount -Valuation.Requested -Total.Deal.Equity -Number.of.sharks.in.deal, data=training, mindev=best.mindev)
plot(tree)
text(tree)

# Logistic 
lrm <- glm(Got.Deal ~ . -Total.Deal.Amount -Valuation.Requested -Total.Deal.Equity -Number.of.sharks.in.deal, family = binomial (link="logit"), data=training)
summary(lrm)

lrm.predict <- predict(lrm, test.x, type="response")
lrm.predict

lrm.classification <- round(lrm.predict, 0) > 0.5
lrm.classification

# prediction accuracy
sum(lrm.classification==test.y) / length(test.y)

# Confusion Matrix
table(lrm.classification, test.y)

