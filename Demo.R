# str(iris)
# # View(iris)
# dim(iris)
# set.seed(1234)
# ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.7, 0.3))
# trainData <- iris[ind==1,]
# testData <- iris[ind==2,]
# myFormula <- Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
# iris_ctree <- ctree(myFormula, data=trainData)
# table(predict(iris_ctree), trainData$Species)
# print(iris_ctree)
# plot(iris_ctree)
# plot(iris_ctree, type="simple")
# testPred <- predict(iris_ctree, newdata = testData)
# table(testPred, testData$Species)
data("bodyfat", package="TH.data")
dim(bodyfat)
attributes(bodyfat)
bodyfat[1:5,]
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))
attributes(bodyfat_rpart)
summary(bodyfat_rpart)
print(bodyfat_rpart)
print(bodyfat_rpart$cptable)
plot(bodyfat_rpart)
text(bodyfat_rpart, use.n=T)