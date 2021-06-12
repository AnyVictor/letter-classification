# Configurar Dataset ------------------------------------------------------

require(tools)
dataset = read.csv('C:/Users/dest1/Documents/Projects/letter-classification/letter-recognition.csv')

nrow(dataset) ## Verificar numero de linhas
ncol(dataset) ## Verificar numero de colunas

##Dar nome para cada coluna
names(dataset) <- c("letter", "xbox", "ybox", "width", "height", "onpix", "xbar", 
                    "ybar", "x2bar", "y2bar", "xybar", "x2ybr", "xy2br", "xege", "xegvy", "yege", "yegvx")
View(dataset)

# Boxplot -----------------------------------------------------------------

boxplot(data = dataset, xbox~letter)
boxplot(data = dataset, ybox~letter)
boxplot(data = dataset, width~letter)
boxplot(data = dataset, height~letter)
boxplot(data = dataset, onpix~letter)
boxplot(data = dataset, xbar~letter)
boxplot(data = dataset, ybar~letter)

# KNN ---------------------------------------------------------------------

library(class)

##Separar 80% do database
set.seed(150)
trainingIndex <- sample(1:nrow(dataset), 0.8*nrow(dataset))

##Criar training set e data set
trainingSet <- dataset[trainingIndex,]
View(trainingSet)

testSet <- dataset[-trainingIndex,]
View(testSet)

##Separar coluna de classificação
trainingClass <- trainingSet[, 1]
trainingSet <- trainingSet[, -1]

testClass <- testSet[, 1]
testSet <- testSet[, -1]

##Criar modelo e realizar o knn
model <- knn(train = trainingSet, test = testSet, cl = trainingClass, k = 3)
table(model)

##Mostrar resultados
tableMatrix = table(model, testClass)
tableMatrix

hit = sum(diag(tableMatrix))
total = sum(tableMatrix)
hit/total

# Árvore de classificação -------------------------------------------------

library(rpart)
dataset_tree <- rpart(letter ~ xbox + ybox + width + height + onpix + xbar + ybar + x2bar + y2bar 
                      + xybar + x2ybr + xy2br + xege + xegvy + yege + yegvx , method = "class", data = dataset)

library(rpart.plot)
rpart.plot(dataset_tree, type=0)



