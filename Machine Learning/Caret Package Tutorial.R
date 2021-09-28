#Caret Package

################### Visualizaton ###############
#Data
str(iris)

#Scatterplot Matrix
library(AppliedPredictiveModeling)
transparentTheme(trans = .4)
library(caret)
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 3))

#Scatterplot Matrix with Ellipses
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "ellipse",
            ## Add a key at the top
            auto.key = list(columns = 3))


#Overlayed Density Plots
transparentTheme(trans = .9)
featurePlot(x = iris[, 1:4], 
            y = iris$Species,
            plot = "density", 
            ## Pass in options to xyplot() to 
            ## make it prettier
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")), 
            adjust = 1.5, 
            pch = "|", 
            layout = c(4, 1), 
            auto.key = list(columns = 3))


#Box Plots
featurePlot(x = iris[, 1:4], 
            y = iris$Species, 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(4,1 ), 
            auto.key = list(columns = 2))

#Scatter Plots
#For regression, the Boston Housing data is used:

library(mlbench)
data(BostonHousing)
regVar <- c("age", "lstat", "tax")
str(BostonHousing[, regVar])

#When the predictors are continuous, featurePlot can be used to create scatter plots of each of the predictors with the outcome. For example:
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter", 
            layout = c(3, 1))

#Add a smoother

featurePlot(x = BostonHousing[, regVar], 
            y = BostonHousing$medv, 
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 1))


################### Pre-Processing ###############
#************Creating Dummy Variables

library(earth)
data(etitanic)

head(etitanic)
head(model.matrix(survived ~ ., data = etitanic))

#Or
dummies <- dummyVars(survived ~ ., data = etitanic)
head(dummies)
head(predict(dummies, newdata = etitanic))

#***********Zero- and Near Zero-Variance Predictors

data(mdrr)
data.frame(table(mdrrDescr$nR11))

#Looking at the MDRR data, the nearZeroVar function can be used to identify near zero-variance variables (the saveMetrics argument can be used to show the details and usually defaults to FALSE):
head(mdrrDescr)
nzv <- nearZeroVar(mdrrDescr, saveMetrics= TRUE)
nzv[nzv$nzv,][1:10,]

dim(mdrrDescr)
#By default, nearZeroVar will return the positions of the variables that are flagged to be problematic.
nzv <- nearZeroVar(mdrrDescr)
#This removes them from the data
filteredDescr <- mdrrDescr[, -nzv]
dim(filteredDescr)

#*************** Identifying Correlated Predictors

#Given a correlation matrix, the findCorrelation function uses the following algorithm to flag predictors for removal:
  
descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

#The code chunk below shows the effect of removing descriptors with absolute correlations above 0.75.
descrCor <- cor(filteredDescr)
summary(descrCor[upper.tri(descrCor)])

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75)
filteredDescr <- filteredDescr[,-highlyCorDescr]
descrCor2 <- cor(filteredDescr)
summary(descrCor2[upper.tri(descrCor2)])

#**************** Linear Dependencies
#The function findLinearCombos uses the QR decomposition of a matrix to enumerate sets of linear combinations (if they exist). For example, consider the following matrix that is could have been produced by a less-than-full-rank parameterizations of a two-way experimental layout:
ltfrDesign <- matrix(0, nrow=6, ncol=6)
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)


#Note that columns two and three add up to the first column. Similarly, columns four, 
#five and six add up the first column. findLinearCombos will return a list that enumerates 
#these dependencies. 
#For each linear combination, it will incrementally remove columns from the matrix and test to see if the dependencies have been resolved. findLinearCombos will also return a vector of column positions can be removed to eliminate the linear dependencies:
  
comboInfo <- findLinearCombos(ltfrDesign)
comboInfo

ltfrDesign[, -comboInfo$remove]

#***************** Centering and Scaling **********

set.seed(96)
inTrain <- sample(seq(along = mdrrClass), length(mdrrClass)/2)

training <- filteredDescr[inTrain,]
test <- filteredDescr[-inTrain,] #What does this "-" mean

trainMDRR <- mdrrClass[inTrain]
testMDRR <- mdrrClass[-inTrain]

preProcValues <- preProcess(training, method = c("center", "scale"))

trainTransformed <- predict(preProcValues, training)
testTransformed <- predict(preProcValues, test)

#**************** Putting It All Together ******************

#In Applied Predictive Modeling there is a case study where the execution times of jobs in a high performance computing environment are being predicted. The data are:
  
library(AppliedPredictiveModeling)
data(schedulingData)
str(schedulingData)

#Suppose we want to use the Yeo-Johnson transformation on the continuous predictors then center and scale them. 
#We run the function on all the columns except the last, which is the outcome.

pp_hpc <- preProcess(schedulingData[, -8], 
                     method = c("center", "scale", "YeoJohnson"))
pp_hpc

#These are not altered but the numeric predictors are transformed. 
#However, the predictor for the number of pending jobs, has a very sparse and unbalanced distribution:
transformed <- predict(pp_hpc, newdata = schedulingData[, -8])
head(transformed)



################### Data Splitting ##################

#************ Simple Splitting Based on the Outcome
#The function createDataPartition can be used to create balanced splits of the data.
#For example, to create a single 80/20% split of the iris data:
library(caret)
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
head(trainIndex) #This extracts the row numbers 

irisTrain <- iris[ trainIndex,]
head(irisTrain)

irisTest  <- iris[-trainIndex,] 
#The negative sign takes all the row numbers that are not those values and subsets those
head(irisTest)

#************ Splitting Based on the Predictors
library(mlbench)
data(BostonHousing)

testing <- scale(BostonHousing[, c("age", "nox")])
testing

set.seed(5)
## A random sample of 5 data points
startSet <- sample(1:dim(testing)[1], 5)
samplePool <- testing[-startSet,]
start <- testing[startSet,]

# the function maxDissim can be used to create sub-samples using a maximum dissimilarity 
# Suppose there is a data set A with m samples and a larger data set B with n samples. We may want to create a sub-sample from B that is diverse when compared to A. To do this, for each sample in B, the function calculates the m dissimilarities between each point in A. The most dissimilar point in B is added to A and the process continues. 
library(proxy)
newSamp <- maxDissim(start, samplePool, n = 20)
head(newSamp)

#************** Data Splitting for Time Series

#caret contains a function called createTimeSlices 
#that can create the indices for this type of splitting.

# see Caret Internet Page

#************* Simple Splitting with Important Groups
#Create Data
set.seed(3527)
subjects <- sample(1:20, size = 80, replace = TRUE)
table(subjects)

#To split the data based on groups, groupKFold can be used:
folds <- groupKFold(subjects, k = 15) 
folds
  #The results in folds can be used as inputs into the index argument of the trainControl function.


############  Model Training and Tuning  ###############

#***************An Example
#The Sonar data are available in the mlbench package. Here, we load the data:
library(mlbench)
data(Sonar)
str(Sonar[, 1:10])

#The function createDataPartition can be used to create a stratified random sample of the data into training and test sets
library(caret)
set.seed(998)
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)
training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

#*************Basic Parameter Tuning

#The function trainControl can be used to specifiy the type of resampling:
  #Options Include: Others are available, such as repeated K-fold cross-validation, leave-one-out etc.

fitControl <- trainControl(## 10-fold CV
                              method = "repeatedcv", #This is were we specify the type
                              number = 10,
                              ## repeated ten times
                              repeats = 10)

#*************** The Model
# The first two arguments to train are the predictor and outcome data objects, respectively. 
# The third argument, method, specifies the type of model (see train Model List or train Models By Tag).
# To illustrate, we will fit a boosted tree model via the gbm package. 
# The basic syntax for fitting this model using repeated cross-validation is shown below:

library("e1071")
set.seed(825)
gbmFit1 <- train(Class ~ ., data = training,  #The first two arguments to train are the predictor and outcome data objects, respectively. 
                 method = "gbm",              #The third argument, method, specifies the type of model
                 trControl = fitControl,      #specifies the type of resampling:
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
gbmFit1


rfFit1 <- train(Class ~ ., data = training,  #The first two arguments to train are the predictor and outcome data objects, respectively. 
                 method = "ranger",              #The third argument, method, specifies the type of model
                 trControl = fitControl,      #specifies the type of resampling:
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
rfFit1

#********************* Alternate Tuning Grids

#For the boosted tree model, we can fix the learning rate and evaluate more than three values of n.trees
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2

#****************** Plotting the Resampling Profile


trellis.par.set(caretTheme())
plot(gbmFit2)
ggplot(gbmFit2)  


#****************** Alternate Performance Metrics
# The user can change the metric used to determine the best settings. 
# By default, RMSE, R2, and the mean absolute error (MAE) are computed for regression while accuracy and Kappa are computed for classification. 
# Also by default, the parameter values are chosen using RMSE and accuracy, respectively for regression and classification. 
# The metric argument of the train function allows the user to control which the optimality criterion is used. 

head(twoClassSummary)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- train(Class ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = FALSE, 
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
head(gbmFit3$results)

gbmFit3$finalModel

#****************** Choosing the Final Model

# Another method for customizing the tuning process is to modify the algorithm that is used to select the "best" parameter values, given the performance numbers. 
# By default, the train function chooses the model with the largest performance value (or smallest, for mean squared error in regression models). 

# Breiman et al (1984) suggested the "one standard error rule" for simple tree-based models. 
# In this case, the model with the best performance value is identified and, using resampling, we can estimate the standard error of performance. 
# The final model used was the simplest model within one standard error of the (empirically) best model. 
# With simple trees this makes sense, since these models will start to over-fit as they become more and more specific to the training data.


#The tolerance function could be used to find a less complex model based on (x-xbest)/xbestx 100, which is the percent difference. 
#For example, to select parameter values based on a 2% loss of performance:

whichTwoPct <- tolerance(gbmFit3$results, metric = "ROC", 
                         tol = 2, maximize = TRUE)  
cat("best model within 2 pct of best:\n")
gbmFit3$results[whichTwoPct,1:6]
  #This indicates that we can get a less complex model with an area under the ROC curve of 0.901 (compared to the "pick the best" value of 0.914).


#****************** Note On the Complexity of the Model
#In the case of boosted trees, the package assumes that increasing the number of iterations adds complexity at a faster rate than increasing the tree depth, so models are ordered on the number of iterations then ordered with depth. 

#***************** Extracting Predictions and Class Probabilities
#For predict.train, the type options are standardized to be "class" and "prob" (the underlying code matches these to the appropriate choices for each model. 
#For example:

predict(gbmFit3, newdata = head(testing))

predict(gbmFit3, newdata = head(testing), type = "prob")

#***************** Exploring and Comparing Resampling Distributions
#Within Model Graphs

#Set the plotting Theme
trellis.par.set(caretTheme())

#Density plot of ROC's in the Model
densityplot(gbmFit3, pch = "|") #A density plot of an ROC curve

#*********** Measureing Perfromance between Models
set.seed(825)
svmFit <- train(Class ~ ., data = training, 
                method = "svmRadial", 
                trControl = fitControl, 
                preProc = c("center", "scale"),
                tuneLength = 8,
                metric = "ROC")

svmFit

#Also, a regularized discriminant analysis model was fit.
set.seed(825)
rdaFit <- train(Class ~ ., data = training, 
                method = "rda", 
                trControl = fitControl, 
                tuneLength = 4,
                metric = "ROC")
rdaFit   


#Given these models, can we make statistical statements about their performance differences? 
#To do this, we first collect the resampling results using resamples.

resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps

summary(resamps)

#************ PLOTS 
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))