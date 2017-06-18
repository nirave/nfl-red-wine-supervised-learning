source("wine.R")
source("nfl.R")
source("decTree.R")
source("nnet.R")
source("boost.R")
source("svn.R")
source("knn.R")
source("plot.R")

machineLearn<-function(data, validation, category, dataType, learnType, plotTitle, xLabel, learnfunc, no_cvlearnfunc, is_class = FALSE) {
    # Start the clock!
    ptm <- proc.time()
    
    model<-learnfunc(data, category, dataType, learnType)
    
    # Stop the clock
    modelTime<-(proc.time() - ptm)[1]
    print(model)    
    trainingError = 0.0

    if (is_class) {
        trainingError = 1 - confusionMatrix(predict(model, data, type="class"), data[[category]])$overall['Accuracy']
        
    } else { 
        trainingError = 1 - confusionMatrix(predict(model, data), data[[category]])$overall['Accuracy']
    }

    # Start the clock!
    ptm <- proc.time()
    
    predictions = NULL
    
    if (is_class) {
        predictions <- predict(model, validation, type="class")
    } else {
        predictions <- predict(model, validation)
    }
    
    # Stop the clock
    predictTime<-(proc.time() - ptm)[1]
    accuracy = confusionMatrix(predictions, validation[[category]])$overall['Accuracy']
    print(confusionMatrix(predictions, validation[[category]])$overall['Accuracy'])
    
    png(filename=paste(dataType, " ", learnType, ".png"))
    counts<-rbind(table(validation[[category]]), table(predictions))
    barplot(counts, main=plotTitle,
            xlab=xLabel, col=c("darkblue","red"),
            legend = c("Actual", "Predicted"), beside=TRUE)
    dev.off()
    
    #If this is cross valdiation, to get the model time, run it WITHOUT cross validation
    if (!is.null(no_cvlearnfunc)) {
        # Start the clock!
        ptm <- proc.time()
        
        model<-no_cvlearnfunc(data, category, dataType, learnType)
        
        # Stop the clock
        modelTime<-(proc.time() - ptm)[1]
        
    }
    
    
    print("Time to model is ")
    print(modelTime)
    
    print("Time to predict all is ")
    print(predictTime)
    
    print("Training error is ")
    print(trainingError)

    return (c(dataType, learnType, trainingError, accuracy, modelTime, predictTime))    
}


#TO DO - Learning curve (number of data points)

#TO DO - Possible hyperameter via cross validation

#Decision Tree
r<-machineLearn(training, validation, "quality", "Red Wine", "Decicion Tree", "Red Wine Quality, Prediction vs Read in Dec Tree", "Red Wine Quality", modeldecisiontree_cv, modeldecisiontree)
nr<-machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "Decision Tree", "NFL Wins, Prediction vs Read in Dec Tree", "NFL Wins", modeldecisiontree, NULL)
#Plot the learning curve
#suppressWarnings(plot_learning(training, "quality", modeldecisiontree_cv, "Red Wine", "Decision Tree", FALSE))

#Neural Network
r<-rbind(r,machineLearn(training, validation, "quality", "Red Wine", "Neural Network", "Red Wine Quality, Prediction vs Read in Neural Network", "Red Wine Quality", modelnnet_cv, modelnueralnet, FALSE))
nr<-rbind(nr, machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "Neural Network", "NFL Wins, Prediction vs Read in Neural Network", "NFL Wins", modelnueralnet_nfl, NULL, FALSE))
#Plot the learning curve
#suppressWarnings(plot_learning(training, "quality", modelnnet_cv, "Red Wine", "Neural Network", FALSE, 3))

#Boosting
r<-rbind(r,machineLearn(training, validation, "quality", "Red Wine", "Boosted Decicion Tree", "Red Wine Quality, Prediction vs Read in Boosted Decision Tree", "Red Wine Quality", modelboosting_cv, modelboosting))
nr<-rbind(nr,machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "Boosted Decision Tree", "NFL Wins, Prediction vs Read in Boosted Deciscion Tree", "NFL Wins", modelboosting, NULL))
#Plot the learning curve
suppressWarnings(plot_learning(training, "quality", modelboosting_cv, "Red Wine", "Boosted Decicion Tree", FALSE, 4))

# #SVM - Linear
r<-rbind(r,machineLearn(training, validation, "quality", "Red Wine", "SVM Linear", "Red Wine Quality, Prediction vs Read in SVM Linear", "Red Wine Quality", modellinear_cv, modellinear, FALSE))
nr<-rbind(nr,machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "SVM Linear", "NFL Wins, Prediction vs Read in SVM Linear", "NFL Wins", modellinear, NULL, FALSE))

# #SVM - Radial
r<-rbind(r,machineLearn(training, validation, "quality", "Red Wine", "SVM Radial", "Red Wine Quality, Prediction vs Read in SVM Radial", "Red Wine Quality", modelradial_cv, modelradial, FALSE))
nr<-rbind(nr,machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "SVM Radial", "NFL Wins, Prediction vs Read in SVM Radial", "NFL Wins", modelradial, NULL, FALSE))

#KNN
r<-rbind(r,machineLearn(training, validation, "quality", "Red Wine", "K Nearest Neighbors", "Red Wine Quality, Prediction vs Read in K Nearest Neighbors", "Red Wine Quality", modelknn_cv, modelknn))
nr<-rbind(nr,machineLearn(nfl_training, nfl_validation, "WinOrLose", "NFL Wins", "K Nearest Neighbors", "NFL Wins, Prediction vs Read in K Nearest Neighbors", "NFL Wins", modelknn, NULL))
#Plot the learning curve
#suppressWarnings(plot_learning(training, "quality", modelknn_cv, "Red Wine", "KNN", FALSE))

#Graphs

#Training/Validation Histogram
png(filename="wine_training_validation.png")
counts<-rbind(table(training[["quality"]]), table(validation[["quality"]]))
barplot(counts, main="Training vs Validation",
        xlab="Wine Quality", ylab="Count", col=c("darkblue","red"),
        legend = c("Training", "Validation"), beside=TRUE)
dev.off()

png(filename="nfl_training_validation.png")
counts<-rbind(table(nfl_training[["WinOrLose"]]), table(nfl_validation[["WinOrLose"]]))
barplot(counts, main="Training vs Validation",
        xlab="NFL Win or Loss", ylab="Count", col=c("darkblue","red"),
        legend = c("Training", "Validation"), beside=TRUE)
dev.off()

rd<-data.frame(r)
nrd<-data.frame(nr)

rd[2]<-c("DT", "NN", "BOOST", "SVM-L", "SVM-R", "KNN")
nrd[2]<-c("DT", "NN", "BOOST", "SVM-L", "SVM-R", "KNN")

png(filename=paste("wine_overall_accuracy.png"))
barplot(as.vector(as.numeric(as.vector(rd[[4]]))), main="Accuracy for Red Wine Score Predictions", xlab = "Learning Method", ylab="Accuracy", names.arg = as.vector(rd[[2]]), ylim=c(0.5, 0.75), xpd=FALSE)
dev.off()

png(filename=paste("nfl_overall_accuracy.png"))
barplot(as.vector(as.numeric(as.vector(nrd[[4]]))), main="Accuracy for NFL Wins/Losses Predictions", xlab = "Learning Method", ylab="Accuracy", names.arg = as.vector(nrd[[2]]), ylim=c(0.5, 0.75), xpd=FALSE)
dev.off()

png(filename=paste("wine_training_error.png"))
barplot(as.vector(as.numeric(as.vector(rd[[3]]))), main="Accuracy for Red Wine Score Training Error", xlab = "Learning Method", ylab="Accuracy", names.arg = as.vector(rd[[2]]), ylim=c(0.0, 0.4), xpd=FALSE)
dev.off()

png(filename=paste("nfl_training_error.png"))
barplot(as.vector(as.numeric(as.vector(nrd[[3]]))), main="Accuracy for NFL Wins/Losses Training Error", xlab = "Learning Method", ylab="Accuracy", names.arg = as.vector(nrd[[2]]), ylim=c(0.0, 0.4), xpd=FALSE)
dev.off()

png(filename=paste("wine_model_time.png"))
barplot(as.vector(as.numeric(as.vector(rd[[5]]))), main="Red Wine Training Time (no cv)", xlab = "Learning Method", ylab="Seconds", names.arg = as.vector(rd[[2]]), ylim=c(0.0, 3.0), xpd=FALSE)
dev.off()

png(filename=paste("nfl_model_time.png"))
barplot(as.vector(as.numeric(as.vector(nrd[[5]]))), main="NFL Model Training Time (no cv)", xlab = "Learning Method", ylab="Seconds", names.arg = as.vector(nrd[[2]]), ylim=c(0.0, 3.0), xpd=FALSE)
dev.off()

png(filename=paste("wine_prediection_time.png"))
barplot(as.vector(as.numeric(as.vector(rd[[6]]))), main="Red Wine Score Prediection Time", xlab = "Learning Method", ylab="Seconds", names.arg = as.vector(rd[[2]]), ylim=c(0.0, 0.3), xpd=FALSE)
dev.off()

png(filename=paste("nfl_prediection_time.png"))
barplot(as.vector(as.numeric(as.vector(nrd[[6]]))), main="NFL Model Prediection Time", xlab = "Learning Method", ylab="Seconds", names.arg = as.vector(nrd[[2]]), ylim=c(0.0, 0.3), xpd=FALSE)
dev.off()
