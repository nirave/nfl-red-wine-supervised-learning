plot_learning<-function(training, cat, learnfunc, dataType, learnType, is_class, acc_index=2) {
    rnums<-seq(from=0.05,to=1.0,by=.20)
    
    learningCurve<-data.frame(observations= integer(length(rnums)-1), accuracies= integer(length(rnums)-1))
    learningCurve_cv<-data.frame(observations= integer(length(rnums)-1), accuracies= integer(length(rnums)-1))
    
    i<-0
    for (num in rnums) {
        inTrain <- createDataPartition(training[[cat]], p = num)[[1]]
        subtraining = training[inTrain,]
        model<-learnfunc(subtraining, cat, dataType, learnType)
        if (is_class) {
            trainingError = confusionMatrix(predict(model, subtraining, type="class"), subtraining[[cat]])$overall['Accuracy']
            
        } else { 
            trainingError = confusionMatrix(predict(model, subtraining), subtraining[[cat]])$overall['Accuracy']
        }   
        
        learningCurve$observations[i] = as.integer(num * nrow(training))
        learningCurve_cv$observations[i] = as.integer(num * nrow(training))
        
        learningCurve$accuracies[i] = 1-trainingError
        learningCurve_cv$accuracies[i] = 1-mean(model$results[[acc_index]])
        
        
        i<-i+1
    }
    
    png(filename=paste(dataType, " ", learnType, "_learning_curve.png"))
    lo <- loess(learningCurve$accuracies~learningCurve$observations)
    temp<-cbind(learningCurve$observations,predict(lo))
    plot(temp, type="n", main=paste("Learning Curve for ", dataType, learnType), xlab = "Number of Samples", ylab="Accuracy", ylim=c(0.0, 0.7))
    lines(temp, col="red")
    lo_cv <- loess(learningCurve_cv$accuracies~learningCurve_cv$observations)
    temp<-cbind(learningCurve_cv$observations,predict(lo_cv))
    lines(temp, col="blue")
    legend(400, .1, c("CV", "Training Error"), lty=1, col=c("blue", "red"))
    dev.off()
    
}

plot_cross<-function(x_values, y_values, dataType, learnType, xlabel) {
    png(filename=paste(dataType, " ", learnType, "_cross.png"))
    #accuracies<-cbind(trained$results[[1]],trained$results[[2]])
    
    accuracies<-cbind(x_values, y_values)
    plot(accuracies, type="o", main=paste(dataType, learnType), xlab = xlabel, ylab="Accuracy")
    dev.off()
}