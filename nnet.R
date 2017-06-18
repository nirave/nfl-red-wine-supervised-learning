library(nnet)

modelnnet_cv<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="repeatedcv", number=5, repeats=1)
    param_grid <- expand.grid(size = c(1,5,10,15,20,25,30), decay=0)
    
    #train_control <- trainControl(method="repeatedcv", number=10, repeats=1)
    #param_grid <- expand.grid(size = c(5,20), decay=0)
    
    trained<-train(as.formula(paste(cat, " ~.")), data=training, trControl=train_control, tuneGrid = param_grid, method="nnet")
    
    plot_cross(trained$results[1], trained$results[3], dataType, learnType, "size")
    
    return (trained)
}

modelnueralnet_cv<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="none")
    
    param_grid <- expand.grid(size = 25, decay=0)
    
    trained<-train(as.formula(paste(cat, " ~.")),  trControl=train_control, tuneGrid = param_grid, data=training, method="nnet")
    
    return (trained)
}

modelnueralnet<-function(training, cat, dataType, learnType) {
    ideal <-class.ind(training[[cat]])
    nntraining<-subset(training, select = -c(quality))
    
    nueral_net<-nnet(nntraining, ideal, size=30, softmax=TRUE)

    return(nueral_net)
}

modelnueralnet_nfl<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="none")
    
    param_grid <- expand.grid(size = 25, decay=0)
    
    trained<-train(as.formula(paste(cat, " ~.")),  trControl=train_control, tuneGrid = param_grid, data=training, method="nnet")

    return (trained)
}

