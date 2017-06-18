library(C50)

modelboosting<-function(training, cat, dataType, learnType) {
    return(C5.0(as.formula(paste(cat, " ~.")), data = training, trials=10))
}

modelboosting_cv<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
    param_grid <- expand.grid(trials = seq(from=1, to=100, by=5), winnow = c(FALSE), model='tree')
    trained<-train(as.formula(paste(cat, " ~.")), data=training, trControl=train_control, tuneGrid = param_grid, method="C5.0")
    
    plot_cross(trained$results[3], trained$results[4], dataType, learnType, "trials")
    
    return (trained)
}
