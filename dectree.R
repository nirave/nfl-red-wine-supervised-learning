library(RWeka)

modeldecisiontree_cv<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
    param_grid <- expand.grid(C = c(0.01, 0.05, .1,.2,.3,.4))
    trained<-train(as.formula(paste(cat, " ~.")), data=training, trControl=train_control, tuneGrid = param_grid, method="J48")
    
    plot_cross(trained$results[1], trained$results[2], dataType, learnType, "c")

    return (trained)
}

modeldecisiontree<-function(training, cat, dataType, learnType) {
    return(J48(as.formula(paste(cat, " ~.")), data = training, control = Weka_control(M=2,U=FALSE)))
}


