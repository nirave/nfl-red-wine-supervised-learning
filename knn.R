#suppressWarnings(suppressMessages(library(kknn)))
library(class)

modelknn<-function(training, cat, dataType, learnType) {
    trained<-train(as.formula(paste(cat, " ~.")), data=training, method="knn")
    
    return (trained)
}

modelknn_cv<-function(training, cat, dataType, learnType) {
    train_control <- trainControl(method="repeatedcv", number=10, repeats=3)
    param_grid <- expand.grid(k = seq(from=1, to=10, by=1))
    trained<-train(as.formula(paste(cat, " ~.")), data=training, trControl=train_control, tuneGrid = param_grid, method="knn")
    
    plot_cross(trained$results[1], trained$results[2], dataType, learnType, "kmax")
    
    return (trained)
}
