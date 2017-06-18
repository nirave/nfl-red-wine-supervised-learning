library(kernlab)

modellinear<-function(training, cat, dataType, learnType) {
    return(ksvm(as.formula(paste(cat, " ~.")), data = training))
}

modelradial<-function(training, cat, dataType, learnType) {
    return(ksvm(as.formula(paste(cat, " ~.")), data = training,
                         type = "C-bsvc", kernel = "rbfdot",
                         kpar = list(sigma = 0.1), C = 1))
    
}


modellinear_cv<-function(training, cat, dataType, learnType) {
    return(ksvm(as.formula(paste(cat, " ~.")), data = training,
                C = 1, cross=10))
    
}

modelradial_cv<-function(training, cat, dataType, learnType) {
    return(ksvm(as.formula(paste(cat, " ~.")), data = training,
                type = "C-bsvc", kernel = "rbfdot",
                kpar = list(sigma = 0.1), C = 1, cross=10))
    
}

modellinearcc<-function(training, cat) {
   c_values<-c(0.1, 0.5, 1, 5, 10)
   
   min_error<-1.0
   best_c_value<-0.0
   
   for (c in c_values) {
       current_error = ksvm(quality ~ ., data = training,
                            C = c, cross=5)@cross
       if (current_error < min_error) {
           min_error = current_error
           best_c_value = c
       }
   }
   
   return(best_c_value)
    
}


modelradialcc<-function(training, cat) {
    c_values<-c(0.1, 0.5, 1, 5, 10)
    
    min_error<-1.0
    best_c_value<-0.0
    
    for (c in c_values) {
        current_error = ksvm(as.formula(paste(cat, " ~.")), data = training,
                             type = "C-bsvc", kernel = "rbfdot",
                             kpar = list(sigma = 0.1), C = c,
                             cross=5)@cross
        if (current_error < min_error) {
            min_error = current_error
            best_c_value = c
        }
    }
    
    return(best_c_value)
}

