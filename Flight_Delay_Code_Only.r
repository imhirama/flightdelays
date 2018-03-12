### Note: for further description and explanation, see the Jupyter notebook included in this repository 

suppressWarnings(library(chron))

###Read in data for basic model
basicvars = read.csv('basic.csv')

###Set variable types for those that aren't correctly set automatically
basicvars$XX_MonthAndDay = as.Date(basicvars$XX_MonthAndDay, "%m/%d/%Y")
basicvars$X_DepTimeBlk = chron(times = basicvars$X_DepTimeBlk)
basicvars$X_ArrTimeBlk = chron(times = basicvars$X_ArrTimeBlk)
basicvars$X_DestAirportID = as.factor(basicvars$X_DestAirportID)
basicvars$X_OriginAirportID = as.factor(basicvars$X_OriginAirportID)
basicvars$XX_PeakDate = as.factor(basicvars$XX_PeakDate)
basicvars$X_Month = as.factor(basicvars$X_Month)

dim(basicvars)

head(basicvars)

###Read in data for day-of model
dayofvars = read.csv('dayof.csv')

###Set variable types for those that aren't correctly set automatically
dayofvars$XX_MonthAndDay = as.Date(dayofvars$XX_MonthAndDay, "%m/%d/%Y")
dayofvars$X_ArrTimeBlk = chron(times = dayofvars$X_ArrTimeBlk)
dayofvars$X_DepTimeBlk = chron(times = dayofvars$X_DepTimeBlk)
dayofvars$X_DestAirportID = as.factor(dayofvars$X_DestAirportID)
dayofvars$X_OriginAirportID = as.factor(dayofvars$X_OriginAirportID)
dayofvars$XX_PeakDate = as.factor(dayofvars$XX_PeakDate)
dayofvars$X_Month = as.factor(dayofvars$X_Month)


dim(dayofvars)

head(dayofvars)

###Evaluation function

evaluateModel <- function(flight_model, dataset, train, test, vocal){
  
  test.Yvals = dataset$Y_ArrDel15[test]
  
  ###Use model to create predicted probabilities of lateness for test set
  yhat = predict(flight_model, newdata=dataset[test,], type="prob")
  mean = mean(yhat[,2])
  
  ###Assign risk levels to flights based on predicted probability. Cut-offs selected through trial & error
  yhat= ifelse(yhat<mean*4, ifelse(yhat<mean*1.5,ifelse(yhat<.7*mean,'A_Low', 'B_Medium'), 'C_High'), 'D_VeryHigh')
  
  ###Create confusion matrix
  confusion = table(test.Yvals, yhat[,2], dnn=c("Delay","Risk Level"))
  
  ###If function was called in vocal mode, print confusion matrix, as integers and as percentages
  if(vocal == TRUE)
      {cat("_________________________________________________\n\nNumber of Flights\n\n")
        print(confusion)
        cat("_________________________________________________")
        cat("\n\nPercent of Total Flights\n\n")
        print(round(confusion/sum(confusion)*100,1))
        cat("_________________________________________________\n")}
  
  ###Calculate prerequisites and success metrics
  
  ##Prerequisites
    
  #Proportion of delays in group A
  A_del = confusion[2]/(confusion[1]+confusion[2]) #delayed flights in A / all flights in A
  A_del
  
  #Proportion of delays in group B
  B_del = confusion[4]/(confusion[3]+confusion[4])  #delayed flights in B / all flights in B
  B_del
  
  #Proportion of delays in group C
  C_del = confusion[6]/(confusion[5]+confusion[6])  #delayed flights in C / all flights in C
  C_del
  
  #Proportion of delays in group D
  D_del = confusion[8]/(confusion[7]+confusion[8])  #delayed flights in D / all flights in D
  D_del
    
  ## Success Metrics
    
  #Percent of on time arrivals flagged in A
  A_sensitivity = confusion[1]/(confusion[1]+confusion[3]+confusion[5]+confusion[7]) #on-time flights in A / all on-time flights
  A_sensitivity
  
  #Percent of delays flagged in C
  C_sensitivity = confusion[6]/(confusion[2]+confusion[4]+confusion[6]+confusion[8]) #delayed flights in C / all delayed flights
  C_sensitivity
  
  #Percent of delays flagged in D
  D_sensitivity = confusion[8]/(confusion[2]+confusion[4]+confusion[6]+confusion[8]) #delayed flights in D / all delayed flights
  D_sensitivity
    
  #Meaningful predictions   
  Meaningful_preds = sum(confusion[c(1,2,5,6,7,8)])/sum(confusion) #flights in levels A,C,D / all flights
  Meaningful_preds
  
  ###Collect and return results
  newstats = data.frame(A_del, B_del, C_del, D_del, A_sensitivity, C_sensitivity, D_sensitivity, Meaningful_preds)
  rownames(newstats) = deparse(substitute(flight_model))
  return(round(newstats,4))
  
}

### Custom K-Fold Cross-Validation Function

suppressWarnings(library(rpart))
suppressWarnings(library(rpart.plot))

crossVal <- function(dataset, k){
  
  #Create empty results dataframe
  stats = data.frame("A_del"=numeric(0),"B_del"=numeric(0), "C_del"=numeric(0),"D_del"=numeric(0), "A_sensitivity"=numeric(0),"C_sensitivity"=numeric(0), "D_sensitivity"=numeric(0),"Meaningful_preds"=numeric(0))
  
  #Shuffle the data
  shuffled<-dataset[sample(nrow(dataset)),]
  
  #Create k equally size folds
  folds <- cut(seq(1,nrow(dataset)),breaks=k,labels=FALSE)
  
  cat("\n")
  
  #Perform k fold cross validation
  for(i in 1:k){
    #Segement data by fold
    test_indices <- which(folds==i,arr.ind=TRUE)
    train_indices = setdiff(1:nrow(shuffled),test_indices)
    
    #Create decision tree model
    usemodel = rpart(Y_ArrDel15~., data=shuffled, subset=train_indices, method = "class", cp = .00005, minsplit = 50) 
    
    #Call evaluateModel function
    newstats = evaluateModel(usemodel, shuffled, train_indices, test_indices, vocal = FALSE)
      
    #Add this model's stats to the raw results dataframe
    stats = rbind(stats,newstats)
    
    #Print progress update
    cat("Fold", i, "meaningful predictions: ",newstats$Meaningful_preds, "\n")
  }

    #Calculate means and standard devations for each metric across the k models
    mean_vals = apply(stats,2,mean)
    sd_vals = apply(stats,2,sd)  

    #Create dataframe of result summary from raw results
    vals = rbind(mean_vals, sd_vals)
    vals = round(vals,4)
    rownames(vals) = c("Average", "St. Dev")
    rownames(stats) = c(1:k)

  #Return results  
  return(vals)
}

###Train/test split 
set.seed(500)
train_indices = sample(1:nrow(dayofvars), nrow(dayofvars)*0.8)
test_indices = setdiff(1:nrow(dayofvars),train_indices)

###Create Basic Model
start = proc.time()
basic = rpart(Y_ArrDel15~., data=basicvars, subset=train_indices, method = "class", cp = .00005, minsplit = 50) 
basic.time = proc.time()-start
cat("Basic Model created in", basic.time[3], "seconds \n")

###Create Day-Of Model
start = proc.time()
dayof = rpart(Y_ArrDel15~., data=dayofvars, subset=train_indices, method = "class", cp = .00005, minsplit = 50) 
dayof.time = proc.time()-start
cat("Day-Of Model created in", dayof.time[3], "seconds \n")


### Plot the structure of the models' decision trees
par(mfrow=c(1,2))
plot(basic, main="Basic Model Decision Tree")
plot(dayof, main="Day-of Model Decision Tree")


###Examine Variable Importance
par(mai=c(.5,2.5,.5,.1), mfrow=c(2,1))
barplot(sort(basic$variable.importance),main="Variable Importance in Basic Model", horiz=TRUE,las=1, cex.names = .8, cex.axis = .8)
barplot(sort(dayof$variable.importance),main="Variable Importance in Day-of Model", horiz=TRUE,las=1, cex.names = .8, cex.axis = .8)


###Evaluate Basic Model
cat("\nBasic Model \n")
basic_results = evaluateModel(basic, basicvars, train_indices, test_indices, vocal = TRUE)
basic_results

###Evaluate Day-Of Model
cat("\nDay-Of Model \n")
dayof_results = evaluateModel(dayof, dayofvars, train_indices, test_indices, vocal = TRUE)
dayof_results

###Check average delay level
cat("\n Average delay level across all flights:", mean(basicvars$Y_ArrDel15))

###Cross Validate
cat("\nBasic Model \n")
basic_crossval = crossVal(basicvars,5)
basic_crossval

cat("\nDay-Of Model \n")
dayof_crossval = crossVal(dayofvars,5)
dayof_crossval


###Compare
comparison = rbind(basic_crossval[1,],dayof_crossval[1,]) 
rownames(comparison) = c("Basic", "Day-Of")
comparison
