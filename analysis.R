#
# lpMiner.Analysis
#
# Launchpad Data mining in R
#
# 2012(c)
# Written by Chris J Arges <christopherarges@gmail.com>
#
require(caret)
require(klaR)
require(e1071)
require(ggplot2)
require(tm)
require(gridExtra)

fieldsUsed<-c("bugtask_assignee", "bugtask_importance", "bugtask_status", "bugtarget_name",
  "package_type","bug_owner","bug_id","bug_description","bug_title")

# Autotriager
a.X=c("bugtask_importance", "bugtarget_name","package_type", "bug_owner")
a.Y=c("bugtask_assignee")
a.formula=bugtask_assignee ~ bugtask_importance + bugtarget_name + package_type + bug_owner

# Status classification
c.X=c("bugtask_importance", "bugtarget_name", "package_type", "bug_owner")
c.Y=c("bugtask_status")	
c.formula=bugtask_status ~ bugtask_importance + bugtarget_name  + package_type + bug_owner 

#
# get_bag_of_words - returns a sparse dtm to give the relative frequency of
#  words in the document.
# 
get_bag_of_words <- function(input) {
  # http://heuristically.wordpress.com/2011/04/08/text-data-mining-twitter-r/
  corpus <- Corpus(VectorSource(input))
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  dtm <- DocumentTermMatrix(corpus)
  dtm.s <- removeSparseTerms(dtm, sparse=0.99)
  return(dtm.s)
}

get_person <- function(id,field) {
  return(person_stats[which(person_stats$id == id),][[field]])
}
get_person_name <- function(id) {
  return(person_stats[which(person_stats$id == id),]$name)
}

add_clusters <- function (dataAll, clusterSize=200) {
  # Create bag of words.
  title.bag <- get_bag_of_words(dataAll$bug_title)
  #title.fit <- kmeans(title.bag, clusterSize)  
  #dataAll$title_class <- title.fit$cluster
  title.fit <- hclust(dist(title.bag), method="ward")
  dataAll$title_class <- cutree(title.fit, k=clusterSize)
  
  description.bag <- get_bag_of_words(dataAll$bug_description)
  #description.fit <- kmeans(description.bag, clusterSize)
  #dataAll$description_class <- description.fit$cluster
  description.fit <- hclust(dist(description.bag), method="ward")
  dataAll$description_class <- cutree(description.fit, k=clusterSize)

  # Format bag classes
  dataAll$title_class <- as.factor(dataAll$title_class)
  dataAll$description_class <- as.factor(dataAll$description_class)
  
  return(dataAll)
}

#
# parse_data - format dataset for analysis
#
parse_data <- function(input, sampleSize=5000) {
  # cut down to the sample size
  data<- input[sample(1:nrow(input), sampleSize, replace=FALSE),]
  
  # Format data so it's ready.
  dataAll <- data[fieldsUsed] # filter it down to only the used fields
  dataAll$bugtask_importance <- as.factor(dataAll$bugtask_importance)
  dataAll$bug_owner <- as.factor(dataAll$bug_owner)
  dataAll$bugtask_assignee <- as.factor(dataAll$bugtask_assignee)
  dataAll$bugtask_status <- as.factor(dataAll$bugtask_status)
  dataAll$bugtarget_name <- as.factor(dataAll$bugtarget_name)
  dataAll$package_type <- as.factor(as.character(dataAll$package_type))
  
  # Return the parsed data, plus the partitions.
  return (dataAll)
}

#
# analyze.nb - Generalized naive bayes analysis function
#
analyze.nb <- function(train, validation, formula, X, Y, laplace)
{
  # Create NB model
  model <- naiveBayes(formula, train, laplace=laplace)

  # Measure performance of model against validation data.
  results <- predict(model, validation[,-match(Y,names(validation))], class="raw") 

  # Determine results.
  confusion<-confusionMatrix(results, validation[[Y]])
  #print(confusion$overall)

  # return a list with the model and confusion matrix.
  return(list(model=model,results=results,confusion=confusion))
}

#
# analyze.svm - Generalized SVM analysis function
#
analyze.svm <- function(train, validation, formula, X, Y, gamma, cost)
{
  #http://stackoverflow.com/questions/7782501/how-to-interpret-predict-result-of-svm-in-r
  # Create SVM model. gamma,cost determined by train.svm
  model <- svm(formula, train, gamma=gamma, cost=cost)

  # Get probabilities of predictions.
  results <- predict(model, validation[,-match(Y,names(validation))]) 
  
  # Determine results.
  confusion<-confusionMatrix(results, validation[[Y]])
  
  # return a list with the model and confusion matrix.
  return(list(model=model,results=results,confusion=confusion))
}

# autotriage.svm - autotriage using svm
autotriage.svm <- function(train, validation) {
  return (analyze.svm(train, validation, formula=a.formula,X=a.X,Y=a.Y, gamma=0.1, cost=10))
}

# autotriage.nb - autotriage using nb
autotriage.nb <- function(train, validation) {
  return (analyze.nb(train, validation, formula=a.formula, X=a.X, Y=a.Y, laplace=0))
}

# classifystatus.svm - predict ending status using svm
classifystatus.svm <- function(train, validation) {
  return (analyze.svm(train, validation, formula=c.formula, X=c.X, Y=c.Y, gamma=0.01, cost=10))
}

# classifystatus.nb - predict ending status using nb
classifystatus.nb <- function(train, validation) {
  return (analyze.nb(train, validation, formula=c.formula, X=c.X, Y=c.Y, laplace=1))
}

do_tune <- function () {
    s<-1000
    dataAll <- parse_data(bd_closed[!is.na(bd_closed$bugtask_assignee),], sampleSize=s)
    data.parts <- createDataPartition(dataAll[[a.Y]],2, p=0.8)
    dataTrain <- dataAll[data.parts$Resample1,]
    dataValidation <- dataAll[data.parts$Resample2,]

    #dataTrain <- add_clusters(dataTrain)
    #dataValidation <- add_clusters(dataValidation)
    dataTrain <- dataTrain[c(a.Y,a.X)]
    dataValidation <- dataValidation[c(a.Y,a.X)]

    tuned <- tune.svm(a.formula, data=dataTrain, gamma = 10^(-2:0), cost = 10^(-1:1))
    
    model <- svm(a.formula, dataTrain, gamma=0.1, cost=10)

    # Get probabilities of predictions.
    results <- predict(model, dataValidation[,-match(a.Y,names(dataValidation))], probability=TRUE) 
  
    # Determine results.
    confusion<-confusionMatrix(results, dataValidation[[a.Y]])
  
    print(confusion[["overall"]])
}


do_tests <- function(testFunction, function_name, dataTrain, dataValidation, s) {
    print(paste("test", s, function_name, dim(dataTrain)[1], dim(dataValidation)[1] ))
    e_time <- system.time(results<-testFunction(dataTrain,dataValidation)$confusion$overall)
    accuracy_total <- as.numeric(results[["Accuracy"]])
    time_total <- as.numeric(e_time[1])
    ret<-c(dim(dataTrain)[1], as.numeric(accuracy_total),as.numeric(time_total))
    return(ret)
}

do_sample_comparison <- function(sampleSizes=seq(1000,8000,by=1000), clusterSizes=c(100,200)) {

  df.a.nb<-data.frame(row.names=c("sample_size","accuracy"))
  df.a.svm<-data.frame(row.names=c("sample_size","accuracy"))
  df.c.nb<-data.frame(row.names=c("sample_size","accuracy"))
  df.c.svm<-data.frame(row.names=c("sample_size","accuracy"))
  
  for (c in clusterSizes) {
    print(paste("clusterSize",c))
    for (s in sampleSizes) {
      print(paste("sampleSize",s))
      
      # Do autotriager tests.
      df.a.nb <- rbind(df.a.nb, c(do_tests(autotriage.nb,"autotriage.nb", a.X, a.Y, input, s,c)))
      df.a.svm <- rbind(df.a.svm, c(do_tests(autotriage.svm,"autotriage.svm",  a.X, a.Y, input, s,c)))

      # Partition Data
      df.c.nb <- rbind(df.c.nb, c(do_tests(classifystatus.nb,"classifystatus.nb",  c.X, c.Y, input, s,c)))
      df.c.svm <- rbind(df.c.svm, c(do_tests(classifystatus.svm,"classifystatus.svm",  c.X, c.Y, input,s,c)))  
    }
  }
  
  return(list(a.nb=df.a.nb, a.svm=df.a.svm, c.nb=df.c.nb, c.svm=df.c.svm))
}

do_sample_comparison_a <- function(sampleSizes=seq(1000,8000,by=1000)) {

  df.a.nb<-data.frame(row.names=c("sample_size","accuracy"))
  df.a.svm<-data.frame(row.names=c("sample_size","accuracy"))

  for (s in sampleSizes) {
    print(paste("sampleSize",s))
   
    dataAll <- parse_data(bd_closed[!is.na(bd_closed$bugtask_assignee),], sampleSize=s)
    #dataAll <- parse_data(bd_closed, sampleSize=s)

    data.parts <- createDataPartition(dataAll[[a.Y]],2, p=0.8)
    dataTrain <- dataAll[data.parts$Resample2,]
    dataValidation <- dataAll[data.parts$Resample1,]

    #dataTrain <- add_clusters(dataTrain)
    #dataValidation <- add_clusters(dataValidation)
    dataTrain <- dataTrain[c(a.Y,a.X)]
    dataValidation <- dataValidation[c(a.Y,a.X)]

    df.a.svm <- rbind(df.a.svm, c(do_tests(autotriage.svm,"autotriage.svm", dataTrain, dataValidation, s)))
    df.a.nb <- rbind(df.a.nb, c(do_tests(autotriage.nb,"autotriage.nb", dataTrain, dataValidation, s)))

  }
  
  return(list(a.nb=df.a.nb, a.svm=df.a.svm))
}

do_sample_comparison_c <- function(sampleSizes=seq(1000,9000,by=1000)) {

  df.c.nb<-data.frame(row.names=c("sample_size","accuracy"))
  df.c.svm<-data.frame(row.names=c("sample_size","accuracy"))

  for (s in sampleSizes) {
    print(paste("sampleSize",s))
    dataAll <- parse_data(bd_closed, sampleSize=s)

    data.parts <- createDataPartition(dataAll[[c.Y]],2, p=0.8)
    dataTrain <- dataAll[data.parts$Resample2,]
    dataValidation <- dataAll[data.parts$Resample1,]

    #dataTrain <- add_clusters(dataTrain)
    #dataValidation <- add_clusters(dataValidation)
    dataTrain <- dataTrain[c(c.Y,c.X)]
    dataValidation <- dataValidation[c(c.Y,c.X)]
    
    df.c.nb <- rbind(df.c.nb, c(do_tests(classifystatus.nb,"classifystatus.nb", dataTrain, dataValidation,s)))
    df.c.svm <- rbind(df.c.svm, c(do_tests(classifystatus.svm,"classifystatus.svm", dataTrain, dataValidation,s)))  
  }
  
  return(list(c.nb=df.c.nb, c.svm=df.c.svm))
}

do_visualize_a <- function(x) {
  b<- ggplot() +
    geom_line(aes(x=x$a.nb[,1], y=x$a.nb[,2], col="Autotriager NB"),size=1.5) + 
    geom_line(aes(x=x$a.svm[,1], y=x$a.svm[,2], col="Autotriager SVM"),size=1.5) + 
    scale_colour_discrete(name="Experiment") + scale_x_continuous(name="Training Sample Size") + scale_y_continuous(name="Accuracy") +
    opts(title="Results of Autotriager Predictor")
  plot(b)
}

do_visualize_c <- function(x) {
  a<- ggplot() + geom_line(aes(x=x$c.nb[,1], y=x$c.nb[,2], col="Status NB"),size=1.5) + 
    geom_line(aes(x=x$c.svm[,1], y=x$c.svm[,2], col="Status SVM"),size=1.5) + 
    scale_colour_discrete(name="Experiment") + scale_x_continuous(name="Training Sample Size") + scale_y_continuous(name="Accuracy") +
    opts(title="Results of Status Predictor")
  plot(a)
}

do_visualize_gg <- function(results, title="Results") {
  res<-results
  colnames(res) <- c("sample","accuracy","time")
  p<-ggplot(res, aes(x=sample,y=accuracy)) + geom_point() + geom_smooth(method=lm) +
    scale_x_continuous(name="Training Sample Size") + scale_y_continuous(name="Accuracy") +
    opts(title=title)
  return(p)
}

do_visualize_both <- function(input, title="Results") {
  res<-input
  names(res) <- c("NaiveBayes","SVM")
  colnames(res$NaiveBayes) <- c("sample","accuracy","time")
  colnames(res$SVM) <- c("sample","accuracy","time")
  d<-melt(res, id=c("sample","accuracy"))[,c(1,2,5)]
 
  p<-ggplot(d, aes(x=sample,y=accuracy, color=L1)) + geom_point() + geom_smooth() +
    scale_x_continuous(name="Training Sample Size") + scale_y_continuous(name="Accuracy") +
    opts(title=title) + scale_colour_discrete(name="Classifier") 
  return(p)
}

# scatter.smooth(x=xc$c.nb[,1],y=xc$c.nb[,2])
# xc<-do_sample_comparison_c(rep(c(1000,5000,9000),each=4))
#ggplot(xc2$c.svm, aes(x=sample,y=accuracy)) + geom_point() + geom_smooth(method=lm)
#xc<-do_sample_comparison_c(rep(seq(1000,10000,by=500),each=20))

#xa<-do_sample_comparison_a(c(1000,3000,5000,7000))
#xa<-do_sample_comparison_a(rep(seq(1000,5000,by=500),each=20))


merge_data <- function(d1,d2) {
  names(d1) <- c("NaiveBayes","SVM")
  colnames(d1$NaiveBayes) <- c("sample","accuracy","time")
  colnames(d1$SVM) <- c("sample","accuracy","time")

  names(d2) <- c("NaiveBayes","SVM")
  colnames(d2$NaiveBayes) <- c("sample","accuracy","time")
  colnames(d2$SVM) <- c("sample","accuracy","time")

  n1<-rbind(d1$NaiveBayes,d2$NaiveBayes)
  n2<-rbind(d1$SVM,d2$SVM)
  
  return(c(list(n1),list(n2)))
 
}


# get_top_results
#
get_top_results <- function(x, validation, N=5) {
  probs<-attr(x$results,"probabilities")
  valid<-validation

  myret<-data.frame()
  df <- data.frame(probs)
  for (r in row.names(probs)) {
    row <- probs[which(row.names(probs) == r),]
    top<-head(sort(row,decreasing=TRUE),N)
    
    ret<-data.frame(
      id=r,
      original.id=valid[r,]$bugtask_assignee,
      predict.id=names(top),
      predict.probabilty=as.numeric(top),
      predict.names=unlist(lapply(names(top), get_person_name)),
      predict.busy=unlist(lapply(names(top), get_person,field="total_in_progress_cases")))
    myret<-rbind(myret,ret)  
  }
  return(myret)
}

generate_graphs <- function() {
  xa<-do_sample_comparison_a(rep(seq(1000,4000,by=500),each=1))
  xc<-do_sample_comparison_c(rep(seq(1000,4000,by=500),each=1))

  print("xa")
  summary(xa[[1]]$accuracy)
  summary(xa[[2]]$accuracy)

  print("xc")
  summary(xc[[1]]$accuracy)
  summary(xc[[2]]$accuracy)

  pdf(file="results%03d.pdf",onefile=FALSE,paper="special",pointsize=14,width=10,height=16)
  plot(do_visualize_both(xc, "Results of Invalid Predictor"))
  plot(do_visualize_both(xa, "Results of Autotriage Predictor"))

  dev.off()
}
