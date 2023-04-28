# The hyperparameter for the Imprecise Dirichlet Model s=2 #

# Find the lower bounds of the class probabilties. #
class.lower<-function(class){
  counts <- table(class)
  lower <- counts/(sum(counts)+2)
  return(lower)
}

# Find the upper bounds of the class probabilties. #
class.higher<-function(class){
  counts <- table(class)
  higher <- (counts+2)/(sum(counts)+2)
  return(higher)
}

# Find the lower bounds of the probabilities of the attribute values conditional on the class values. #
attribute.lower <- function(class, attribute){
  counts <- table(class, attribute)
  lower.ai <- counts/(apply(counts, 1, sum)+2)
  return(lower.ai)
}

# Find the upper bounds of the probabilities of the attribute values conditional on the class values. #
attribute.higher <-function(class, attribute){
  counts <- table(class, attribute)
  higher.ai <- (counts+2)/(apply(counts, 1, sum)+2)
  return(higher.ai)
}

# Find the lower bounds of the joint probability of observing class value c and vector of attribute values a. #
probca.lower <- function(dataframe, attrcols, classcol, testrow, classvalue){
  classes.lower <- class.lower(dataframe[classcol])
  attrs.lower <- lapply(attrcols, 
                        function(colid) attribute.lower(dataframe[,classcol], dataframe[,colid]))
  condprobsac.lower <- sapply(
    1:length(attrcols),
    function(i){
      colid <- attrcols[i]
      a.level <- testrow[1, colid]
      prob.table.a.given.c.lower <- attrs.lower[[i]]
      prob.table.a.given.c.lower[classvalue, a.level]
    }
  )
  prob.c.lower <- classes.lower[classvalue]
  prob.c.lower*prod(condprobsac.lower)
}

# Find the lower bounds of the joint probability of observing class value c and vector of attribute values a. #
probca.higher <- function(dataframe, attrcols, classcol, testrow, classvalue){
  classes.higher <- class.higher(dataframe[classcol])
  attrs.higher <- lapply(attrcols, 
                         function(colid) attribute.higher(dataframe[,classcol], dataframe[,colid]))
  condprobsac.higher <- sapply(
    1:length(attrcols),
    function(i){
      colid <- attrcols[i]
      a.level <- testrow[1, colid]
      prob.table.a.given.c.higher <- attrs.higher[[i]]
      prob.table.a.given.c.higher[classvalue, a.level]
    }
  )
  prob.c.higher <- classes.higher[classvalue]
  prob.c.higher*prod(condprobsac.higher)
}

# Find the lower bound of the joint probability for class value benign. #
b.low_i<-function(i){probca.lower(train, 2:10, 11, test[i,], 'benign')}

# Find the upper bound of the joint probability for class value benign. #
b.high_i<-function(i){probca.higher(train, 2:10, 11, test[i,], 'benign')}

# Find the lower bound of the joint probability for class value malignant. #
m.low_i<-function(i){probca.lower(train, 2:10, 11, test[i,], 'malignant')}

# Find the upper bound of the joint probability for class value malignant. #
m.high_i<-function(i){probca.higher(train, 2:10, 11, test[i,], 'malignant')}

# Build the Naive Credal Classifier using interval dominance. #
ncclassifier <- function(i){
  if(b.low_i(i)>m.high_i(i)){
    NonDominatedClasses<-'benign'
  }else{
    if(m.low_i(i)>b.high_i(i)){
      NonDominatedClasses<-'malignant'
    }
    else{
      NonDominatedClasses<-'no non-dominated classes'
    }
  }
  print(NonDominatedClasses)
}

# For the confusion matrix: #

# Find the actual class values. #
actualclass_i <- function(i){
  as.character(test[i,11])
}

# Find the classified values from the NCC. #
ncclassified_i <- function(i){
  ncclassifier(i)
}

# Find matrices correpsonding to the actual class value. #
ncmatrix_i <- function(i){
  if(actualclass_i(i)=='malignant'){
    print(matrix(c(1,0,0,0), nrow=2, ncol=2))
  }else {
    print(matrix(c(0,0,0,1), nrow=2, ncol=2))
  }
}

# Set t to be an empty matrix. #
t <- matrix(c(0,0,0,0), nrow=2, ncol=2)

# Find matrices corresponding to whether the classified class value is the same as the actual class value or not,
# and add these all to t. 
# If the NCC gave no non-dominated classes then add on an empty matrix to t. #
for(i in 1:dim(test)[1]){
  if(ncclassified_i(i)==actualclass_i(i)){
    toprint<- ncmatrix_i(i)
  }else{ if(ncclassified_i(i)=='malignant'){
    toprint <- matrix(c(0,1,0,0), nrow=2, ncol=2)
  }else{if(ncclassified_i(i)=='benign'){
    toprint<- matrix(c(0,0,1,0), nrow=2, ncol=2)
  }else{
    toprint<-matrix(c(0,0,0,0), nrow=2, ncol=2)
  }
  }
  }
  t <- t + toprint
  print(t)
}
