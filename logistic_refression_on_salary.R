df <- read.csv("adult_sal.csv")
head(df)
colnames(df)
library(dplyr)
df <- select(df,-X)
head(df)
str(df)
summary(df)
table(df$type_employer)
unemp <- function(job){
  job <- as.character(job)
  if (job=="Without-pay" || job == "Never-worked"){
    return ("unemployed")
    }
  else {
    return (job)
    }
}
df$type_employer <- sapply(df$type_employer,unemp)
table(df$type_employer)


combin <- function(job){
  job <- as.character(job)
  if (job=="State-gov" || job == "Local-gov"){
    return ("SL-gov")
  }
  else if (job=="Self-emp-not-inc" || job == "Self-emp-inc") {
    return ("self-employed")
  }
  else {
    return (job)
  }
}

df$type_employer <- sapply(df$type_employer,combin)
table(df$type_employer)
table(df$marital)

group_marital <- function(mar){
  mar <- as.character(mar)
  
  # Not-Married
  if (mar=='Separated' | mar=='Divorced' | mar=='Widowed'){
    return('Not-Married')
    
    # Never-Married   
  }else if(mar=='Never-married'){
    return(mar)
    
    #Married
  }else{
    return('Married')
  }
}

df$marital <- sapply(df$marital,group_marital)
table(df$marital)

table(df$country)
Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')
group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}
Asia
df$country <- sapply(df$country,group_country)
table(df$country)
str(df)

df$type_employer <- sapply(df$type_employer,factor)
df$type_employer
df$country <- sapply(df$country,factor)
df$marital <- sapply(df$marital,factor)
library(Rcpp)
library(Amelia)
head(df)
df[df=="?"] <- NA
table(df$type_employer)
df$type_employer <- sapply(df$type_employer,factor)
df$country <- sapply(df$country,factor)
df$marital <- sapply(df$marital,factor)
df$occupation <- sapply(df$occupation,factor)
missmap(df)
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
df <- na.omit(df)
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))
str(df)
names(df)[names(df)=="country"]<- "region"
colnames(df)
head(df)
library(caTools)
set.seed(101)
smp_size <- floor(0.75 * nrow(df))
set.seed(123)
train_ind <- sample(seq_len(nrow(df)), size = smp_size)

train <- df[train_ind, ]
test <- df[-train_ind, ]
help(glm)
table(train$income)

incom <- function(job){
  job <- as.character(job)
  if (job=="0" ){
    return (0)
  }
  else   {
    return (1)
  }
}
train$income <- sapply(train$income,incom)
model <- glm(income ~.,family=binomial(logit),data=train)
test = sample(df, sample==FALSE)
summary(model)
test$predicted.income = predict(model, newdata=test, type="response")

table(test$income, test$predicted.income > 0.5)
