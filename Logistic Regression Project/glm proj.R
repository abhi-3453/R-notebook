adult <- read.csv('adult_sal.csv')
head(adult)
str(adult)
library(dplyr)
adult <- select(adult,-X)

### CLEANING EMPLOYER ###

table(adult$type_employer)
employer <- function(job){
  job <- as.character(job)
  if(job=='Without-pay' || job =='Never-worked')
  {return('Unemployed') }

  else if(job=='Self-emp-inc' || job=="Self-emp-not-inc")
  {return('Self employed') }
  
  else if(job=='Local-gov' || job=="State-gov")
  {return('SLGov') }
  
  else {return(job)}
}

adult$type_employer <- sapply(adult$type_employer, employer)

### CLEANING MARITAL STATUS ###

marital <- function(job){
  job <- as.character(job)
  if(job=='Divorced' || job =='Seperated' || job=="Widowed")
  {return('Not Married') }
  
  else if(job=='Never-married')
  {return(job) }
  
  else {return('Married')}
}

adult$marital <- sapply(adult$marital,marital)
table(adult$marital)

### CLEANING COUNTRY ###

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


country <- function(job){
  job <- as.character(job)
  if(job %in% Asia)
  {return('Asia') }
  else if(job %in% North.America)
  {return('North.America') }
  else if(job %in% Europe)
  {return('Europe') }
  else if(job %in% Latin.and.South.America)
  {return('Latin & South America') }
  
  else {return('South')}
}

adult$country <- sapply(adult$country, country)
table(adult$country)

### CHANGING TO FACTORS ###

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)


str(adult)
table(adult$education)

###MISSING VALUES###

library('Amelia')

adult[adult== '?'] <- NA

str(adult)
adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$occupation <- sapply(adult$occupation,factor)

missmap(adult,y.at = c(1), y.labels = c(''), col= c("yellow","black"))

adult <- na.omit(adult)

### EXPLORATORY ANALYSIS ###

str(adult)
pl <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),color='black',binwidth = 1)
pl1 <-ggplot(adult,aes(hr_per_week)) + geom_histogram(color='black',binwidth=1) 
pl2 <- ggplot(adult,aes(country)) + geom_bar(aes(fill=income),color='black') + theme_bw()  +  theme(axis.text.x = element_text(angle = 90, hjust=1))
ggplotly(pl2)

##SPLITTING DATA### 
library('caTools')
split=sample.split(adult$income,SplitRatio=0.7)
train= subset(adult,split==TRUE)
test= subset(adult,split==FALSE)

###CREATING MODEL###
model = glm(income ~ . , family = binomial(logit), data=train)
smodel <- step(model, direction = "backward", scale = 0)
### PREDICTING DATA ###
pred <- predict(model, newdata = test, type = 'response')
spred <- predict(smodel, newdata = test, type = 'response')
