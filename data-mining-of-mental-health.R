# ## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE,results='hide'-----
# 
# # Function: installing and loading of packages
# install_load <- function (packages)  {   
#    
#   # Start loop to determine if each package is installed
#    for(package in packages){
# 
#        # If package is installed locally, load
#        if(package %in% rownames(installed.packages()))
#           do.call('library', list(package))
# 
#        # If package is not installed locally, download, then load
#        else {
#           install.packages(package, dependencies = TRUE)
#           do.call("library", list(package))
#        }
#    } 
# }
# 
# # Generic libraries loading
# libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
# install_load(libs)
# 
# # Specific methods libraries loading
# libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
# install_load(libs.methods)

# Data loading
data <- read.csv("survey.csv")


# ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE,results='hide'-----

# To delete no important elements
data <- data[ , !(names(data) %in% "state")]
data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
data <- data[ , !(names(data) %in% "self_employed")]


# Gender unification.
data$Gender %<>% str_to_lower()

male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")

data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% male_str) "male" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% female_str) "female" else x )
data$Gender <- sapply(as.vector(data$Gender), function(x) if(x %in% trans_str) "trans" else x )
data %<>% filter(Gender != "a little about you")
data %<>% filter(Gender != "guy (-ish) ^_^")
data %<>% filter(Gender != "p")


# Age categorization
data$Age<-cut(data$Age, c(-Inf,20,35,65,Inf))

# NA values detection and deleting the row.
sapply(data, function(x) sum(is.na(x)))
data <- data[!is.na(data$work_interfere),]

# Saving the original data with all importants variables 
data.origin <- data


## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------

for(i in 1:length(data)){
  aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
  percent <- round(max(abs(aux[1,]-aux[2,])), digits = 2)

  if(percent > 10 & percent < 99){
    
    # Data preparing to visualization
    aux <- prop.table(table(data$treatment, data[,i]), 1)*100 
    nom <- colnames(aux)
    type <- c(rep("Yes",ncol(aux)),rep("No",ncol(aux)))
    val <- append(aux[1,], aux[2,])
    data.aux<-data.frame(nom=nom,type=type ,val=val)
    
    # Use of the library ggplot2 to data visualization 
    g <- ggplot() + geom_bar(data=data.aux,aes(x=nom, y=val,fill=type),stat='identity',position='dodge')+
      coord_flip() +
      labs(
        x = "Importance",
        y = "",
        title = paste("Mental Health comparation about ", names(data[i]), sep=""),
        subtitle = paste("The most different is ", percent, "%", sep=""),
        caption = "\nDetermined by matrix of covariances"
        ) %>% suppressWarnings()
    print(g)
  }

}

## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE,results='hide'-----
# Selection of variables with higher variability
data <- data.frame(gender= data$Gender,
                   family_history= data$family_history,
                   work_interfere= data$work_interfere,
                   benefits= data$benefits, 
                   care_options= data$care_options,
                   anonymity= data$anonymity,
                   treatment=data$treatment)

# Preparing regression function for the use in other methods
regresion <- treatment~
                gender+
                family_history+
                work_interfere+
                benefits+
                care_options+
                anonymity
              

# Saving prediction percentage of each method
percent <- data.frame(methods=c("Trees Classifier", "Neuronal Network","Randon Forest","Bagging"), value=c(0,0,0,0))

# Data training and testing
set.seed(101)
n <- nrow(data)
data.index <- sample(1:n , size=round(n*0.7))
train <- data[data.index,]
test <- data[-data.index,]

## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE,results='hide'-----

# Preparation of training scheme
control <- trainControl(method = "repeatedcv", number = 3, repeats = 3)

set.seed(27)
regresion.origin <-  treatment~.
caret_gbm <- caret::train(regresion.origin,
                         data = data.origin,
                         method = "gbm",
                         preProcess = NULL,
                         trControl = control)

# Importance estimation of variable category
importance <- varImp(caret_gbm, scale=TRUE)

# Data preparing to visualization
data.imp <- data.frame(Overall=importance$importance$Overall, group= rownames(importance$importance))
data.imp <- data.imp[with(data.imp, order(-data.imp$Overall)), ] # Orden inverso

aux<- data.imp[1:10,]
data.imp<- data.frame(Overall=aux$Overall, group= aux$group)
data.imp1 <- data.imp
data.imp0 <- data.frame(Overall=0, group= data.imp$group)
data.imp <- rbind(data.imp,data.imp0)

## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
# Use of the library ggplot2 to data visualization 
ggplot() +
  geom_point(data = data.imp1, aes(x = Overall, y = group, color = group), size = 4) +
  geom_path(data = data.imp, aes(x = Overall, y = group, color = group, group = group), size = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Importance",
    y = "",
    title = "Mental Health Aspects in The Tech",
    subtitle = "Scaled feature importance",
    caption = "\nDetermined with Stochastic Gradient Boosting and
    repeated cross validation (3 repeats, 3 times)"
  ) %>% suppressWarnings()


## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
# Executing model C5.0
model <- C5.0( treatment ~ . , data = train)

# Prediction
prediction <- predict(model,newdata=test)

# Confussion matrix
( mc <- table(prediction, test$treatment) )

# Succesful percentage of clasification
( percent$value[1] <- sum(diag(mc)) / sum(mc) * 100 )

## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
# Calculation of size and decay parameters
# size: number of intermediate hidden unit
# decay: avoiding overfitting
parameter <- train( treatment ~ . , data=train, method="nnet", trace=F)
size <- parameter$bestTune$size
decay <- parameter$bestTune$decay
#parameter$bestTune

# Neuronal Network model
model <- nnet(treatment ~ ., size=size, decay=decay, trace=F, data=train)

# Prediction. Creating a dataframe with the probabilities
predict <- data.frame(predict(model, test), treatment=predict(model,test, type="class"))

# Confussion matrix
( mc <- table(predict$treatment,test$treatment, dnn = c("Asignado","Real")) )

# Succesful percentage of clasification
( percent$value[2] <- sum(diag(mc)) / nrow(test) * 100 )


## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
# Random Forest model
model <- randomForest(treatment ~ .,  data=train)

# Prediction. Creating a dataframe with the probabilities
predict <- predict(model, test)

# Confussion matrix
( mc <- with(test, table(predict, treatment)) )

# Succesful percentage of clasification
( percent$value[3] <- sum(diag(mc)) / sum(mc) * 100 )



## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
# Bagging model
model <- bagging(treatment ~ .,  data=train)

# Prediction. Creating a dataframe with the probabilities
predict <- predict(model, test)

# Confussion matrix
( mc <- with(test, table(predict, treatment)) )

# Succesful percentage of clasification
( percent$value[4] <- sum(diag(mc)) / sum(mc) * 100 )



## ------------------------------------------------------------------------
percent$methods <- paste(percent$methods, " - " , round(percent$value,digits = 2) , "%" , sep = "")
visualize <- data.frame(valor=percent$value, group= percent$methods)
visualize2 <- rbind(visualize,data.frame(valor=50, group= visualize$group))

ggplot() +
  geom_point(data = visualize, aes(x = valor, y = group, color = group), size = 4) +
  geom_path(data = visualize2, aes(x = valor, y = group, color = group, group = group), size = 2) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5)) +
  labs(
    x = "Percentage of success",
    y = "Methods",
    title = "Percentage of success of the methods"
  ) 

## ------------------------------------------------------------------------
# Executing model C5.0
model <- C5.0( treatment ~ . , data = train)
plot(model)


## ------------------------------------------------------------------------
mental_all_var <- treatment ~
                    gender+
                    family_history+
                    work_interfere+
                    benefits+
                    care_options+
                    anonymity

rpart_all_variables <- rpart(mental_all_var,
                            data=train,
                            method = "class")

rpart.plot::rpart.plot(rpart_all_variables, type = 4, fallen.leaves = FALSE, extra = 5)

## ----eval=TRUE,echo=TRUE,warning=FALSE, message=FALSE--------------------
data <- data.origin
# Data preparing
country <- data$Country
ill <- data$treatment
data.aux <- data.frame(country,ill)

# Data frame definition
data.result <- ddply(data.aux,.(country,ill), nrow)

# Frecuency accumulation of treated people
frec <- data.frame(id = data.result$country
                   , value = data.result$V1)

frec <- mutate(group_by(frec,id), cumsum=cumsum(value))

data.all <- data.frame(id=frec$id,num=frec$cumsum)
data.all <- data.all[with(data.all, order(-data.all$num)), ]
data.all <- data.all[!duplicated(data.all$id),]
data.all <- data.all[with(data.all, order(data.all$id)), ]

# Putting treated people in negative way
for (i in 1:length(data.result$country)){
    if(data.result$ill[i] =="Yes"){
      data.result$V1[i] <- data.result$V1[i]*-1
    }
}

# Frecuency accumulation of treated people
frec <- data.frame(id = data.result$country
                          , value = data.result$V1)

frec <- mutate(group_by(frec,id), cumsum=cumsum(value))

data.aux <- data.frame(id=frec$id,num=frec$cumsum)
data.aux <- data.aux[with(data.aux, order(-data.aux$num)), ]
data.aux <- data.aux[!duplicated(data.aux$id),]
data.aux <- data.aux[with(data.aux, order(data.aux$id)), ]

# Making relative treated people
data <- data.frame(id=data.all$id)
data$id <- data.aux$id
data$num <- (data.aux$num / data.all$num )
data$category <- data$num

# Categorization of treated people by geographic location
data$category  <- cut(data$num, breaks=c(-Inf, -0.75, -0.5, -0.35, 0, 0.35, 0.5, 0.75, Inf))
levels(data$category) <- c("-70%","-50%","-30%","-15%","+15%","+30%","+50%","+70%")

d <- data.frame( country=data$id, value=data$cate)
n <- joinCountryData2Map(d, joinCode="NAME", nameJoinColumn="country")

# Visualization of treated people by geographic location
mapCountryData(n, nameColumnToPlot="value", mapTitle="World" ,catMethod="categorical",
               colourPalette=c('red','blue'),missingCountryCol="grey", aspect =0)


