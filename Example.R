# # Function: installing and loading of packages
# install_load <- function (packages)  {
# 
#   # Start loop to determine if each package is installed
#   for(package in packages){
# 
#     # If package is installed locally, load
#     if(package %in% rownames(installed.packages()))
#       do.call('library', list(package))
# 
#     # If package is not installed locally, download, then load
#     else {
#       install.packages(package, dependencies = TRUE)
#       do.call("library", list(package))
#     }
#   }
# }

# # Generic libraries loading
# libs <- c("ggplot2", "maps", "plotly", "plyr", "dplyr", "rworldmap","stringr","lubridate", "plotly", "reshape2", "magrittr", "ggthemes", "tidyr", "DT", "lubridate","RColorBrewer")
# install_load(libs)
# 
# # Specific methods libraries loading
# libs.methods <- c("C50", "lattice", "caret", "nnet", "e1071","Matrix", "foreach","glmnet","C50","randomForest","ipred","rpart")
# install_load(libs.methods)
data <- read.csv("survey.csv")
# To delete no important elements
data <- data[ , !(names(data) %in% "state")]
# data <- data[ , !(names(data) %in% "Timestamp")]
data <- data[ , !(names(data) %in% "comments")]
# data <- data[ , !(names(data) %in% "self_employed")]


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
# data$Age<-cut(data$Age, breaks=c(-Inf,21,35,65,Inf),labels = c("21 and under","22-35","36-65","65+"))

# NA values detection and deleting the row.
sapply(data, function(x) sum(is.na(x)))
data <- data[!is.na(data$work_interfere),]
data <- data[!is.na(data$self_employed),]
# Saving the original data with all importants variables
data.origin <- data

# ggplot(data = mpg)
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
    g <- ggplot()+
      geom_bar(data=data.aux,aes(x=nom, y=val,fill=type),stat='identity',position='dodge')+
      coord_flip() +
      labs(
        x = names(data[i]),
        y = "Percentage",
        title = paste(names(data[i])," Frequency", sep="")
        # subtitle = paste("The most different is ", percent, "%", sep=""),
        # caption = "\nDetermined by matrix of covariances"
      # ) %>% suppressWarnings()
      )
    print(g)
  }

}

# Selection of variables with higher variability
data <- data.frame(age= data$Age, gender= data$Gender,
                   family_history= data$family_history,
                   work_interfere= data$work_interfere,
                   benefits= data$benefits,
                   care_options= data$care_options,
                   anonymity= data$anonymity,
                   treatment=data$treatment)

# Preparing regression function for the use in other methods
regresion <- treatment~
  age+
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

mental_all_var <- treatment ~
  age+
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