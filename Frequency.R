if (!require(tidyverse))
  install.packages('tidyverse')
library(tidyverse)
# require(tidyverse)
df <- read.csv("cleansurvey.csv")
df <- select(df,-c(X))
df$Age<-cut(df$Age, breaks=c(-Inf,21,35,65,Inf),labels = c("21 and under","22-45","46-65","65+"))
age_group <- df %>% 
  group_by(Age) %>% 
  summarise(count = n())
# gender_table <- table(df$Gender,df$treatment)
# for (i in 1:length(df)) {
#   aux <- prop.table(table(df$treatment, df[,i]), 1) * 100

# df <- lapply(df,table)
# solution <- as.data.frame(table(unlist(df)))
# mytable <- table(solution)
# table(df)
aux <- prop.table(table(df$treatment, df$Gender),1)*100
# auxdf <- as.data.frame(aux)
# filter(auxdf,auxdf$Freq>1)
nom <- colnames(aux)
type <- c(rep("Yes", ncol(aux)), rep("No", ncol(aux)))
val <- append(aux[1,], aux[2,])
df.aux <- data.frame(nom = nom, type = type , val = val)

g <- ggplot() +
  # geom_bar(mapping = aes(gender_table))
  geom_bar(
    data = df.aux,
    aes(x = nom, y = val, fill = type),
    stat = 'identity',
    position = 'dodge'
  ) +
  # coord_flip() +
  labs(
    x = "Gender",
    y = "Percentage"
    # title = paste(names(df[i]), "Frequency", sep = " ")
  )
print(g)
# }


# age_group <- df %>% 
#   group_by(Age) %>% 
#   summarise(count = n())
# age_group

# ggplot(age_group, aes(x = Age, y = count, fill = Age)) +  
#   geom_bar(stat = "identity") +
#   xlab("Age Group") + 
#   ylab("Number of People") 


# df <- data.frame(age= df$Age, gender= df$Gender,
#                    family_history= df$family_history,
#                    work_interfere= df$work_interfere,
#                    benefits= df$benefits,
#                    care_options= df$care_options,
#                    anonymity= df$anonymity,
#                    treatment=df$treatment)
# 
# 
# regression <- treatment ~
#   gender+
#   family_history+
#   work_interfere+
#   benefits+
#   care_options+
#   anonymity
# 
# set.seed(101)
# n <- nrow(df)
# df.index <- sample(1:n , size=round(n*0.7))
# train <- df[df.index,]
# test <- df[-df.index,]
# 
# mental_all_var <- treatment ~
#   gender+
#   family_history+
#   work_interfere+
#   benefits+
#   care_options+
#   anonymity
# 
# rpart_all_variables <- rpart(mental_all_var,
#                              data=train,
#                              method = "class")
# 
# rpart.plot(rpart_all_variables, type = 4, fallen.leaves = FALSE, extra = 5)
