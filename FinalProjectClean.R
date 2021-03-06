require(tidyverse)
df <- read.csv("survey.csv")
# missing_vals <-sapply(df, function(y) sum(length(which(is.na(y)))))
# missing_vals <- data.frame(missing_vals)
# View(missing_vals)
df <- select(df,-c(comments, state, self_employed, work_interfere))
df$Gender <- as.factor(df$Gender %>% str_to_lower())
male_str <- c("male", "m", "male-ish", "maile", "mal", "male (cis)", "make", "male ", "man","msle", "mail", "malr","cis man", "cis male")
trans_str <- c("trans-female", "something kinda male?", "queer/she/they", "non-binary","nah", "all", "enby", "fluid", "genderqueer", "androgyne", "agender", "male leaning androgynous", "guy (-ish) ^_^", "trans woman", "neuter", "female (trans)", "queer", "ostensibly male, unsure what that really means" )
female_str <- c("cis female", "f", "female", "woman",  "femake", "female ","cis-female/femme", "female (cis)", "femail")
df$Gender <- as.factor(sapply(as.vector(df$Gender), function(x) if(x %in% male_str) "male" else x ))
df$Gender <- as.factor(sapply(as.vector(df$Gender), function(x) if(x %in% female_str) "female" else x ))
df$Gender <- as.factor(sapply(as.vector(df$Gender), function(x) if(x %in% trans_str) "trans" else x ))
df %>% filter(Gender != "a little about you")
df %>% filter(Gender != "p")
summary(df)
dim(df)
# write.csv(df,file = "cleansurvey.csv")
df$Age[which(df$Age<0)]<-20
df$Age[which(df$Age>100)]<-60
summary(df$Age)
ggplot(df,aes(x=Age))+geom_histogram(aes(y=..density..),fill="#62AB61")+geom_density(col="#3438BD",size=1)+labs(x="Age",title="Distribution of Age")