if (!require(tidyverse))
  install.packages('tidyverse')
library(tidyverse)
# require(tidyverse)
df <- read.csv("cleansurvey.csv")
# df$Age<-cut(df$Age, c(-Inf,20,35,65,Inf))

# gender_table <- table(df$Gender,df$treatment)
# for (i in 1:length(df)) {
#   aux <- prop.table(table(df$treatment, df[,i]), 1) * 100

# df <- lapply(df,table)
# solution <- as.data.frame(table(unlist(df)))
# mytable <- table(solution)
# table(df)
aux <- prop.table(table(df$treatment, df$Gender),1)*100

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
  coord_flip() +
  labs(
    x = "Gender",
    y = "Percentage"
    # title = paste(names(df[i]), "Frequency", sep = " ")
  )
print(g)
# }