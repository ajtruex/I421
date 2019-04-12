require(dplyr)
require(ggplot2)
require(tidyr)
dat = read.csv("mental-heath-in-tech-2016_20161114.csv")
dim(dat)
head(names(dat))
names(dat) = tolower(names(dat))
head(names(dat))
# Explore a few variables
#summary(dat$do.you.currently.have.a.mental.health.disorder.)
#summary(dat$have.you.had.a.mental.health.disorder.in.the.past.)
#summary(dat$have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.)

# Shorten some var names
# -----------------------------------------------------------------------------------
dat2 = rename(dat, 
              mdis_now = do.you.currently.have.a.mental.health.disorder.,
              mdis_past = have.you.had.a.mental.health.disorder.in.the.past.,
              mdis_diagnosed = have.you.been.diagnosed.with.a.mental.health.condition.by.a.medical.professional.,
              age = what.is.your.age.,
              sex = what.is.your.gender.,
              country = what.country.do.you.live.in.,
              mdis_cat = if.yes..what.condition.s..have.you.been.diagnosed.with.)


# Save all mdis variables in a new df
dat2 %>% select(contains('mdis'), age, sex, country) %>% str()
disorder_vars = dat2 %>% select(contains('mdis'))

names(disorder_vars)
female_levels = levels(dat2$sex)[grep('(fe).*|^f$|fm|woman|female', levels(dat2$sex), ignore.case = T, perl = T)]
male_levels   = levels(dat2$sex)[grep('^m$|\bmale| male|mail|male |^male| man|\bman|^man$|masculine|dude|^male$', levels(dat2$sex), ignore.case = T, perl = T)]
dat2$sex_new = dat2$sex
dat2$sex_new[dat2$sex_new %in% female_levels] = 'female'
dat2$sex_new[dat2$sex_new %in% male_levels] = 'male'
dat2$sex_new[!(dat2$sex_new %in% c('male', 'female'))]
summary(dat2$sex_new %in% female_levels)
summary(dat2$sex_new %in% male_levels)
summary(!(dat2$sex_new %in% c(male_levels, female_levels)))

dat2$sex_new = droplevels(dat2$sex_new)
table(dat2$sex_new)