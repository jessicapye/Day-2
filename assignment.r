library("TraMineR")
#load biofam data
data(biofam)
View(biofam)
#print variable names
names(biofam)
#generate age variable: survey year (2002) - birthyear
biofam$age<- 2002-biofam$birthyr
#min, max, median, mean age
summary(biofam$age)
#min, max, mediam, mean of age for only women
summary(biofam$age[biofam$sex=="woman"])
#create cohort factor
biofam$cohort <-cut(biofam$birthyr,c(1900,1930,1940,1950,1960),labels=c("1900-1920","1930-1939","1940-1949","1950-1959"),right=FALSE)
#histogram of birth year distribution
plot(biofam$cohort,col=c("red","green","blue","yellow"),main="Variable cohort")
#frequency table of cohort factor
table(biofam$cohort)
#cross tabulate cohort with the state at 25 years old
table(biofam$cohort,biofam$a25)
#regression for married with a child and having left home at 25 years old
biofam$mcl<- biofam$a25==6
lg.gr1<-glm(biofam$mcl~biofam$sex + biofam$plingu02,family=binomial)
summary(lg.gr1)
#regression for the youngest cohort only
lg.gr2<-glm(biofam$mcl[biofam$cohort=="1900-1920"]~biofam$sex[biofam$cohort=="1900-1920"] + biofam$plingu02[biofam$cohort=="1900-1920"],family=binomial)
summary(lg.gr2)
