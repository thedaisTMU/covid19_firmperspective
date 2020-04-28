library(data.table)
library(stringr)
library(SDMTools)

load("Data/main.RDA")


#Need to predict employment dynamics for people in the February LFS, if they were surveyed again, and if nothing changed, where would they be.

setkey(main,SURVYEAR,SURVMNTH)

sex.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,SEX)]
setkey(sex.by.firm.size,SURVYEAR,SURVMNTH)
sex.by.firm.size <- sex.by.firm.size[!is.na(FIRMSIZE)]
sex.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,SEX)]
sex.by.firm.size[,pct_change:=(V1-lagged)/V1]

sex.by.firm.size.average <- sex.by.firm.size[1:208,.(mean(pct_change,na.rm=TRUE),
                                                     median(pct_change,na.rm=TRUE),
                                                     var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,SEX)]
names(sex.by.firm.size.average) <- c("FIRMSIZE","SEX","MEAN.LAG",
                                     "MED.LAG","VAR.LAG")

sex.by.firm.size.average.sum <- sex.by.firm.size[209:.N]
setkey(sex.by.firm.size.average.sum,FIRMSIZE,SEX)
setkey(sex.by.firm.size.average,FIRMSIZE,SEX)
sex.by.firm.size.average.sum <- sex.by.firm.size.average.sum[sex.by.firm.size.average,nomatch=0]
sex.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]


####################
#Age



age.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,AGE_12)]
setkey(age.by.firm.size,SURVYEAR,SURVMNTH)
age.by.firm.size <- age.by.firm.size[!is.na(FIRMSIZE)]
age.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,AGE_12)]
age.by.firm.size[,pct_change:=(V1-lagged)/V1]

age.by.firm.size.average <- age.by.firm.size[1:1248,.(mean(pct_change,na.rm=TRUE),
                                                     median(pct_change,na.rm=TRUE),
                                                     var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,AGE_12)]
names(age.by.firm.size.average) <- c("FIRMSIZE","AGE_12","MEAN.LAG",
                                     "MED.LAG","VAR.LAG")
age.by.firm.size.average.sum <- age.by.firm.size[1249:.N]
setkey(age.by.firm.size.average.sum,FIRMSIZE,AGE_12)
setkey(age.by.firm.size.average,FIRMSIZE,AGE_12)
age.by.firm.size.average.sum <- age.by.firm.size.average.sum[age.by.firm.size.average,nomatch=0]
age.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]


########################
#Union
########################

union.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,UNION)]
setkey(union.by.firm.size,SURVYEAR,SURVMNTH)
union.by.firm.size <- union.by.firm.size[!is.na(FIRMSIZE)]
union.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,UNION)]
union.by.firm.size[,pct_change:=(V1-lagged)/V1]

union.by.firm.size.average <- union.by.firm.size[1:312,.(mean(pct_change,na.rm=TRUE),
                                                      median(pct_change,na.rm=TRUE),
                                                      var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,UNION)]
names(union.by.firm.size.average) <- c("FIRMSIZE","UNION","MEAN.LAG",
                                     "MED.LAG","VAR.LAG")
union.by.firm.size.average.sum <- union.by.firm.size[313:.N]
setkey(union.by.firm.size.average.sum,FIRMSIZE,UNION)
setkey(union.by.firm.size.average,FIRMSIZE,UNION)
union.by.firm.size.average.sum <- union.by.firm.size.average.sum[union.by.firm.size.average,nomatch=0]
union.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]
############################
#Union slight deep dive - early retirement thing
############################
main[,AGE_2:=1]
main[AGE_12>=11,AGE_2:=2]


age.union.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,UNION,AGE_2)]
setkey(age.union.by.firm.size,SURVYEAR,SURVMNTH)
age.union.by.firm.size <- age.union.by.firm.size[!is.na(FIRMSIZE)]
age.union.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,UNION,AGE_2)]
age.union.by.firm.size[,pct_change:=(V1-lagged)/V1]

age.union.by.firm.size.average <- age.union.by.firm.size[1:622,.(mean(pct_change,na.rm=TRUE),
                                                         median(pct_change,na.rm=TRUE),
                                                         var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,UNION,AGE_2)]
names(age.union.by.firm.size.average) <- c("FIRMSIZE","UNION","AGE_2","MEAN.LAG",
                                       "MED.LAG","VAR.LAG")
age.union.by.firm.size.average.sum <- age.union.by.firm.size[623:.N]
setkey(age.union.by.firm.size.average.sum,FIRMSIZE,UNION,AGE_2)
setkey(age.union.by.firm.size.average,FIRMSIZE,UNION,AGE_2)
age.union.by.firm.size.average.sum <- age.union.by.firm.size.average.sum[age.union.by.firm.size.average,nomatch=0]



#####################################
#Education
#####################################

educ.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,EDUC)]
setkey(educ.by.firm.size,SURVYEAR,SURVMNTH)
educ.by.firm.size <- educ.by.firm.size[!is.na(FIRMSIZE)]
educ.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,EDUC)]
educ.by.firm.size[,pct_change:=(V1-lagged)/V1]

educ.by.firm.size.average <- educ.by.firm.size[1:728,.(mean(pct_change,na.rm=TRUE),
                                                      median(pct_change,na.rm=TRUE),
                                                      var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,EDUC)]
names(educ.by.firm.size.average) <- c("FIRMSIZE","EDUC","MEAN.LAG",
                                     "MED.LAG","VAR.LAG")
educ.by.firm.size.average.sum <- educ.by.firm.size[729:.N]
setkey(educ.by.firm.size.average.sum,FIRMSIZE,EDUC)
setkey(educ.by.firm.size.average,FIRMSIZE,EDUC)
educ.by.firm.size.average.sum <- educ.by.firm.size.average.sum[educ.by.firm.size.average,nomatch=0]
educ.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]



##########################
#Tenure stuff
##########################
tenure.by.firm.size <- main[!is.na(FIRMSIZE),.(wt.mean(TENURE,FINALWT),
                                               sqrt(wt.var(TENURE,FINALWT))),
                            by=.(SURVYEAR,SURVMNTH,FIRMSIZE)]

tenure.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE)]
tenure.by.firm.size[,lagged_std:=shift(V2),by=.(FIRMSIZE)]
tenure.by.firm.size[,pct_change_mean:=(V1-lagged)/V1]
tenure.by.firm.size[,pct_change_std:=(V2-lagged_std)/V2]
















