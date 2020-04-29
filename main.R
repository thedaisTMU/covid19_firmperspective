library(data.table)
library(stringr)
library(SDMTools)
library(BFTheme)
library(ggplot2)
library(extrafont)

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

desnsity.plot.test <- ggplot(sex.by.firm.size[9:208],aes(pct_change)) +
  brookfield.base.theme() +
  stat_density(colour="#14365D",geom="area",fill="#14365D",alpha=0.9) +
  stat_function(fun=dnorm,n=101,args=list(mean =0.001434484, sd=0.0216845), colour="#DD347A",alpha=0.7,size=1.3) +
  labs(y="Density estimates",x="Percent Change in Employment",title="Figure x",subtitle="Distribution of month-to-month variations in jobs by gender is approximately normal") +
  scale_y_continuous(expand = expansion(mult=c(0,0.05),add=c(0,0))) +
  scale_x_continuous(expand=c(0,0),breaks = c(-0.06,-0.04,-0.02,0,0.02,0.04,0.06),labels = c("-6%","-4%","-2%","0%","2%","4%","6%")) +
  annotate("segment",x=0.04,y=3.8,xend=0.05,yend=10,colour="#DD347A") +
  annotate("text",x=0.05,y=10.5,colour="#DD347A",label="Normal Distribution",family="RooneySans-Regular")


ks.test(sex.by.firm.size[!is.na(pct_change) & SEX==2 & SURVYEAR*SURVMNTH != 6060,pct_change],
        "pnorm",mean=0.001255376,sd=0.01953138)
ks.test(sex.by.firm.size[!is.na(pct_change) & SEX==1 & SURVYEAR*SURVMNTH != 6060,pct_change],
        "pnorm",mean=0.001613593,sd=0.0237412)

sex.by.firm.size.average.sum[,FIRMSIZE:=as.character(FIRMSIZE)]
sex.by.firm.size.average.sum[,SEX:=as.character(SEX)]
sex.by.firm.size.average.sum[SEX==1,SEX:="Male"]
sex.by.firm.size.average.sum[SEX==2,SEX:="Female"]
sex.by.firm.size.average.sum[FIRMSIZE=="1",FIRMSIZE:="0-20 Employees"]
sex.by.firm.size.average.sum[FIRMSIZE=="2",FIRMSIZE:="21-100 Employees"]
sex.by.firm.size.average.sum[FIRMSIZE=="3",FIRMSIZE:="101-500 Employees"]
sex.by.firm.size.average.sum[FIRMSIZE=="4",FIRMSIZE:="501+ Employees"]
sex.by.firm.size.average.sum[FIRMSIZE=="0-20 Employees",FIRMSIZE.ORDER:=1]
sex.by.firm.size.average.sum[FIRMSIZE=="21-100 Employees",FIRMSIZE.ORDER:=2]
sex.by.firm.size.average.sum[FIRMSIZE=="101-500 Employees",FIRMSIZE.ORDER:=3]
sex.by.firm.size.average.sum[FIRMSIZE=="501+ Employees",FIRMSIZE.ORDER:=4]
sex.by.firm.size.average.sum[,FIRMSIZE:=reorder(FIRMSIZE,FIRMSIZE.ORDER)]
sex.by.firm.size.average.sum[,pct_change:=abs(pct_change*100)]
gender.diff.plot.main <- plot.column.bf(sex.by.firm.size.average.sum,
                                        "pct_change",
                                        "FIRMSIZE",
                                        group.by="SEX",
                                        label.unit = "%",
                                        colours=c(set.colours(2,categorical.choice=c("dark.blue","light.blue"))),
                                        plot.title = "Gender Differences in Layoffs Are Concentrated in Smaller Firms",
                                        plot.fig.num = "Figure X",
                                        y.axis = "% Reduction in Workers",
                                        caption = "Source: Labour Force Survey PUMF, Author Calculations") +
  theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),hjust=0.5,angle=0, family = "RooneySans-Light")) +
  scale_y_continuous(breaks = c(0,5,10,15,20),labels = c("0%","-5%","-10%","-15%","-20%"),expand=expansion(mult=c(0,0.2)))



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


age.by.firm.size.average.sum[,FIRMSIZE:=as.character(FIRMSIZE)]
age.by.firm.size.average.sum[AGE_12==1,AGE.ENUMERATED:="15-19 Years Old"]
age.by.firm.size.average.sum[AGE_12==2,AGE.ENUMERATED:="20-24 Years Old"]
age.by.firm.size.average.sum[AGE_12]
age.by.firm.size.average.sum[FIRMSIZE=="1",FIRMSIZE:="0-20 Employees"]
age.by.firm.size.average.sum[FIRMSIZE=="2",FIRMSIZE:="21-100 Employees"]
age.by.firm.size.average.sum[FIRMSIZE=="3",FIRMSIZE:="101-500 Employees"]
age.by.firm.size.average.sum[FIRMSIZE=="4",FIRMSIZE:="501+ Employees"]
age.by.firm.size.average.sum[FIRMSIZE=="0-20 Employees",FIRMSIZE.ORDER:=1]
age.by.firm.size.average.sum[FIRMSIZE=="21-100 Employees",FIRMSIZE.ORDER:=2]
age.by.firm.size.average.sum[FIRMSIZE=="101-500 Employees",FIRMSIZE.ORDER:=3]
age.by.firm.size.average.sum[FIRMSIZE=="501+ Employees",FIRMSIZE.ORDER:=4]
age.by.firm.size.average.sum[,FIRMSIZE:=reorder(FIRMSIZE,FIRMSIZE.ORDER)]
age.by.firm.size.average.sum[,pct_change:=abs(pct_change*100)]
youth.diff.plot.main <- plot.column.bf(age.by.firm.size.average.sum[AGE_12 %in% c(1,2)],
                                        "pct_change",
                                        "FIRMSIZE",
                                        group.by="AGE.ENUMERATED",
                                        label.unit = "%",
                                        colours=c(set.colours(2,categorical.choice=c("dark.blue","light.blue"))),
                                        plot.title = "Youth layoffs were significant at firms of all sizes",
                                        plot.fig.num = "Figure X",
                                        y.axis = "% Reduction in Workers",
                                        caption = "Source: Labour Force Survey PUMF, Author Calculations") +
  theme(axis.text.x = ggplot2::element_text(size=9, margin=ggplot2::margin(t=2),hjust=0.5,angle=0, family = "RooneySans-Light")) +
  scale_y_continuous(breaks = c(0,10,20,30,40,50),labels = c("0%","-10%","-20%","-30%","-40%","-50%"),expand=expansion(mult=c(0,0.1)))


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





##################
immigration.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,IMMIG)]
setkey(immigration.by.firm.size,SURVYEAR,SURVMNTH)
immigration.by.firm.size <- immigration.by.firm.size[!is.na(FIRMSIZE)]
immigration.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,IMMIG)]
immigration.by.firm.size[,pct_change:=(V1-lagged)/V1]
immigration.by.firm.size.average <- immigration.by.firm.size[SURVYEAR*SURVMNTH!=6060,.(mean(pct_change,na.rm=TRUE),
                                                                     median(pct_change,na.rm=TRUE),
                                                                     var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,IMMIG)]

immigration.by.firm.size.average.sum <- immigration.by.firm.size[SURVYEAR*SURVMNTH == 6060]
setkey(immigration.by.firm.size.average.sum,FIRMSIZE,IMMIG)
setkey(immigration.by.firm.size.average,FIRMSIZE,IMMIG)
immigration.by.firm.size.average.sum <- immigration.by.firm.size.average.sum[immigration.by.firm.size.average,nomatch=0]
immigration.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]



#######

naics.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,NAICS_21)]
setkey(naics.by.firm.size,SURVYEAR,SURVMNTH)
naics.by.firm.size <- naics.by.firm.size[!is.na(FIRMSIZE)]
naics.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,NAICS_21)]
naics.by.firm.size[,pct_change:=(V1-lagged)/V1]
naics.by.firm.size.average <- naics.by.firm.size[SURVYEAR*SURVMNTH!=6060,.(mean(pct_change,na.rm=TRUE),
                                                                                       median(pct_change,na.rm=TRUE),
                                                                                       var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,NAICS_21)]

naics.by.firm.size.average.sum <- naics.by.firm.size[SURVYEAR*SURVMNTH == 6060]
setkey(naics.by.firm.size.average.sum,FIRMSIZE,NAICS_21)
setkey(naics.by.firm.size.average,FIRMSIZE,NAICS_21)
naics.by.firm.size.average.sum <- naics.by.firm.size.average.sum[naics.by.firm.size.average,nomatch=0]
names(naics.by.firm.size.average.sum) <- c("SURVYEAR","SURVMNTH","FIRMSIZE","NAICS_21","EMP","lagged","pct_change","MEAN.LAG","MED.LAG","VAR.LAG")
naics.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]

###############################
#Geography
geography.by.firm.size <- main[,sum(FINALWT),by=.(SURVYEAR,SURVMNTH,FIRMSIZE,PROV)]
setkey(geography.by.firm.size,SURVYEAR,SURVMNTH)
geography.by.firm.size <- geography.by.firm.size[!is.na(FIRMSIZE)]
geography.by.firm.size[,lagged:=shift(V1),by=.(FIRMSIZE,PROV)]
geography.by.firm.size[,pct_change:=(V1-lagged)/V1]
geography.by.firm.size.average <- geography.by.firm.size[SURVYEAR*SURVMNTH!=6060,.(mean(pct_change,na.rm=TRUE),
                                                                           median(pct_change,na.rm=TRUE),
                                                                           var(pct_change,na.rm=TRUE)),by=.(FIRMSIZE,PROV)]

geography.by.firm.size.average.sum <- geography.by.firm.size[SURVYEAR*SURVMNTH == 6060]
setkey(geography.by.firm.size.average.sum,FIRMSIZE,PROV)
setkey(geography.by.firm.size.average,FIRMSIZE,PROV)
geography.by.firm.size.average.sum <- geography.by.firm.size.average.sum[geography.by.firm.size.average,nomatch=0]
names(geography.by.firm.size.average.sum) <- c("SURVYEAR","SURVMNTH","FIRMSIZE","PROV","EMP","lagged","pct_change","MEAN.LAG","MED.LAG","VAR.LAG")
geography.by.firm.size.average.sum[,percentile:=pnorm(abs(pct_change),mean=MEAN.LAG,sd=sqrt(VAR.LAG))]








