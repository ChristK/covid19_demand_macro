library(data.table)
library(ggplot2)
icu_age<-fread("./raw_data/icu_icnarc_age_dist.csv")
icu_age<-icu_age[, list(perc=sum(perc)), by=.(age, agel,ageu)]
icu_age[, agesq:=age^2]
icu_age[, agecb:=age^3]
icu_age[, perc:=(perc/100)/(ageu-agel)]

model1<-lm(perc~age+agesq+agecb, data = icu_age)
summary(model1)
icu_age[, pred1:=predict(model1)]
ggplot(icu_age, aes(x=age, y=pred1)) + geom_line() +
  geom_line(aes(y=perc)) 

newdata<-data.table(age=0:85)
newdata[, agesq:=age^2]
newdata[, agecb:=age^3]

newdata<-merge(newdata,icu_age[, .(age,perc)], by="age", all.x = T)
newdata[age>23, pred1:=predict(model1, newdata = newdata[age>23])]
newdata[age<25,pred1:=0.0005 +0.00007758*age ]

newdata[, tot:=sum(pred1)]
newdata[,pred1:=pred1/tot]
ggplot(newdata, aes(x=age, y=pred1)) + geom_line()+
  geom_line(data=newdata[!is.na(newdata$perc)],na.rm=TRUE) 

newdata[, tot2:=sum(pred1)]

newdata[, ageg:=cut(age,c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,200),right = F)]

agenames<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
            "55-59",
            "60-64",
            "65-69",
            "70-74",
            "75-79",
            "80+")
levels(newdata$ageg) <- agenames

dat1<-newdata[, list(perc=sum(pred1)), by=.(ageg)]


+
  geom_line(aes(y=perc)) 




