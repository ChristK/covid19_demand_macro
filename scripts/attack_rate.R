file_name = "summary_ 2020-03-23 .csv"
library(data.table)
dat = fread("./raw_data/summary_ 2020-03-23 .csv")


pop<-fread("./raw_data/LA_ECON_9_04_1yr_pop.csv")

pop[, ageg:=cut(age,c(0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,200),right = F)]

agenames<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49","50-54",
"55-59",
"60-64",
"65-69",
"70-74",
"75-79",
"80+")
levels(pop$ageg) <- agenames
table(pop$ageg)

pop<-pop[year==2017, list(pop=sum(pop)), by=.(lad11cd,ageg)]

dat2<-dat[, list(inf=sum(new.inf.m)), by=.(age,loc)]

dat2<-dat[, list(inf=sum(new.inf.m)), by=.(age,loc)]


dat2<-merge(dat2,pop, by.x=c("loc","age"),  by.y=c("lad11cd","ageg"), all.x = T )

cm<-c("E06000006","E08000011","E08000012","E08000013","E08000014","E08000015")

dat2<-dat2[loc %in% cm]

dat2<-dat2[, list(inf=sum(inf), pop=sum(as.numeric(pop))), by=.(age)]
dat2[, ar:=inf/pop]
dat2<-dat2[order(age)]
