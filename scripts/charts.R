library(ggplot2)
library(data.table)


#run this script before running indicator_compiler_countrynotes.R if you've made changes to indicators

rm(list = ls())


las<-c("E08000011", "E08000012", "E08000013", "E08000014", "E08000015")
 
p<-0
for (i in las) {
d <- list.files("results/",pattern = i ,recursive = T, full.names=F,ignore.case=T,
                all.files=T)
t1<-fread(file.path("results",d))
t1$lad11cd<-i
p<-p+1
h<-paste0("temp",p)
assign(h,t1)
}

l = list(temp1,temp2,temp3,temp4,temp5)
dt2<-rbindlist(l, use.names=TRUE, fill=TRUE)

names_la<-fread("raw_data/merseyside.csv")
dt2<-merge(dt2,names_la, by.x = "lad11cd", by.y = "V3")

dt2[, date:=as.Date(V1.x)]
ggplot(dt2, aes(x=date, y=bed_icu_0.5, color=V2, linetype=V2)) + geom_line()+theme_bw()+

ggplot(dt2, aes(x=date, y=bed_icu_0.5, color=V2)) + 
  geom_line(size = 1) +
  labs(title = "ICU bed occupancy",
       x = "Date",
       y = "Persons requiring ICU",
       caption = "",
       color = "LA") +
theme_light()

mersey<-fread("results/Merseyside_demand_summary__combined.csv")

mersey[, date:=as.Date(V1)]
mersey$date<-mersey$date+7


ggplot(mersey, aes(x=date)) + 
  geom_line(aes(y=bed_norm_0.5,  color="Normal"), size=1) + geom_line(aes(y=bed_icu_0.5, color="ICU"),size=1)+
  scale_colour_manual("", 
                      breaks = c("Normal", "ICU"),
                      values = c("blue","red"))+
  labs(title = "Bed occupancy",
       x = "Date",
       y = "Persons requiring hospital bed"
  ) +theme_light() 





library(data.table)
#dat = fread("./raw_data/pred_cheshire_merseyside_2020-03-29.csv")
dat = fread("raw_data/data_extract/summary_ 2020-03-23 .csv")
dat<-dat[, .(k,loc,age,date,new.inf.m,new.inf.lo,new.inf.hi)]
write.csv(dat,"./raw_data/infections.csv")
las<-c("E08000011", "E08000012", "E08000013", "E08000014", "E08000015")
dat<-dat[loc %in% las]
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
pop<-pop[year==2017, list(pop=sum(pop)), by=.(lad11cd,la_name,ageg)]


dat2<-dat[, list(inf=sum(new.inf.m)), by=.(date,loc)]
dat2[, dat:=as.Date(date)]
dat2<-merge(dat2,names_la, by.x = "loc", by.y = "V3")
dat2$dat<-dat2$dat+7
ggplot(dat2, aes(x=dat, y=inf, color=V2)) + 
  geom_line(size = 1) +
  labs(title = "New infections",
       x = "Date",
       y = "New infections",
       caption = "",
       color = "LA") +
  theme_light()



dat2<-dat[, list(inf=sum(new.inf.m)), by=.(age,loc)]
dat2<-merge(dat,pop, by.x=c("loc","age"),  by.y=c("lad11cd","ageg"), all.x = T )

sum(dat2$inf)

sum(dt2$bed_norm_0.5)+sum(dt2$bed_icu_0.5)

dt2

dat2<-dat2[, list(inf=sum(inf), pop=sum(as.numeric(pop))), by=.(age)]
dat2[, ar:=inf/pop]
dat2<-dat2[order(age)]



