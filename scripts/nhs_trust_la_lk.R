
dt1<-fread("./raw_data/England Emergency Patients by LA 201819 - Output.csv")

dt2<-fread("./raw_data/Monthly-SITREPSs-CC-and-UOC-Extracts-JANUARY-2020-oa9U1.csv")

dt1[, totad:=sum(CountAdm),by=.(resladst_ons)]

#dt1[, fraction:=CountAdm/sum(CountAdm),by=.(resladst_ons)]



dt1<-dt1[CountAdm>100 & grepl("Child",`PROVIDER NAME`)==F]

dt1[, fraction:=CountAdm/sum(CountAdm),by=.(resladst_ons)]

dt2$procode3<-dt2$`Org Code`
dt2$icubeds<-dt2$`Number of adult critical care beds open`
dt2<-dt2[, .(procode3,icubeds)]
dt1[procode3=="RQ6",procode3:="REM"]
dt1<-merge(dt1,dt2,by="procode3")
dt1$fae_emergency<-NULL

write.csv(dt1, file="./raw_data/la_nhs_lk.csv")

cm<-c("E06000006","E08000011","E08000012","E08000013","E08000014","E08000015")

# "E06000007","E06000008","E06000009"



#cmdt<-dt1[resladst_ons %in% cm]

cmdt<-dt1[resladst_ons %in% cm, .(procode3,icubeds,`PROVIDER NAME`)]
setkey(cmdt, "procode3")
cmdt<-unique(cmdt)

sum(cmdt$icubeds)


icu<-cmdt[, ]
  cmdt[unique(procode3),  ]
  
  
  
  