DT <- data.table(
  date = rep(c("2014-12-11","2014-12-12","2014-12-13","2014-12-14"), each=3),
  interval = c(100,200,300),
  steps = c(10,10,NA, NA,20,20, 30,NA,30, 40,40,NA),
  key = c('date','interval')
  )
DT

# insert column "typeDay" as "weekend" or "weekday" depending on date
DT <- DT[, typeDay:=ifelse(weekdays(as.Date(date),abbreviate=TRUE) %in% c("Sat","Sun"),
                           "weekend", "weekday")][, ss:=steps]

AUX <- DT[!is.na(steps), mean(steps), keyby=.(typeDay,interval)]
setkeyv(DT,key(AUX))
DT <- DT[AUX][, steps:=ifelse(is.na(steps),V1,steps)][, V1:=NULL]
setkeyv(DT,c('date','interval'))
DT


# check :=ifelse(cond,if,else), CJ cross join, SJ sorted join

# FILLe <- DT[we=='e' & !is.na(s), mean(s), by=i]
# setkey(FILLe,i)
# FILLe
# 
# FILLd <- DT[we=='d' & !is.na(s), mean(s), by=i]
# setkey(FILLd,i)
# FILLd
# #setkey(DT,i)
# tables()
# 
# DT[, sOld:=s]
# kDT <- key(DT)
# setkey(DT,i)
# DT<-DT[FILLe][,`:=`(vE=V1,V1=NULL)]
# DT<-DT[FILLd][,`:=`(vD=V1,V1=NULL)]
# 
# DT[we=='e' & is.na(s), s:=vE]
# DT[we=='d' & is.na(s), s:=vD]
# 
# setkeyv(DT,kDT)
# DT



