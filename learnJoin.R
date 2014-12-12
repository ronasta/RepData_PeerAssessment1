DT <- data.table(
  d = rep(letters[1:3], each=4),
  i = c(1,2,3),
  s = c(10,10,NA, NA,20,20, 30,NA,30, 40,40,NA),
  we = c(rep('e',6),rep('d',6)),
  key = c('d','i')
  )
DT

FILLe <- DT[we=='e' & !is.na(s), mean(s), by=i]
setkey(FILLe,i)
FILLe

FILLd <- DT[we=='d' & !is.na(s), mean(s), by=i]
setkey(FILLd,i)
FILLd
#setkey(DT,i)
tables()

DT[, sOld:=s]
kDT <- key(DT)
setkey(DT,i)
DT<-DT[FILLe][,`:=`(vE=V1,V1=NULL)]
DT<-DT[FILLd][,`:=`(vD=V1,V1=NULL)]

DT[we=='e' & is.na(s), s:=vE]
DT[we=='d' & is.na(s), s:=vD]

setkeyv(DT,kDT)
DT



