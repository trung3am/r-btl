#a
dataGK$TongDiemGK = (round(rowSums(dataGK[,c(4:28)])/25*10,1))
dataCK$TongDiemCK = (round(rowSums(dataCK[,c(4:32)])/29*10,1))

DiemCuaMoiSVGK<- select(dataGK,No,TongDiemGK)
DiemCuaMoiSVCK<- select(dataCK,No,TongDiemCK)
DiemCuaMoiSVGK

#b
DiemTongKet<-inner_join(DiemCuaMoiSVCK,DiemCuaMoiSVGK)
DiemTongKet$TongKet = round(((rowSums(DiemTongKet[,c(2:2)])*0.6 +
                                rowSums(DiemTongKet[,c(3:3)])*0.4)),1)
DiemTongKet

#1
TrungViMauGK<-mean(dataGK$TongDiemGK)
CucDaiMauGK<-max(dataGK$TongDiemGK)
CucTieuMauGK<-min(dataGK$TongDiemGK)

TrungViMauGK
CucTieuMauGK
CucDaiMauGK

TrungViMauCK<-mean(dataCK$TongDiemCK)
CucDaiMauCK<-max(dataCK$TongDiemCK)
CucTieuMauCK<-min(dataCK$TongDiemCK)

TrungViMauCK
CucTieuMauCK
CucDaiMauCK

#2-5 ghep bang? bi lech so'
DiemGK9<-DiemTongKet[DiemTongKet[,3]>=9,]
DiemCK9<-DiemTongKet[DiemTongKet[,2]>=9,]
DiemGK9
DiemCK9

SinhVienCoDiemGKCKLonHonBang9 <- nrow(DiemTongKet[DiemTongKet[,"TongDiemCK"]>=9 & DiemTongKet[,"TongDiemGK"]>=9,])
SinhVienCoDiemGKCKLonHonBang9 

SinhVienCoDiemGKCKLonHonBang7 <- nrow(DiemTongKet[DiemTongKet[,"TongDiemCK"]>=7 & DiemTongKet[,"TongDiemGK"]>=7,])
SinhVienCoDiemGKCKLonHonBang7 

SinhVienCoDiemGKCKLonHonBang5 <- nrow(DiemTongKet[DiemTongKet[,"TongDiemCK"]>=5 & DiemTongKet[,"TongDiemGK"]>=5,])
SinhVienCoDiemGKCKLonHonBang5 

SinhVienCoDiemGKCKNhoHon5 <- nrow(DiemTongKet[DiemTongKet[,"TongDiemCK"]<5 & DiemTongKet[,"TongDiemGK"]<5,])
SinhVienCoDiemGKCKNhoHon5

#2-5 ko ghep bang
GK9 <- nrow(dataGK[dataGK[,"TongDiemGK"]>=9,])
CK9 <- nrow(dataCK[dataCK[,"TongDiemCK"]>=9,])
GK9
CK9

GK7 <- nrow(dataGK[dataGK[,"TongDiemGK"]>=7,])
CK7 <- nrow(dataCK[dataCK[,"TongDiemCK"]>=7,])
GK7
CK7

GK5 <- nrow(dataGK[dataGK[,"TongDiemGK"]>=5,])
CK5 <- nrow(dataCK[dataCK[,"TongDiemCK"]>=5,])
GK5
CK5

GKNhoHon5 <- nrow(dataGK[dataGK[,"TongDiemGK"]<5,])
CKNhoHon5 <- nrow(dataCK[dataCK[,"TongDiemCK"]<5,])
GKNhoHon5
CKNhoHon5


#6
#graph diem cua SV
plot(table(dataCK$TongDiemCK),type = "l",xlab = "Diem", ylab = "So SV")
plot(table(dataGK$TongDiemGK),type = "l",xlab = "Diem", ylab = "So SV")

#graph pho? hay gi ko biet???
plot(select(dataGK,No,TongDiemGK),type = "p")

plot(select(dataCK,No,TongDiemCK),type = "p")


#7
SVDiemCaoNhatGK<- select(dataGK[dataGK[,"TongDiemGK"] == max(dataGK$TongDiemGK),], No, MANH, TO,TongDiemGK)

SVDiemCaoNhatCK<- select(dataCK[dataCK[,"TongDiemCK"] == max(dataCK$TongDiemCK),], No, MANH, TO,TongDiemCK)
SVDiemCaoNhatGK
SVDiemCaoNhatCK

#8
SVDiemNhoNhatGK<- select(dataGK[dataGK[,"TongDiemGK"] == min(dataGK$TongDiemGK),], No, MANH, TO,TongDiemGK)

SVDiemNhoNhatCK<- select(dataCK[dataCK[,"TongDiemCK"] == min(dataCK$TongDiemCK),], No, MANH, TO,TongDiemCK)
SVDiemNhoNhatGK
SVDiemNhoNhatCK

#9
DiemTrungBinhGK <- round(mean(DiemCuaMoiSVGK$TongDiemGK),1)
DiemTrungBinhCK <- round(mean(DiemCuaMoiSVCK$TongDiemCK),1)
DiemTrungBinhGK
DiemTrungBinhCK

#10
SVCoDiemTrungBinhGK <-nrow(select(dataGK[dataGK[,"TongDiemGK"] == DiemTrungBinhGK,], No, MANH, TO,TongDiemGK))
SVCoDiemTrungBinhCK <-nrow(select(dataCK[dataCK[,"TongDiemCK"] == DiemTrungBinhCK,], No, MANH, TO,TongDiemCK))
SVCoDiemTrungBinhGK
SVCoDiemTrungBinhCK


#11
DoPhanTanGK <- sd(DiemCuaMoiSVGK$TongDiemGK)
DoPhanTanCK <- sd(DiemCuaMoiSVCK$TongDiemCK)
DoPhanTanCK
DoPhanTanGK

#12
skewness(DiemCuaMoiSVGK$TongDiemGK)
skewness(DiemCuaMoiSVCK$TongDiemCK)

kurtosis(DiemCuaMoiSVGK$TongDiemGK)
kurtosis(DiemCuaMoiSVCK$TongDiemCK)

#13
Q1GK = quantile(DiemCuaMoiSVGK$TongDiemGK,0.25)
Q3GK = quantile(DiemCuaMoiSVGK$TongDiemGK,0.75)
Q1CK = quantile(DiemCuaMoiSVCK$TongDiemCK,0.25)
Q3CK = quantile(DiemCuaMoiSVCK$TongDiemCK,0.75)

#14 tinh = quantitle theo percentage 75%
DSSVQ3GK <-dataGK[dataGK[,"TongDiemGK"] >= Q3GK,]
DSSVQ3CK <-dataCK[dataCK[,"TongDiemCK"] >= Q3CK,]

SoSVQ3GK = nrow(DSSVQ3GK)
SoSVQ3CK = nrow(DSSVQ3CK)
SoSVQ3GK
SoSVQ3CK

#15 graph theo quantile percentage 75%


plot(table(DSSVQ3GK$TongDiemGK),type="l")
plot(table(DSSVQ3CK$TongDiemCK),type="l")

plot(select(dataGK[dataGK[,"TongDiemGK"] >= Q3GK,],No, TongDiemGK))
plot(select(dataCK[dataCK[,"TongDiemCK"] >= Q3CK,],No, TongDiemCK))


#14 tinh 2 muc diem cao nhat dat dc sort asc 1-2
GKK <- function(k){
  temp<-table(round(TongSoCauDungMoiSinhVienGK/25*10,1))
  options(digits = 2)
  muc <- as.double(names(temp[length(temp)- k + 1]))
  DS <- dataGK[dataGK[,"TongDiemGK"] == muc,]
  return(DS)
}

GKK(1)
GKK(2)
GK2MucCaoNhat = nrow(GKK(1)) + nrow(GKK(2))
GK2MucCaoNhat

CKK <- function(k){
  temp<-table(round(TongSoCauDungMoiSinhVienCK/29*10,1))
  options(digits = 2)
  muc <- as.double(names(temp[length(temp)- k + 1]))
  DS <- dataCK[dataCK[,"TongDiemCK"] == muc,]
  return(DS)
}

CKK(1)
CKK(2)
CK2MucCaoNhat = nrow(CKK(1)) + nrow(CKK(2))
CK2MucCaoNhat


#15 graph theo sort asc 2 muc cao nhat
fetchK <- function(x,k){
  if(x == "CK"){
    temp<-table(round(TongSoCauDungMoiSinhVienCK/29*10,1))
    options(digits = 2)
  } 
  else{
    temp<-table(round(TongSoCauDungMoiSinhVienGK/25*10,1))
  }
  
  options(digits = 2)
  muc <- as.double(names(temp[length(temp)- k + 1]))
  return(muc)
}


plot(select(dataGK[dataGK[,"TongDiemGK"] == fetchK("GK", 1) |
                     dataGK[,"TongDiemGK"] == fetchK("GK", 2) ,]
            ,No,TongDiemGK),)

plot(select(dataCK[dataCK[,"TongDiemCK"] == fetchK("CK", 1) |
                     dataCK[,"TongDiemCK"] == fetchK("CK", 2) ,]
            ,No,TongDiemCK),)

#16- quantile ?


GK <- function(k) {
  Q <- (quantile(DiemCuaMoiSVGK$TongDiemGK,k))
  DS <- dataGK[dataGK[,"TongDiemGK"] >= Q,]
  return(nrow(DS))
}

CK <- function(k) {
  Q <- (quantile(DiemCuaMoiSVCK$TongDiemCK,k))
  DS <- dataCK[dataCK[,"TongDiemCK"] >= Q,]
  return(nrow(DS))
}
CK(0.87)
GK(0.75)

#16 muc asc sort 

SLSVGK <- function(k){
  return (nrow(GKK(k)))
}

SLSVCK <- function(k){
  return (nrow(CKK(k)))
}

SLSVGK(5)
SLSVCK(6)

# 17muc asc sort graph

GraphGK <- function(k){
  plot(select(dataGK[dataGK[,"TongDiemGK"] == fetchK("GK", k),]
              ,No,TongDiemGK),)
}

GraphGK(5)

GraphCK <- function(k){
  plot(select(dataCK[dataCK[,"TongDiemCK"] == fetchK("CK", k),]
              ,No,TongDiemCK),)
}

GraphCK(4)
