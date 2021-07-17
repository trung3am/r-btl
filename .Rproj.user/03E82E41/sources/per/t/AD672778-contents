install.packages("moments")
install.packages("dplyr")
install.packages("readxl")
library(moments)
library(dplyr)
library(readxl)



dataGK <- read_excel("192_CO1007.xlsx", sheet = "GK", skip = 4)
dataCK <- read_excel("192_CO1007.xlsx", sheet = "CK", skip = 4)

TongSinhVienCK <- max(dataCK$No)
TongSinhVienGK <- max(dataGK$No)
TongSinhVien <- TongSinhVienGK + TongSinhVienCK
TongSinhVien

TongSoCauDungMoiSinhVienCK <- (rowSums(dataCK[,c(4:32)]))
TongSoCauDungMoiSinhVienGK <- (rowSums(dataGK[,c(4:28)]))

TongSoCauDungMoiSinhVienGK
TongSoCauDungMoiSinhVienCK



TongSoCauSaiMoiSinhVienGK <- rowSums(dataGK[,c(4:28)] < 1)
TongSoCauSaiMoiSinhVienCK <- rowSums(dataCK[,c(4:32)] < 1)

TongSoCauSaiMoiSinhVienGK
TongSoCauSaiMoiSinhVienCK



SoCauDungGK <- colSums(dataGK[,c(4:28)])
SoCauDungCK <- colSums(dataCK[,c(4:32)])

CauDungNhieuNhatGK <- subset(SoCauDungGK, SoCauDungGK == max(SoCauDungGK))
CauDungItNhatGK <- subset(SoCauDungGK,SoCauDungGK == min(SoCauDungGK))

CauDungNhieuNhatCK <- subset(SoCauDungCK,SoCauDungCK == max(SoCauDungCK))
CauDungItNhatCK <- subset(SoCauDungCK,SoCauDungCK == min(SoCauDungCK))

CauDungNhieuNhatGK
CauDungItNhatGK

CauDungNhieuNhatCK
CauDungItNhatCK


dataGK1921 <- subset(dataGK, dataGK$MADE == 1921)
SoLanTraLoiDungGK1921 <- colSums(dataGK1921[,c(4:28)])
barplot(SoLanTraLoiDungGK1921,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataGK1922 <- subset(dataGK, dataGK$MADE == 1922)
SoLanTraLoiDungGK1922 <- colSums(dataGK1924[,c(4:28)])
barplot(SoLanTraLoiDungGK1922,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataGK1923 <- subset(dataGK, dataGK$MADE == 1923)
SoLanTraLoiDungGK1923 <- colSums(dataGK1923[,c(4:28)])
barplot(SoLanTraLoiDungGK1923,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataGK1924 <- subset(dataGK, dataGK$MADE == 1924)
SoLanTraLoiDungGK1924 <- colSums(dataGK1924[,c(4:28)])
barplot(SoLanTraLoiDungGK1924,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataCK1921 <- subset(dataCK, dataCK$MADE == 1921)
SoLanTraLoiDungCK1921 <- colSums(dataCK1921[,c(4:32)])
barplot(SoLanTraLoiDungCK1921,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataCK1922 <- subset(dataCK, dataCK$MADE == 1922)
SoLanTraLoiDungCK1922 <- colSums(dataCK1924[,c(4:32)])
barplot(SoLanTraLoiDungCK1922,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataCK1923 <- subset(dataCK, dataCK$MADE == 1923)
SoLanTraLoiDungCK1923 <- colSums(dataCK1923[,c(4:32)])
barplot(SoLanTraLoiDungCK1923,xlab = "Cau hoi", ylab = "So lan tra loi dung")

dataCK1924 <- subset(dataCK, dataCK$MADE == 1924)
SoLanTraLoiDungCK1924 <- colSums(dataCK1924[,c(4:32)])
barplot(SoLanTraLoiDungCK1924,xlab = "Cau hoi", ylab = "So lan tra loi dung")


SoLanTraLoiSaiGK1921 <- colSums(dataGK1921[,c(4:28)] < 1)
barplot(SoLanTraLoiSaiGK1921,xlab = "Cau hoi", ylab = "So lan tra loi Sai")


SoLanTraLoiSaiGK1922 <- colSums(dataGK1924[,c(4:28)] < 1)
barplot(SoLanTraLoiSaiGK1922,xlab = "Cau hoi", ylab = "So lan tra loi Sai")


SoLanTraLoiSaiGK1923 <- colSums(dataGK1923[,c(4:28)] < 1)
barplot(SoLanTraLoiSaiGK1923,xlab = "Cau hoi", ylab = "So lan tra loi Sai")


SoLanTraLoiSaiGK1924 <- colSums(dataGK1924[,c(4:28)] < 1)
barplot(SoLanTraLoiSaiGK1924,xlab = "Cau hoi", ylab = "So lan tra loi Sai")


SoLanTraLoiSaiCK1921 <- colSums(dataCK1921[,c(4:32)] < 1)
barplot(SoLanTraLoiSaiCK1921,xlab = "Cau hoi", ylab = "So lan tra loi Sai")

SoLanTraLoiSaiCK1922 <- colSums(dataCK1924[,c(4:32)] < 1)
barplot(SoLanTraLoiSaiCK1922,xlab = "Cau hoi", ylab = "So lan tra loi Sai")

SoLanTraLoiSaiCK1923 <- colSums(dataCK1923[,c(4:32)] < 1)
barplot(SoLanTraLoiSaiCK1923,xlab = "Cau hoi", ylab = "So lan tra loi Sai")


SoLanTraLoiSaiCK1924 <- colSums(dataCK1924[,c(4:32)] < 1)
barplot(SoLanTraLoiSaiCK1924,xlab = "Cau hoi", ylab = "So lan tra loi Sai")

