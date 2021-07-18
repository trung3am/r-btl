library(moments)
library(dplyr)
library(readxl)
library(ggplot2)

CDR <- read_excel("192_CO1007.xlsx", 
                          range = "B2:C12")

LOGK <- read_excel("192_CO1007.xlsx", 
                          sheet = "GK_0", range = "B8:AA11", col_names = FALSE)

colnames(LOGK)[1] <- "MADE" 

LOCK <- read_excel("192_CO1007.xlsx", 
                   sheet = "CK_0", range = "B8:AE11", col_names = FALSE)

colnames(LOCK)[1] <- "MADE" 


#1-2
LietKe <- CDR[is.na(CDR[,"STT"] ),]
SoCDR <- nrow(LietKe)

SoCDR
LietKe

#3-4

TSGK<- as.data.frame(t(LOGK))
TSGK<- TSGK[2:nrow(TSGK),]

colnames(TSGK) <- c(1921:1924)

tansuatGK<- data.frame(
  LearningOutcome = c("LO1", "LO2", "LO3"),
  Freq = c(
    sum(TSGK[1] <= 12),
    sum(TSGK[1] <= 23  & TSGK[1] >= 21),
    sum(TSGK[1] >= 31)
  )
)
#KQGK
tansuatGK
GTSKGK <-ggplot(tansuatGK, aes(x = ""  , 
                               y = "Freq")) + geom_bar(stat = "identity"
                               ) + coord_polar("y", start=0)

GTSKGK<- ggplot(tansuatGK, aes(x="", y=Freq, fill=LearningOutcome)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

GTSKGK


TSCK<- as.data.frame(t(LOCK))
TSCK<- TSCK[2:nrow(TSCK),]

colnames(TSCK) <- c(1921:1924)

tansuatCK<- data.frame(
  LearningOutcome = c("LO1", "LO2", "LO3"),
  Freq = c(
    sum(TSCK[1] <= 12),
    sum(TSCK[1] <= 23  & TSCK[1] >= 21),
    sum(TSCK[1] >= 31)
  )
)
#KQCK
tansuatCK
GTSKCK <-ggplot(tansuatCK, aes(x = ""  , 
                               y = "Freq")) + geom_bar(stat = "identity"
                               ) + coord_polar("y", start=0)

GTSKCK<- ggplot(tansuatCK, aes(x="", y=Freq, fill=LearningOutcome)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

GTSKCK



#5-6

tempGK1<- dataGK[dataGK[,"MADE"] == 1921,]
GK1921 <- data.frame(
  LO = TSGK[,"1921"],
  TLD1 = colSums(tempGK1[,c(4:28)])
)
GK1921
SUMGK1921<-aggregate(GK1921$LO, by = list(LO=GK1921$LO),FUN=sum)

tempGK2<- dataGK[dataGK[,"MADE"] == 1922,]
GK1922 <- data.frame(
  LO = TSGK[,"1922"],
  TLD2 = colSums(tempGK2[,c(4:28)])
)
GK1922
SUMGK1922<-aggregate(GK1922$LO, by = list(LO=GK1922$LO),FUN=sum)

tempGK3<- dataGK[dataGK[,"MADE"] == 1923,]
GK1923 <- data.frame(
  LO = TSGK[,"1923"],
  TLD3 = colSums(tempGK3[,c(4:28)])
)
GK1923
SUMGK1923<-aggregate(GK1923$LO, by = list(LO=GK1923$LO),FUN=sum)

tempGK4<- dataGK[dataGK[,"MADE"] == 1924,]
GK1924 <- data.frame(
  LO = TSGK[,"1924"],
  TLD4 = colSums(tempGK4[,c(4:28)])
)
GK1924
SUMGK1924<-aggregate(GK1924$LO, by = list(LO=GK1924$LO),FUN=sum)

SUMGK1924
SUMGK1923

colnames(SUMGK1921)[2] <- "TLD1"
colnames(SUMGK1922)[2] <- "TLD2"
colnames(SUMGK1923)[2] <- "TLD3"
colnames(SUMGK1924)[2] <- "TLD4"

SUMGK <-merge(x = SUMGK1921,y = SUMGK1922, by = "LO" )
SUMGK <-merge(x = SUMGK,y = SUMGK1923, by = "LO" )
SUMGK <-merge(x = SUMGK,y = SUMGK1924, by = "LO" )

DungLOGK <- data.frame( LO = SUMGK$LO,SUMTLD = SUMGK$TLD1 + SUMGK$TLD2 
                    + SUMGK$TLD3 + SUMGK$TLD4)
FGK <- transform(DungLOGK,FRQ = SUMTLD/sum(DungLOGK$SUMTLD))
#Tan suat & graph giua ky
FGK
plot(DungLOGK)
ggplot(DungLOGK) + geom_point(mapping = aes(x = LO, y = SUMTLD))



tempCK1<- dataCK[dataCK[,"MADE"] == 1921,]
CK1921 <- data.frame(
  LO = TSCK[,"1921"],
  TLD1 = colSums(tempCK1[,c(4:32)])
)
CK1921
SUMCK1921<-aggregate(CK1921$LO, by = list(LO=CK1921$LO),FUN=sum)

tempCK2<- dataCK[dataCK[,"MADE"] == 1922,]
CK1922 <- data.frame(
  LO = TSCK[,"1922"],
  TLD2 = colSums(tempCK2[,c(4:32)])
)
CK1922
SUMCK1922<-aggregate(CK1922$LO, by = list(LO=CK1922$LO),FUN=sum)

tempCK3<- dataCK[dataCK[,"MADE"] == 1923,]
CK1923 <- data.frame(
  LO = TSCK[,"1923"],
  TLD3 = colSums(tempCK3[,c(4:32)])
)
CK1923
SUMCK1923<-aggregate(CK1923$LO, by = list(LO=CK1923$LO),FUN=sum)

tempCK4<- dataCK[dataCK[,"MADE"] == 1924,]
CK1924 <- data.frame(
  LO = TSCK[,"1924"],
  TLD4 = colSums(tempCK4[,c(4:32)])
)
CK1924
SUMCK1924<-aggregate(CK1924$LO, by = list(LO=CK1924$LO),FUN=sum)

SUMCK1924
SUMCK1923

colnames(SUMCK1921)[2] <- "TLD1"
colnames(SUMCK1922)[2] <- "TLD2"
colnames(SUMCK1923)[2] <- "TLD3"
colnames(SUMCK1924)[2] <- "TLD4"

SUMCK <-merge(x = SUMCK1921,y = SUMCK1922, by = "LO" )
SUMCK <-merge(x = SUMCK,y = SUMCK1923, by = "LO" )
SUMCK <-merge(x = SUMCK,y = SUMCK1924, by = "LO" )

DungLOCK <- data.frame( LO = SUMCK$LO,SUMTLD = SUMCK$TLD1 + SUMCK$TLD2 
                        + SUMCK$TLD3 + SUMCK$TLD4)
FCK <- transform(DungLOCK,FRQ = SUMTLD/sum(DungLOCK$SUMTLD))
#Tan suat & graph cuoi ky
FCK
plot(DungLOCK)
ggplot(DungLOCK) + geom_point(mapping = aes(x = LO, y = SUMTLD))


# 

#7 



#TINH CHUNG 4 MADE GOP LAI  
FQ11G <- sum(rowSums(LOGK[,c(2:26)] == 11),na.rm = T)
FQ12G <- sum(rowSums(LOGK[,c(2:26)] == 12),na.rm = T)
FQ21G <- sum(rowSums(LOGK[,c(2:26)] == 21),na.rm = T)
FQ22G <- sum(rowSums(LOGK[,c(2:26)] == 22),na.rm = T)
FQ23G <- sum(rowSums(LOGK[,c(2:26)] == 23),na.rm = T)
FQ31G <- sum(rowSums(LOGK[,c(2:26)] == 31),na.rm = T)
FQ32G <- sum(rowSums(LOGK[,c(2:26)] == 32),na.rm = T)

TanSuatGK<- data.frame(
  Lo = c(11,12,21,22,23,31,32),
  F= c(FQ11G,FQ12G,FQ21G,FQ22G,FQ23G,FQ31G,FQ32G)
)
gtansuatgk <- ggplot(TanSuatGK) + geom_point(mapping = aes(x = Lo, y = F))
gtansuatgk

FQ11 <- sum(rowSums(LOCK[,c(2:26)] == 11),na.rm = T)
FQ12 <- sum(rowSums(LOCK[,c(2:26)] == 12),na.rm = T)
FQ21 <- sum(rowSums(LOCK[,c(2:26)] == 21),na.rm = T)
FQ22 <- sum(rowSums(LOCK[,c(2:26)] == 22),na.rm = T)
FQ23 <- sum(rowSums(LOCK[,c(2:26)] == 23),na.rm = T)
FQ31 <- sum(rowSums(LOCK[,c(2:26)] == 31),na.rm = T)
FQ32 <- sum(rowSums(LOCK[,c(2:26)] == 32),na.rm = T)

TanSuatCK<- data.frame(
  Lo = c(11,12,21,22,23,31,32),
  F= c(FQ11,FQ12,FQ21,FQ22,FQ23,FQ31,FQ32)
)
gtansuatck <- ggplot(TanSuatCK) + geom_point(mapping = aes(x = Lo, y = F))
gtansuatck

TanSuatChung <- data.frame(
  Lo = c(11,12,21,22,23,31,32),
  F = TanSuatGK$F + TanSuatCK$F
)
TanSuatChung
gtansuatchung <- ggplot(TanSuatChung) + 
  geom_point(mapping = aes(x = Lo, y = F))

gtansuatchung



