
dataCK <- read.csv("CK.csv")
dataGK <- read.csv("GK.csv")

TongSinhVienCK <- max(dataCK$No)
TongSinhVienGK <- max(dataGK$No)
TongSinhVien <- TongSinhVienGK + TongSinhVienCK

print("Tong sinh vien trong file GK ")
print(TongSinhVienGK)
print("Tong sinh vien trong file CK ")
print(TongSinhVienCK)

print("Tong sinh vien trong 2 file ")
print(TongSinhVien)