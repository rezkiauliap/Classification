# Title     : Classification (Homework)
# Objective : To classify HR dataset
# Author    : Rezki Aulia Putri
# Date      : September 2019

#membaca data yang akan diklasifikasi
klasifikasi <- read.csv("Dataklasifikasi.csv", sep = ";")

#Berkenalan dulu dengan data (hint: summary(), dim(), plot())
summary(klasifikasi)

x1 <- sapply (klasifikasi[,8], switch, "sales"=1, "technical"=2, "support"=3, "IT"=4, "product_mng"=5, "marketing"=6, "(Other)"=7)
x2 <- sapply (klasifikasi[,9], switch, "hight"=1, "medium"=2, "low"=3)


# install Package yang akan dibutuhkan jika belum
install.packages("ISLR")
install.packages("rpart")
install.packages("randomForest")

# Load library yang sudah diinstall
library(ISLR) # package yang mengeload dataset: Wage
library(rpart) # package yang memuat fungsi rpart (decision tree)
library(randomForest) # package yang memuat fungsi randomForest

## Membagi data menjadi data training dan data testing. data training yang digunakan adalah 60% dan sisanya yaitu 40% untuk data testing
klasifikasi$left <- as.factor(klasifikasi$left)
idxs <- sample(1:nrow(klasifikasi),as.integer(0.6*nrow(klasifikasi)))
trainklasifikasi <- klasifikasi[idxs,]
testklasifikasi <- klasifikasi[-idxs,]

# Metode klasifikasi yang digunakan adalah Decition Tree dan Random Forest
## Decision Tree 
tree <- rpart(left ~ ., 
              data = data.frame(trainklasifikasi), method = "class")
predict(tree, data.frame(klasifikasi),type="class" )

### Membuat confusion matrix untuk Decition Tree
conf <- table(klasifikasi[,'left'],predict(tree, data.frame(klasifikasi),type="class"))
conf

### Menghitung akurasi Decition Tree berdasarkan confusion matrix
AA <- conf[1, 1] 
AB <- conf[1,2]
BA <- conf[2,1]
BB <- conf[2,2]

acc <- (AA+BB)/(AA+AB+BA+BB)
acc
### 0.971264750983399

## Random forest 
randomFor <- randomForest(left ~ ., data = data.frame(klasifikasi), ntree=10000, importance = TRUE)
predict(randomFor, data.frame(klasifikasi),type="class")

### Membuat confusion matrix untuk Random Forest
confusion <- table(klasifikasi[,'left'],predict(tree, data.frame(klasifikasi), type="class"))
confusion

### Menghitung akurasi Random Forest berdasarkan confusion matrix
xx <- confusion[1, 1] 
xy <- confusion[1,2]
yx <- confusion[2,1]
yy <- confusion[2,2]

accu <- (xx+yy)/(xx+xy+yx+yy)
accu
###0.971264750983399


# Dari hasil penghitungan akurasi untuk Decition Tree dan Random Forest berdasarkan confusion matrix, terlihat bahwa kedua metode tersebut memiliki nilai akurasi yang sama untuk data yang digunakan pada percobaan ini.