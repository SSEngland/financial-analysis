#data cleaning
sp500[is.na(sp500)] <- 0
#convert it to data matrix
u<-data.matrix(sp500)

#univariate non-linear time series correlation
library(dCovTS)
#max 15 days lag
ADCF(sp500$SHW, MaxLag = 15, unbiased = FALSE)

#SHW&CSCO very strong correlation 0.8956487 with no time lag
#IT&GE very strong correlation 0.9369493 with no time lag

#get the last 100 records of the second share in sp500
data<-tail(ibmSp500[,2],100)
#cherry pick two stocks as a test
data<-data.matrix(tail(sp500$IT,2000))
data2<-data.matrix(tail(sp500$GE,2000))
two_stocks <- merge(data,data2,by="row.names")
#simple plot to see correlation
library(ggplot2)
ggplot(data=two_stocks,aes(IT,GE))+geom_jitter()
# getting non-linear correlation of two time series
u<-data.matrix(two_stocks)
u<-u[,2:3]
c<-mADCF(u, lags=0, unbiased = FALSE, output = TRUE)
#finding the suitable time lag
mADCFplot(u, MaxLag = 15, ylim = NULL, b = 499, bootMethod = c("Wild Bootstrap",
                                                               "Independent Bootstrap"))
#the script above will change the plot settings to 4 charts per page, this will change it back to one chart per page
par(mfrow=c(1,1))
#this will remove all charts
dev.off()
#try with 1st 5 stocks now
allstockdata<-data.matrix(tail(sp500[,(1:5)],2000))
row.names(allstockdata)<-NULL
cor_matrix<-mADCF(allstockdata, lags=0, unbiased = FALSE, output = TRUE)
#Distance Correlation Matrix at lag:  0 
#         [MMM]     [ACN]     [ATVI]    [AYI]    [ADBE]
#[MMM] 1.0000000 0.9441019 0.9179730 0.9476662 0.9730916
#[ACN] 0.9441019 1.0000000 0.9402125 0.9506718 0.9543290
#[ATVI] 0.9179730 0.9402125 1.0000000 0.9391710 0.9735264
#[AYI] 0.9476662 0.9506718 0.9391710 1.0000000 0.9626206
#[ADBE] 0.9730916 0.9543290 0.9735264 0.9626206 1.0000000
#all stocks for 500 days
allstockdata<-data.matrix(tail(sp500[],500))
row.names(allstockdata)<-NULL
cor_matrix<-mADCF(allstockdata, lags=0, unbiased = FALSE, output = TRUE)
write.table(cor_matrix, "C:/Users/syick/Desktop/r scripts/SP500_cor.txt", sep="\t")
cor_matrix_lag1<-mADCF(allstockdata, lags=1, unbiased = FALSE, output = TRUE)
write.table(cor_matrix_lag1, "C:/Users/syick/Desktop/r scripts/SP500_cor_lag1.txt", sep="\t")
cor_matrix_lag7<-mADCF(allstockdata, lags=7, unbiased = FALSE, output = TRUE)
write.table(cor_matrix_lag7, "C:/Users/syick/Desktop/r scripts/SP500_cor_lag7.txt", sep="\t")

#get names of the stocks
colnames(sp500)

#multiple time series
#non-linear time series correlation
library(dCovTS)
b<-mADCV(u, lags=1, unbiased = FALSE, output = TRUE) # trying to find Unbiased function and the meaning
b2<-data.frame(b)
plot(b2)
library(reshape2)
melted_b <- melt(b)
library(ggplot2)
ggplot(data = melted_b, aes(x=Var1, y=Var2, fill=value)) + geom_tile()

ggplot(data = melted_b, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "green", mid="white",
                       midpoint = 250, limit = c(0,500), space = "Lab", 
                       name="Non-linear\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()