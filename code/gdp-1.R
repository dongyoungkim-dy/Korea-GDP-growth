library(tseries)
library(forecast)

gdp <- read.csv("GDP_1982-2020.csv", header = TRUE)

#gdp_gr<- read.csv("GDP_gr.csv", header = TRUE)

  
gdp <- ts(gdp[,2], start = 1982, frequency = 4)
#gdp_gr2 <- ts(gdp_gr[,3], start = c(1982,2), frequency = 4)
gdp_gr <- ts((gdp-lag(gdp,-1))/lag(gdp,-1)*100,
             start=c(1982,2), frequency = 4)

###log gdp
lo.gdp <- log(gdp)
###gdp diff
diff1.gdp <- diff(gdp, 1)

diff1.lo.gdp <- diff(lo.gdp, 1)
diff2.lo.gdp <- diff(lo.gdp, 2)
diff3.lo.gdp <- diff(lo.gdp, 3)
diff4.lo.gdp <- diff(lo.gdp, 4)
auto.arima(gdp)
auto.arima(lo.gdp)
####### gdp growth
plot(gdp_gr, ylab="GDP(조 원)", xlab="연도", col="black", main="GDP growth of Korea, 1982-2020")
#plot(gdp_gr2/1000)
plot(diff1.gdp <- diff(gdp, 1))
plot(log(gdp))
plot(gdp)
plot(diff1.lo.gdp)
plot(diff2.lo.gdp)
plot(diff3.lo.gdp)
plot(diff4.lo.gdp)
##### gdp growth traits
spectrum(gdp_gr, spans=c(3,3), main="스팩트럼") #no seasonal factor
par(mfrow=c(2,1))
acf(gdp_gr, main="상관도표") 
pacf(gdp_gr, main="부분상관도표") 
### LOG GDP, diff log gdp
acf(lo.gdp, main="상관도표") 
pacf(lo.gdp, main="부분상관도표") 
acf(diff1.lo.gdp, main="상관도표") 
pacf(diff1.lo.gdp, main="부분상관도표") 
acf(diff4.lo.gdp, main="상관도표") 
pacf(diff4.lo.gdp, main="부분상관도표") 

adf.test(gdp_gr) # 단위근 없음 
adf.test(log(gdp))
adf.test(diff1.lo.gdp) # 단위근 없음 
adf.test(diff4.lo.gdp) # 단위근 없음 
Box.test(gdp_gr, lag=8, type="Ljung") #8차까지 자기상관계수가 0이라는 귀무가설
Box.test(log(gdp), lag=8, type="Ljung") #8차까지 자기상관계수가 0이라는 귀무가설
Box.test(diff1.lo.gdp, lag=8, type="Ljung") #8차까지 자기상관계수가 0이라는 귀무가설
Box.test(diff4.lo.gdp, lag=8, type="Ljung") #8차까지 자기상관계수가 0이라는 귀무가설

###### arima 모형추정
gdp_gr_fit1 = arima(gdp_gr, order=c(1,1,0))
gdp_gr_fit2 = arima(gdp_gr, order=c(0,1,1))
gdp_gr_fit3 = arima(gdp_gr, order=c(0,0,1))
gdp_gr_fit3 = arima(gdp_gr, order=c(1,0,0))
gdp_gr_fit3 = arima(gdp_gr, order=c(1,0,1))

summary(gdp_gr_fit1)
gdp_log_fit1 = arima(gdp, order=c(1,1,0))
gdp_log_fit2 = arima(log(gdp), order=c(0,1,1))
gdp_diff1_fit1 = arima(diff1.lo.gdp, order=c(1,1,0))
gdp_diff1_fit2 = arima(diff1.lo.gdp, order=c(0,1,1))
gdp_diff4_fit1 = arima(diff4.lo.gdp, order=c(1,1,0))
gdp_diff4_fit2 = arima(diff4.lo.gdp, order=c(0,1,1))


##### arima 모형의 진단
tsdiag(gdp_gr_fit1)  ### 잔차분석
tsdiag(gdp_gr_fit2)  ### 잔차분석
tsdiag(gdp_log_fit1)  ### 잔차분석
tsdiag(gdp_log_fit2)  ### 잔차분석
tsdiag(gdp_diff1_fit1)  ### 잔차분석
tsdiag(gdp_diff1_fit2)  ### 잔차분석
tsdiag(gdp_diff4_fit1)  ### 잔차분석
tsdiag(gdp_diff4_fit2)  ### 잔차분석
plot(forecast(gdp_gr_fit, h=4))   #### 향후 4분기 예측
summary(forecast(gdp_gr_fit, h=4))
