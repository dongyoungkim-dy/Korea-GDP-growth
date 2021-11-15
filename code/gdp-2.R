library(mFilter)

gdp <- read.csv("GDP_1982-2020.csv", header = TRUE)
gdp <- ts(gdp[,2], start = 1982, frequency = 4)
#gdp_gr2 <- ts(gdp_gr[,3], start = c(1982,2), frequency = 4)
#gdp_gr <- ts((gdp-lag(gdp,-1))/lag(gdp,-1)*100,start=c(1982,2), frequency = 4)
log.gdp <- log(gdp)
log.gdp.hp = mFilter(log.gdp, filter="HP")
gdp_t = exp(log.gdp.hp$trend)
gdpsam = exp((log.gdp+lag(log.gdp, -1)+lag(log.gdp, 1))/3)
gdp_c = gdpsam/gdp_t*100
gdp_c2 = gdp/gdp_t*100
gdp_i = gdp/gdpsam*100
plot(gdp_t, main="추세변동요인")
plot(gdp_t, main="추세변동요인 with 계절조정계열")
lines(gdp, lty=3, col = "blue")
plot(gdp_i, main="불규칙 변동요인")
plot(gdp_c, main="순환변동요인")

plot(gdp_c, main="순환변동요인", xaxt="none")
axis(1, seq(1982,2020,1), las = 2, cex.axis=0.7)
abline(v=seq(1982,2020,1), lty=3, col="gray")
abline(h=100,lty=3, col="blue")
                                      
