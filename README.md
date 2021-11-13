# Korea-GDP-growth
## hi
>hi
---
<!-- Bullet lis-->
* hi
* hi
- hi

<!--Code-->
```
gdp <- ts(gdp[,2], start = 1982, frequency = 4)
#gdp_gr2 <- ts(gdp_gr[,3], start = c(1982,2), frequency = 4)
gdp_gr <- ts((gdp-lag(gdp,-1))/lag(gdp,-1)*100,
             start=c(1982,2), frequency = 4)
```
