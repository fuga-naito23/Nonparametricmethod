library(MASS)
n4 <- 25 #データ数
mu <- 0 #正規分布のパラメーター （平均）
sg <- 1 #正規分布のパラメーター (標準偏差)
min.score <- c() #配列を準備
max.score <- c() #配列を準備
for (i in 1:500) {
  testdate4 <- rnorm(n4, mu, sg)
  #標準正規分布に従う乱数発生
  min.score[i] <- min(testdate4)
  max.score[i] <- max(testdate4)
}
minmin <- min(min.score-1) #PDFの範囲指定
minmax <- max(min.score+1) #PDFの範囲指定
## 最小値の密度関数
xv4 <- seq(minmin, minmax, 0.1)
yv4 <- n4*(1-pnorm(xv4, 0, 1))^(n4-1)*dnorm(xv4, 0, 1)

truehist(min.score, col="pink")
lines(xv4, yv4, col="blue")

maxmin <- min(max.score-1)
maxmax <- max(max.score+1)

##最大値の密度関数
xv5 <- seq(maxmin, maxmax, 0.1)
yv5 <- n4*(pnorm(xv5, 0, 1))^(n4-1)*dnorm(xv5,0,1)
truehist(max.score, col = "pink")
lines(xv5,yv5, col="blue")

  
  