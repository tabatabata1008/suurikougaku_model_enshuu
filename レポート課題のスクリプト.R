■課題 1 
install.packages("quantmod") 
install.packages("XML") # 命令文：パッケージのインストール 
library(quantmod) > library(XML) # 命令文：パッケージの読み込み

(1)
D1 <- getSymbols("KO", env=NULL, from="2014-01-01", to="2018-12-31") # 命令文：コカコーラ社（KO）の株価データ取得
chartSeries(D1, TA="addBBands()") # 命令文：D1の可視化 
(2)
D2 <- getSymbols("PFE", env=NULL, from="2014-01-01", to="2018-12-31") # 命令文：ファイザー社（PFE）の株価データ取得
chartSeries(D2, TA="addBBands()") # 命令文：D2の可視化 
(3)
D3 <- getSymbols("DIS", env=NULL, from="2014-01-01", to="2018-12-31") # 命令文：ウォルトディズニー社（DIS）の株価データ取得
chartSeries(D3, TA="addBBands()") # 命令文：D3の可視化 
(4)
D4 <- getSymbols("WMT", env=NULL, from="2014-01-01", to="2018-12-31") # 命令文：ウォルマート社（WMT）の株価データ取得
chartSeries(D4, TA="addBBands()") #　命令文：D4の可視化 
(5)
D5 <- getSymbols("DAL", env=NULL, from="2014-01-01", to="2018-12-31") # 命令文：デルタ航空（DAL）の株価データ取得
chartSeries(D5, TA="addBBands()") # 命令文：D5の可視化 


■課題 2 
(1)
P1 <- as.vector(D1[,4]) #  # 命令文：コカコーラ社の終値 
R1 <- P1[-1]/P1[-length(P1)] - 1 # コカコーラ社の収益率

P2 <- as.vector(D2[,4]) # 命令文：ファイザー社の終値 
R2 <- P2[-1]/P2[-length(P2)] - 1 # 命令文：ファイザー社の収益率

P3 <- as.vector(D3[,4]) # 命令文：ウォルトディズニー社の終値 
R3 <- P3[-1]/P3[-length(P3)] - 1 # 命令文：ウォルトディズニー社の収益率

P4 <- as.vector(D4[,4]) # 命令文：デルタ航空の終値 
R4 <- P4[-1]/P4[-length(P4)] - 1 # 命令文：ウォルマート社の収益率

P5 <- as.vector(D5[,4]) # 命令文：デルタ航空の終値 
R5 <- P5[-1]/P5[-length(P5)] - 1 # 命令文：ウォルトディズニー社の収益率


　　市場が年間250日なのでこのすうじでOK （うるう年含むから6日多い？）
R <- cbind(R1,R2,R3,R4,R5) # 命令文：5 銘柄の収益率の行列  
dim(R)
mu <- 250*apply(R, 2, mean) # 命令文：収益率の平均（年率換算） 
sigma <- sqrt(250)*apply(R, 2, sd) # 命令文：収益率の標準偏差（年率換算） 
Sigma <- 250*var(R) # 命令文：収益率の共分散行列（年率換算）
plot(sigma, mu, xlab="標準偏差(株式の価格変動リスク)", ylab="期待値(株式の収益性)", xlim=c(0,0.30),ylim=c(0,0.20), type="n") # 命令文：軸ラベルと軸範囲を指定し，マーカーは消す
text(sigma, mu, c("KO","PFE","DIS","WMT","DAL")) #命令文： 散布図に銘柄を表示


(2)(3)(4)
install.packages("Rsolnp") # 命令文：パッケージのインストール
library(Rsolnp) # 命令文：パッケージの読み込み

【プログラムコード】 
f1 <- function(w){ t(w)%*%Sigma%*%w } # 命令文：目的関数
g1 <- function(w){ c(mu%*%w, sum(w)) } # 命令文：等式条件のベクトル値関数 
rb <- 0.10 # 命令文：要求収益率を10％としている 
sol <- solnp(pars = rep(1/5,5), # 命令文：初期点 w=(w[1],w[2],w[3],w[4],w[5])=(1/5,1/5,1/5,1/5,1/5) 
　　fun = f1, eqfun = g1, # 命令文：目的関数と等式条件の関数 
　　eqB = c(rb,1), # 命令文：等式条件の右辺定数ベクトル 
　　LB = rep(0,5) # 命令文：重みベクトルの下限
) 
round(sol$pars, 2) # 命令文：最適解の出力

【実行結果】 
 

【プログラムコード】 
rb = seq(min(mu), max(mu), by=0.01) # 命令文：要求収益率の範囲 
W <- matrix(0, length(rb), 5) # 命令文：最適重み格納用 
Obj <- rep(0, length(rb)) # 命令文：目的関数値（分散）格納用 
for (k in 1:length(rb)){ # 命令文：異なる要求収益率　rb[k] で求解 
　　sol <- solnp(pars = rep(1/5,5), fun = f1, eqfun = g1,
　　　　eqB = c(rb[k],1), LB = rep(0,5)
　　) 
　　W[k,] <- sol$pars # 命令文：最適重みを保存 
　　Obj[k] <- tail(sol$values, 1) # 命令文：最終的な目的関数値を保存
}

sd <- sqrt(Obj) # 命令文：収益率の標準偏差
plot(sigma, mu, xlab="標準偏差(株式の価格変動リスク)", ylab="期待値(株式の収益性)", xlim=c(0,0.30), ylim=c(0,0.2), type="n") # 命令文：軸ラベルと範囲を指定し，マーカーは消す 
text(sigma, mu, c("KO","PFE","DIS","WMT","DAL")) # 命令文：散布図に銘柄を表示
points(sd, rb, col=2, pch=16) # 命令文：効率的フロンティアの描画

par(mar=c(5,5,5,5)) # 命令文：枠外にスペースを設ける
par(xpd=T) # 命令文：枠外に記入する許可
barplot(t(W), names.arg=round(rb,4), col=2:6, xlab="要求収益率",ylab="重み")# 命令文：最適重みの棒グラフの作成
legend(par()$usr[2],par()$usr[4], legend=c("KO","PFE","DIS","WMT","DAL"),col=2:6,lty=0,pch=15)# 命令文：凡例を枠外に表示させる

(5)
SR <- (rb - 0.01)/sd # 命令文：シャープ比率の計算
round(W[which.max(SR),], 2) # 命令文：2014~2018のシャープ比率が最大となる重みを出力 

(6)
sr <- round(W[which.max(SR),], 2)
D6 <- getSymbols("KO", env=NULL, from="2019-01-01", to="2019-12-31") # 命令文：コカコーラ社（KO）の株価データ取得
chartSeries(D6, TA="addBBands()") # 命令文：D6の可視化 
D7 <- getSymbols("PFE", env=NULL, from="2019-01-01", to="2019-12-31") # 命令文：ファイザー社（PFE）の株価データ取得
chartSeries(D7, TA="addBBands()") # 命令文：D7の可視化 
D8 <- getSymbols("DIS", env=NULL, from="2019-01-01", to="2019-12-31") # 命令文：ウォルトディズニー社（DIS）の株価データ取得
chartSeries(D8, TA="addBBands()") # 命令文：D8の可視化 
D9 <- getSymbols("WMT", env=NULL, from="2019-01-01", to="2019-12-31") # 命令文：ウォルマート社（WMT）の株価データ取得
chartSeries(D9, TA="addBBands()") #　命令文：D9の可視化 
D10 <- getSymbols("DAL", env=NULL, from="2019-01-01", to="2019-12-31") # 命令文：デルタ航空（DAL）の株価データ取得
chartSeries(D10, TA="addBBands()") # 命令文：D10の可視化 

P6 <- as.vector(D6[,4]) # 命令文：コカコーラ社の2019終値 
R6 <- P6[-1]/P6[-length(P6)] - 1 # 命令文：コカコーラ社の2019収益率
P7 <- as.vector(D7[,4]) # 命令文：ファイザー社の2019終値 
R7 <- P7[-1]/P7[-length(P7)] - 1 # 命令文：ファイザー社の2019収益率
P8 <- as.vector(D8[,4]) # 命令文：ウォルトディズニー社の2019終値 
R8 <- P8[-1]/P8[-length(P8)] - 1 # 命令文：ウォルトディズニー社の2019収益率
P9 <- as.vector(D9[,4]) # 命令文：デルタ航空の2019終値 
R9 <- P9[-1]/P9[-length(P9)] - 1 # 命令文：ウォルマート社の2019収益率
P10 <- as.vector(D10[,4]) # 命令文：デルタ航空の2019終値 
R10 <- P10[-1]/P10[-length(P10)] - 1 # 命令文：ウォルトディズニー社の2019収益率

R2019 <- cbind(R6*0.03,R7*0.38,R8*0.25,R9*0.01,R10*0.33) # 命令文：（5）のシャープ比率を考量した場合の5 銘柄の2019収益率の行列  
dim(R2019)

newmu <- 250*apply(R2019, 2, mean) # 命令文：（5）のシャープ比率を考量した場合の2019収益率の平均（年率換算） 
newsigma <- sqrt(250)*apply(R2019, 2, sd) # 命令文：（5）のシャープ比率を考量した場合の2019収益率の標準偏差（年率換算） 
newmu # 命令文：newmuの出力
newsigma # 命令文：newsigmaの出力

■課題 3
(1)
bankR <- 0.01/250 # 命令文：無リスクの銀行預金（年利子率 1%）の収益率
R_6 <- cbind(R1,R2,R3,R4,R5,bankR) # 命令文：6 銘柄の収益率の行列  
mu_6 <- 250*apply(R_6, 2, mean) # 命令文：6 銘柄収益率の平均（年率換算） 
sigma_6 <- sqrt(250)*apply(R_6, 2, sd) # 命令文：収益率の標準偏差（年率換算） 
plot(sigma_6, mu_6, xlab="標準偏差(株式の価格変動リスク)", ylab="期待値(株式の収益性)", xlim=c(0,0.30),ylim=c(0,0.20), type="n") # 命令文：軸ラベルと軸範囲を指定し，マーカーは消す
text(sigma_6, mu_6, c("KO","PFE","DIS","WMT","DAL","bank")) #命令文： 散布図に銘柄を表示

(2)
Sigma_6 <- 250*var(R_6) # 命令文：6 銘柄の収益率の共分散行列（年率換算）
f2 <- function(w){ t(w)%*%Sigma_6%*%w } # 命令文：目的関数
g2 <- function(w){ c(mu_6%*%w, sum(w)) } # 命令文：等式条件のベクトル値関数 
rb2 <- 0.10 # 命令文：要求収益率を10％としている 
sol2 <- solnp(pars = rep(1/6,6), # 命令文：初期点 w=(w[1],w[2],w[3],w[4],w[5],w[6])=(1/6,1/6,1/6,1/6,1/6,1/6) 
　　fun = f2, eqfun = g2, # 命令文：目的関数と等式条件の関数 
　　eqB = c(rb2,1), # 命令文：等式条件の右辺定数ベクトル 
　　LB = rep(0,6) # 命令文：重みベクトルの下限
) 
round(sol2$pars, 2) # 命令文：最適解の出力

rb2 = seq(min(mu_6), max(mu_6), by=0.01) # 命令文：要求収益率の範囲 
W2 <- matrix(0, length(rb2), 6) # 命令文：最適重み格納用 
Obj2 <- rep(0, length(rb2)) # 命令文：目的関数値（分散）格納用 
for (k in 1:length(rb2)){ # 命令文：異なる要求収益率　rb2[k] で求解 
　　sol2 <- solnp(pars = rep(1/6,6), fun = f2, eqfun = g2,
　　　　eqB = c(rb2[k],1), LB = rep(0,6)
　　) 
　　W2[k,] <- sol2$pars # 命令文：最適重みを保存 
　　Obj2[k] <- tail(sol2$values, 1) # 命令文：最終的な目的関数値を保存
}

sd_6 <- sqrt(Obj2) # 命令文：収益率の標準偏差
plot(sigma_6, mu_6, xlab="標準偏差(株式の価格変動リスク)", ylab="期待値(株式の収益性)", xlim=c(0,0.30), ylim=c(0,0.2), type="n") # 命令文：軸ラベルと範囲を指定し，マーカーは消す 
text(sigma_6, mu_6, c("KO","PFE","DIS","WMT","DAL","bank")) # 命令文：散布図に 6 銘柄を表示
points(sd_6, rb2, col=2, pch=16) # 命令文：効率的フロンティアの描画

(3)
par(mar=c(5,5,5,5)) # 命令文：枠外にスペースを設ける
par(xpd=T) # 命令文：枠外に記入する許可
barplot(t(W2), names.arg=round(rb2,4), col=2:7, xlab="要求収益率",ylab="重み")# 命令文：6 銘柄の最適重みの棒グラフの作成
legend(par()$usr[2],par()$usr[4], legend=c("KO","PFE","DIS","WMT","DAL","bank"),col=2:7,lty=0,pch=15)# 命令文：凡例を枠外に表示させる




■課題 4
(1)
【ソースコード】
install.packages("Rsolnp") # パッケージのインストール
library(Rsolnp) # パッケージの読み込み

fa <- function(x){ -x[1] + 2*((x[2])^2) - 3*((x[3])^3)} # 命令文：目的関数 
ha <- function(x){ c(x[1]+x[2], x[2]+x[3]) }# 命令文：等式条件のベクトル値関数 
ia <- function(x){ c(x[1], x[2], x[3]) }# 命令文：不等式条件のベクトル値
Ma <- 100 # 命令文：十分大きな正数 
sola <- solnp(pars = c(0.1,0.1,0.1), # 命令文：初期点 x=(x[1],x[2],x[3])=(0.1,0.1,0.1) 
　　fun = fa, eqfun = ha, ineqfun = ia,# 命令文：目的関数と等式条件と不等式条件の関数 
  eqB = c(1,1), # 命令文：等式条件の右辺定数ベクトル
  ineqLB = c(0,0,0), ineqUB = c(M,M,M) # 命令文：不等式条件の下限と上限
)
print(sola) # 命令文：計算結果の出力

(2)
install.packages("Rsolnp") # パッケージのインストール
library(Rsolnp) # パッケージの読み込み

fb <- function(x){ -2*x[1]*(x[2]^2) } # 命令文：目的関数 
hb <- function(x){ c(x[1]^2+2*(x[2]^2), x[1], x[2]) }# 命令文：不等式条件のベクトル値関数 
Mb <- 100 # 十分大きな正数 
solb <- solnp(pars = c(0.1,0.1), # 命令文：初期点 x=(x[1],x[2])=(0.1,0.1) 
　　fun = fb, ineqfun = hb, # 命令文：目的関数と不等式条件の関数 
　　ineqLB = c(-M,0,0), ineqUB = c(3,M,M) # 命令文：不等式条件の下限と上限
)
print(solb) # 命令文：計算結果の出力

