課題問題
課題1
(1)
Z <- read.csv("Automobile.csv", row.names=1) # 命令文：データの読み込み
Z1 <- na.omit(Z) # 命令文：欠損値（NA）を含む行の削除 
Z2　<-　Z1[,-9] # 命令文：engineLocation（全事例で一定値）を削除
dim(Z2)
Z2a <- Z2[sapply(Z2, is.factor)] # 命令文：文字列変数を抽出 
Z2b <- Z2[sapply(Z2, is.numeric)] # 命令文：数値変数を抽出

install.packages("caret") # 命令文：パッケージのインストール
library(caret) # 命令文：パッケージの読み込み 
tmp <- dummyVars(~., data=Z2a) # 命令文：tmp にダミー変数情報を格納 
Z3 <- as.data.frame(predict(tmp, Z2a)) # 命令文：文字列変数をダミー変数に変換
Z4 <- cbind(Z3,Z2b)
dim(Z4) # 命令文：Z3のデータフレームのサイズ表示

(2)
n <- nrow(Z4); p <- ncol(Z4)-1 # 命令文：事例と説明変数の数  
X <- Z4[,1:p]; y <- Z4[,p+1] # 命令文：説明変数と目的変数 
lm1 <- lm(y~., data=X) # 命令文：回帰式（フルモデル）の推定 
summary(lm1) # 命令文：結果の表示 

round(coef(lm1), 1) # 命令文：係数の表示
Predict <- predict(lm1) # 命令文：予測値を格納
Residuals <- residuals(lm1) # 命令文：残差を格納 
data.frame(Z4, Predict, Residuals) # 命令文：各事例の結果一覧 

(3)
とりあえず多重共線性の確認
install.packages("car") # 命令文：パッケージのインストール 
library(car) # 命令文：パッケージの読み込み
round(vif(lm1), 1) # 命令文：各変数の VIF 値の表示

lm0 <- lm(y~1, data=X) # 命令文：定数モデル（切片のみ）の推定 
lm1 <- lm(y~., data=X) # 命令文：フルモデル（全説明変数）の推定
step1 <- step(lm1, scope=list(upper=lm1, lower=lm0)) # 命令文：ステップワイズ法（探索開始：lm1，探索範囲：全説明変数） 
summary(step1) # 命令文：結果の表示 

課題2
(1)
D1 <- na.omit(Z) # 命令文：欠損値（NA）を含む行の削除 
D2 <- D1[sapply(D1, is.numeric)] # 数値変数を抽出
D3 <- scale(D2) # 数値変数を正規化（平均 0，標準偏差 1）
dim(D3)

(2)
n <- nrow(D3); p <- ncol(D3)-1 # 命令文：事例と説明変数の数  
X <- D3[,1:p]; y <- D3[,p+1] # 命令文：説明変数と目的変数 

install.packages("glmnet") # 命令文：パッケージのインストール 
library(glmnet) # 命令文：パッケージの読み込み 

par(mar=c(5,4.5,6,10.8)) # 命令文：枠外にスペースを設ける
par(xpd=T) # 命令文：枠外に記入する許可

gn1 <- glmnet(as.matrix(X), y, alpha=0) # 命令文：ridge 回帰
plot(gn1, xvar="lambda", xlab="正則化項の重み（の対数）",ylab="係数",col=rainbow(15),lwd = 2.6) # 命令文：lambda（の対数）に対する係数
legend(par()$usr[2],par()$usr[4], legend=c(colnames(X)), col=rainbow(15), lty=1,lwd = 2.6,pch=16) # 命令文：凡例を枠外表示

gn2 <- glmnet(as.matrix(X), y, alpha=1) # 命令文：lambda 回帰
plot(gn2, xvar="lambda", xlab="正則化項の重み（の対数）",ylab="係数",col=rainbow(15),lwd = 2.6) # 命令文：lambda（の対数）に対する係数
legend(par()$usr[2],par()$usr[4], legend=c(colnames(X)),col=rainbow(15), lty=1,lwd = 2.6,pch=16) # 命令文：凡例を枠外表示

gn3 <- glmnet(as.matrix(X), y, alpha=0.1) # 命令文：elastic net（α = 0.1） 
plot(gn3, xvar="lambda", xlab="正則化項の重み（の対数）",ylab="係数",col=rainbow(15),lwd = 2.6) # 命令文：lambda（の対数）に対する係数
legend(par()$usr[2],par()$usr[4], legend=c(colnames(X)),col=rainbow(15), lty=1,lwd = 2.6,pch=16) # 命令文：凡例を枠外表示

t(coef(gn1, s=exp(4))) # 命令文：lambda = exp(4) の係数
predict(gn1, newx=as.matrix(X), s=exp(4)) # 命令文：lambda = exp(4) の予測値 

(4)
set.seed(777) # 命令文：乱数の種（分割は乱数依存） 
cv.gn1 <- cv.glmnet(as.matrix(X), y, nfold=5, alpha=0) # 命令文：ridge 回帰の 5 分割交差確認
plot(cv.gn1,xlab="正則化項の重み（の対数）",ylab="平均残差二乗和（予測誤差）") # 命令文：lambda に対する平均残差二乗和（予測誤差）の図示
t(coef(cv.gn1, s=cv.gn1$lambda.min)) # 命令文：ridge 回帰の平均残差二乗和が最小となる係数 

set.seed(777) # 命令文：乱数の種（分割は乱数依存）
cv.gn2 <- cv.glmnet(as.matrix(X), y, nfold=5, alpha=1) # 命令文：lasso 回帰の 5 分割交差確認
plot(cv.gn2,xlab="正則化項の重み（の対数）",ylab="平均残差二乗和（予測誤差）") # 命令文：lambda に対する平均残差二乗和（予測誤差）の図示
t(coef(cv.gn2, s=cv.gn2$lambda.min)) # 命令文：lasso 回帰の平均残差二乗和が最小となる係数 

set.seed(777) # 命令文：乱数の種（分割は乱数依存）
cv.gn3 <- cv.glmnet(as.matrix(X), y, nfold=5, alpha=0.1) # 命令文：elastic net（α = 0.1） の 5 分割交差確認
plot(cv.gn3,xlab="正則化項の重み（の対数）",ylab="平均残差二乗和（予測誤差）") # 命令文：lambda に対する平均残差二乗和（予測誤差）の図示
t(coef(cv.gn3, s=cv.gn3$lambda.min)) # 命令文：elastic net（α = 0.1） の平均残差二乗和が最小となる係数 


課題3
(1)
A <- read.csv("Prices.csv", row.names=1) # 命令文：データの読み込み
N <- nrow(A); P <- ncol(A)-1 # 命令文：事例と説明変数の数 
xx <- A[,1:P]; YY <- A[,P+1] # 命令文：説明変数と目的変数 
                                                  
a0 <- matrix(1, nrow=10, ncol=1) # 命令文：すべて1で10行1列の行列作成
x <- as.matrix(cbind(a0,xx)) # 命令文：結合して説明変数データを作る
Y <- as.matrix(YY) # 命令文：目的変数データ作成
xt <- t(x) # 命令文：xの転置
a <- solve(xt%*%x)%*%xt%*%Y # 命令文：定義式から最小2乗推定量を求める
round(a,1) # 命令文：出力

lm00 <- lm(YY~., data=xx) # 命令文：関数lmの設定                                  
summary(lm00)  # 命令文：結果出力
round(coef(lm00), 1) # 係数の表示


(2)
epsi <- Y-x%*%a # 命令文：残差の定義
sumepsi <- t(epsi)%*%(epsi) # 命令文：残差二乗和

sumy <- t(Y-mean(Y))%*%(Y-mean(Y)) # 命令文：目的変数の分散
Rsq2 <- 1-(sumepsi/sumy) # 命令文：決定変数R^2
round(Rsq2,4) # 命令文：下4桁までで表す
Rr <- 1-((sumepsi/(N-1-P)) / (sumy/(N-1))) # 命令文：
round(Rr,4) # 命令文：下4桁までで表す

(3)
R <- cor(as.matrix(xx)) # 命令文：説明変数行列の相関行列
vif <- solve(R) # 命令文：Rの逆行列
VIF <- diag(vif) # 命令文：対角成分
round(VIF,1) # 命令文：下1桁までで表す

> round(vif(lm00), 1) # 命令文：各変数の VIF 値の表示
   Space      Age Distance  Minutes 
     1.2      1.1    858.7    861.6 

(4)
AIC <- N*log(sumepsi/N) + 2*(P+1) # 命令文：定義式からAICを求める
round(AIC,2) # 命令文：下2桁まででAICを出力する

lm11 <- lm(YY~., data=xx) # 命令文：フルモデル（全説明変数）の推定
step11 <- step(lm11, scope=list(upper=lm11,lower=lm00)) # 命令文：関数 step の設定
summary(step11) # 命令文：結果表示

(5)
Yt <- t(Y) # 命令文：Yの転置
f1 <- function(a1){ # 命令文：f1はa1を引数とする関数
  return(f = (Yt%*%Y) - (2*Yt%*%x%*%(a1)) + (t(a1)%*%xt%*%x%*%a1) ) # 命令文：最小二乗推定量をa1の関数で定義する
}

optim(c(0,0,0,0,0),f1,method="BFGS")  # 命令文：初期点 a1=(0,0,0,0) から関数 f1 を準ニュートン法で最小化 

summary(lm00)  # 命令文：関数 lm の結果表示

(6)
newx <- scale(xx) # 命令文：説明変数を標準化（平均 0，標準偏差 1）
newY <- scale(YY) # 命令文：目的変数を標準化（平均 0，標準偏差 1）
lam <- exp(-2)

f2 <- function(a){  # 命令文：f1は(1)のaを引数とする関数
  newsumepsi <- 0 # 命令文：newsumepsiの初期値は0
  suma <- 0 # 命令文：sumaの初期値は0
  for(i in 1:N){ # 命令文：iを1からNまで1ずつ増やす
    newsumepsi <- newsumepsi + (newY[i] -a[1] -a[2]*newx[i,1] -a[3]*newx[i,2] -a[4]*newx[i,3] -a[5]*newx[i,4])^2 # 命令文：残差二乗和を定義式から求める
  }
  suma <- t(a)%*%a # 命令文：定義式からa^2の和を表す
  return(f = newsumepsi/(2*N) + (lam/2)*suma) # 命令文：λ = exp(?2) の ridge 回帰の係数を求める
}

optim(c(0,0,0,0,0),f2,method="BFGS") # 命令文：初期点 a=(0,0,0,0,0) から関数 f2 を準ニュートン法で最小化 

gn11 <- glmnet(as.matrix(newx), newY, alpha=0) # 命令文：データを標準化した場合のridge 回帰 
t(coef(gn11, s=exp(-2))) # 命令文：lambda = exp(-2) のridge回帰の係数


