課題１
#まずは確認
(a <- matrix(c(2,7,3), 3, 1))
(b <- matrix(c(6,-3,1), 3, 1))
(A <- matrix(c(2,4,1,2,-3,5), 2, 3))
(B <- matrix(c(-1,2,3,-2,0,3), 3, 2))
(AT <- t(A)) 
(BT <- t(B)) 
#ここから計算
(1)
a+b

(2)
a*b 

(3)
AT+B

(4)
A+BT

(5)
A%*%B 

(6)
B%*%A 

課題２
(1)
(A <- matrix(c(-1,2,3,1,4,2,2,2,7), 3, 3))
(A_inv <- solve(A)) 

(2)
(Y <- matrix(c(2,3,2,-3,5,-7,3,5,-2), 3, 3)); (Z <- c(-2,1,4)) 
solve(Y, Z) #連立方程式「Yx =Z」の解　x　の計算 

(3)
(A <- matrix(c(-1,1,4,1,2,3,4,3,2), 3, 3))
eigen(A) # 行列 A の固有値と固有ベクトル


課題3
（1）
D1 <- read.csv("Automobile.csv", row.names=1) # データの読み込み 
D2 <- na.omit(D1) # 欠損値（NA）を含む行の削除 
D3 <- dim(D2)
D3[1]

（2）
D4 <- D2$price # 列 priceの抽出 
D5 <- summary(D4) # オブジェクトの基本統計量
D5[1] # 最小値
D5[3] # 平均値
D5[4] #　中央値
D5[6]　#　最大値
sd(D4) # 標準偏差

(3)
D6 <- D2[,22:25] # 各列の抽出
cor(D6) # 相関行列

(4)
D7 <- tapply(D2$price, D2$bodyStyle, mean) # 変数 bodyStyle による変数 price の層別平均
M7 <- as.matrix(D7) # 行列に変換
barplot(M7[,1],xlab="bodyStyle", ylab="price", names.arg=rownames(M7)) #棒グラフ

(5)
D8 <- cbind(D2[22], D2[24]) # データを結合 
M8 <- as.matrix(D8) # 行列に変換
dim(M8)
M8
plot(M8[,1], M8[,2], xlab="horsepower", ylab="cityMPG", xlim=c(0,200), ylim=c(0,50)) # horsepower と cityMPG の散布図

(6)
D9 <- subset(D2,bodyStyle == "sedan")
D10 <- D9[order(D9[,17]),] # engineSizeの昇順に並び替え 
D11 <- scale(D10[,11:13])
dim(D11)
matplot(D11,xlab="sedanのうちengineSizeの昇順", ylab="標準化された値",type="b", pch=1:5) # セダンの80台分の標準化された各変数の折れ線グラフ 
legend("topright", legend=c("length","width","height"), pch=1:5, col=1:5, lty=1:5) # 右上に凡例を表示

(7)
hist(D4,xlab="price", ylab="度数", breaks=15) # priceをヒストグラム化して階級を15分割 

(8)
D12 <-  D2[,24:25] #  cityMPG，highwayMPG の抽出
D12 
boxplot(D12, ylab="value") # cityMPG，highwayMPGの箱ひげ図


課題4
(1)
x <- 0 # x の初期値は 0
for(k in 5:25){ # k を 5 から 25 まで 1 ずつ増やす 
  x <- x+sin(k) # 求める関数
} 
x # 結果の出力 

（2）
x <- 2 # x に 2 を代入する 
if(x^2 >= 10){ # 条件式：x^2 は 10　より大きい 
　　paste(x, " の 2 乗は 10 以上") # 条件式が真の場合
}else{
　　paste(x, " の 2 乗は 10 より小さい") # 条件式が偽の場合
}

x <- 5 # x に 5 を代入する 
if(x^2 >= 10){ # 条件式：x^2 は 10　より大きい 
　　paste(x, " の 2 乗は 10 以上") # 条件式が真の場合
}else{
　　paste(x, " の 2 乗は 10 より小さい") # 条件式が偽の場合
}

(3)
fun <- function(a,b){ # fun は a と b を引数とする関数 
  x <- 1 # x の初期値は 1
  for(k in a:b){ # k を a から b まで 1 ずつ増やす 
    if(k%%2 == 1){ # 条件式：k を 2 で割ると余りは 1 
      x <- x * k # 条件式が真の場合：x に k をかける
    }
  }
 return(paste("答えは", x,"です．")) # 結果の出力
}
fun(-5,10)

(4)
fun <- function(M){ # funSign は行列 M を引数とする関数 
  m <- nrow(M); n <- ncol(M) # M の行数と列数  
  for(i in 1:m){ # i を 1 から m まで 1 ずつ増やす 
    for(j in 1:n){ # j を 1 から n まで 1 ずつ増やす 
      if(M[i,j] != 0){ # M の (i,j) 成分が非0ならば 
        Mk <- matrix(c(i,j,M[i,j]) , 1,3) 
        print(Mk) 
      }
    }
  }
} 
M <- matrix(c(0,0,0,0,3,0,1,0,0,0,0,0,0,0,2,0), 4, 4) # 準備 
fun(M) # 疎形式を出力 


(5)
fun1 <- function(x){ # fun1 はx を引数とする関数 
  return(f = (2*x-1) / (x^2 + 5) ) # 関数の定義
}
optim(0, fun1, method="BFGS") # 最小化
optim(0, fun1, method="BFGS",control=list(fnscale=-1)) # 最大化

