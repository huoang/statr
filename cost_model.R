min f（x）=100（x[2]-x[1]^2）^2+（1-x[1]）^2
#写出目标函数
obj<-function(x){
  f<-c(10*(x[2]-x[1]^2), 1-x[1])
  sum(f^2)
}

b1 <- 0.04757357   
b7 <- 0.05719350

obj <- function(x){
     f <- c(b1-x,b7-x)
     sum(f^2)
  } 

x0 <- 100
nlm(obj,x0)

class(x0)

w1 <- 0.5
w7 <- 0.5

w1*b1+w7*b7

s1 <- 0.00539679
s7 <- 0.00659101


obj <- function(x){
  f <- c((b1-x)/s1,(b7-x)/s7)
  sum(f^2)
} 
x0 <- 2000
nlm(obj,x0)

w1 <- (1/s1^2)/(1/s1^2+1/s7^2)
w7 <- 1-w1

w1*b1+w7*b7

s11 <- 0.0000291254
s77 <- 0.0000434414
s17 <- 0.0000189242
sqrt(0.0000291254)
obj <- function(x){
  f <- c((b1-x)/s11,(b7-x)/s17,
         (b1-x)/s17,(b7-x)/s77)
  sum(f^2)
} 
x0 <- 2000
nlm(obj,x0)

w1=(s11+s17)/(s11+2*s17+s77)
w7=1-w1
w1*b1+w7*b7

cost08 <- read_csv('h:/pyr/data/cost_model_08.csv',col_names = TRUE,
                                         col_types = NULL,
                                        locale(encoding = 'gbk'))


cost09 <- read_csv('h:/pyr/data/cost_model_09.csv',col_names = TRUE,
                   col_types = NULL,
                   locale(encoding = 'gbk'))

cost10 <- read_csv('h:/pyr/data/cost_model_10.csv',col_names = TRUE,
                   col_types = NULL,
                   locale(encoding = 'gbk'))

cost11 <- read_csv('h:/pyr/data/cost_model_11.csv',col_names = TRUE,
                   col_types = NULL,
                   locale(encoding = 'gbk'))

names(cost09)[37:39] <- names(cost08)[37:39]
names(cost10)[37:39] <- names(cost08)[37:39]
names(cost11)[37:39] <- names(cost08)[37:39]


cost08$year <- '2008'
cost09$year <- '2009'
cost10$year <- '2010'
cost11$year <- '2011'


library(readstata13)

cost_all  <- rbind(cost08,cost09,cost10,cost11)

write_csv(cost_all,'h:/pyr/data/cost_all.csv')

?write_csv

rlt_lm_08 <- lm(log(cost)~ outp +opsqr +opcubed +inp +inpsqr +inpcubed+
     wageindex +beds +fixed +cmi,data=cost08)

cost_all <- read.dta13('d:/statr/data/cost_all.dta')

summary(rlt_lm_08)

str(rlt_lm_08)

hist(rlt_lm_08$residuals)

res <- rlt_lm_08$residuals

res <- cost_all$ei

rlt<-fitdistr(x = res,
         densfun = dnorm,
         start = list(mean=1,sd=1),# need to provide named list of starting values
         lower = list(mean = 0,sd=0))

para_mean <- rlt$estimate[1]
para_sd <- rlt$estimate[2]

d <- dnorm(res,para_mean,para_sd)

df <- data.frame(res,d)

ggplot(df,aes(x = res,y=..density..)) +
  geom_histogram(fill = 'cornsilk',color = 'purple') +
  #geom_density()   +
  geom_line(aes(x=res,y=d),colour="red")

x1 <- sample(res,15)
ks.test(x1,'pnorm',para_mean,para_sd)

p_llogis <- pllogis(x,sha_llogis,rate_llogis)

p_llogis_br <- pllogis(breaks,sha_llogis,rate_llogis)

p_llogis_br[length(p_llogis_br)] <- 1

p_llogis_br <- diff(p_llogis_br)

freq_llogis<-length(x)*p_llogis_br

sum((freq_llogis-freq_x)^2/freq_llogis)

chisq.test(freq_x,p=(freq_llogis/length(x)))


library(dplyr)
library(readr)
library(ggplot2)
library(MASS)
library(car)
library(systemfit)
rev <- read_csv('d:/statr/data/fee_inp.csv',col_names = TRUE,
                   col_types = NULL,
                   locale(encoding = 'gbk'))
rev <- rev[c(3,20)]

names(rev) <- c('inprev','inps')

rev$inprev <-  rev$inprev/10000
rev$inps <- rev$inps/10000

ggplot(rev,aes(x=inps,y=inprev))+geom_point()

cost_all$cons <- 1

x_2008 = cost_all %>% filter(year == '2008') %>%
        select(cons,outp,opsqr,opcubed,inp,inpsqr,inpcubed,
        wageindex,beds,fixed,cmi)
x_2009 = cost_all %>% filter(year == '2009') %>%
  select(cons,outp,opsqr,opcubed,inp,inpsqr,inpcubed,
         wageindex,beds,fixed,cmi)
x_2010 = cost_all %>% filter(year == '2010') %>%
  select(cons,outp,opsqr,opcubed,inp,inpsqr,inpcubed,
         wageindex,beds,fixed,cmi)
x_2011 = cost_all %>% filter(year == '2011') %>%
  select(cons,outp,opsqr,opcubed,inp,inpsqr,inpcubed,
         wageindex,beds,fixed,cmi)

y_2008 <- cost_all$cost[cost_all$year == '2008']
y_2009 <- cost_all$cost[cost_all$year == '2009']
y_2010 <- cost_all$cost[cost_all$year == '2010']
y_2011 <- cost_all$cost[cost_all$year == '2011']

x_2008_m <- as.matrix(x_2008)
x_2009_m <- as.matrix(x_2009)
x_2010_m <- as.matrix(x_2010)
x_2011_m <- as.matrix(x_2011)


y_2008_m <- as.matrix(y_2008)
y_2009_m <- as.matrix(y_2009)
y_2010_m <- as.matrix(y_2010)
y_2011_m <- as.matrix(y_2011)
dim(x_2008_m)


##############################2008========================
xm <- cbind(x_2008_m,x_2009_m,
      x_2010_m,x_2011_m)
head(xm)
dim(xm)

xm <- xm[,-c(1,12,23,34)]

ymx_2008 <- cbind(y_2008_m,xm)
colnames(ymx_2008)[1] <- 'y2008'
ymx_2008 <- as.data.frame(ymx_2008)
colnames(ymx_2008)[2:41] <-paste('x',c(1:40),sep='') 

dim(xm)
dim(ymx_2008)
head(ymx_2008)
X <- ymx_2008[2:41]
X$cons <-1
dim(X)
X <- as.matrix(X)
head(X)
Y <- log(ymx_2008[1])
Y <- as.matrix(Y)
dim(Y)
class(Y)
m_rlt_2008 <- solve(t(X)%*%X,tol=1e-21)%*%t(X)%*%Y
rlt_2008 <- lm(log(y2008) ~.,data = ymx_2008 )
summary(rlt_2008)
#str(rlt)
rlt_2008$coefficients[2]
m_rlt_2008[1,1]


###########################2009=========================
xm <- cbind(x_2009_m,x_2008_m,
            x_2010_m,x_2011_m)
head(xm)
dim(xm)
xm <- xm[,-c(1,12,23,34)]
ymx_2009 <- cbind(y_2009_m,xm)
colnames(ymx_2009)[1] <- 'y2009'
ymx_2009 <- as.data.frame(ymx_2009)
colnames(ymx_2009)[2:41] <-paste('X2011',c(1:40),sep='') 
dim(xm)
dim(ymx_2009)
head(ymx_2009)
X <- ymx_2009[2:41]
X$cons <-1
dim(X)
X <- as.matrix(X)
head(X)
Y <- log(ymx_2009[1])
Y <- as.matrix(Y)
dim(Y)
class(Y)
m_rlt_2009 <- solve(t(X)%*%X,tol=1e-21)%*%t(X)%*%Y
rlt_2009 <- lm(log(y2009) ~.,data = ymx_2009 )
summary(rlt_2009)
#str(rlt)
rlt_2009$coefficients[2]
m_rlt_2009[1,1]

###########################2010=========================
xm <- cbind(x_2010_m,x_2008_m,
            x_2009_m,x_2011_m)
head(xm)
dim(xm)
xm <- xm[,-c(1,12,23,34)]
ymx_2010 <- cbind(y_2010_m,xm)
colnames(ymx_2010)[1] <- 'y2010'
ymx_2010 <- as.data.frame(ymx_2010)
colnames(ymx_2010)[2:41] <-paste('x',c(1:40),sep='') 
dim(xm)
dim(ymx_2010)
head(ymx_2010)
X <- ymx_2010[2:41]
X$cons <-1
dim(X)
X <- as.matrix(X)
head(X)
Y <- log(ymx_2010[1])
Y <- as.matrix(Y)
dim(Y)
class(Y)
m_rlt_2010 <- solve(t(X)%*%X,tol=1e-21)%*%t(X)%*%Y
rlt_2010 <- lm(log(y2010) ~.,data = ymx_2010 )
summary(rlt_2010)
#str(rlt)
rlt_2010$coefficients[2]
m_rlt_2010[1,1]


###########################2011=========================
#xm <- cbind(x_2011_m,x_2008_m,
#            x_2009_m,x_2010_m)

ymx_2011 <- cbind(y_2011_m,xm)
colnames(ymx_2011)[1] <- 'y2011'
ymx_2011 <- as.data.frame(ymx_2011)
colnames(ymx_2011)[2:41] <-paste('X2011',c(1:40),sep='') 
dim(xm)
dim(ymx_2011)
head(ymx_2011)
X2011 <- ymx_2011[2:41]
X2011$cons <-1
dim(X2011)
X2011 <- as.matrix(X2011)
head(X2011)
Y2011 <- log(ymx_2011[1])
Y2011 <- as.matrix(Y2011)
dim(Y2011)
class(Y2011)
m_rlt_2011 <- solve(t(X2011)%*%X2011,tol=1e-21)%*%t(X2011)%*%Y2011
rlt_2011 <- lm(log(y2011) ~.,data = ymx_2011)
summary(rlt_2011)
#str(rlt)
rlt_2011$coefficients[2]
m_rlt_2011[1,1]







##############################second2008========================
x_2008_m <- as.data.frame(x_2008_m)
x_2009_m <- as.data.frame(x_2009_m)
x_2010_m <- as.data.frame(x_2010_m)
x_2011_m <- as.data.frame(x_2011_m)

x_2008_m$opxip <- x_2008_m$outp*x_2008_m$inp
x_2009_m$opxip <- x_2009_m$outp*x_2009_m$inp
x_2010_m$opxip <- x_2010_m$outp*x_2010_m$inp
x_2011_m$opxip <- x_2011_m$outp*x_2011_m$inp


xm <- cbind(x_2008_m,x_2009_m,
            x_2010_m,x_2011_m)
dim(xm)

xm <- xm[,-c(1,13,25,37)]
head(xm)

colnames(xm)[1:11] <-c("op08","op208","op308",
                       "inp08","inp208","inp308",
                       "wgix08","beds08","fix08",
                       "cmi08",'oxi08') 
colnames(xm)[12:22] <-c("op09","op209","op309",
                       "inp09","inp209","inp309",
                       "wgix09","beds09","fix09",
                       "cmi09",'oxi09') 

colnames(xm)[23:33] <-c("op10","op210","op310",
                        "inp10","inp210","inp310",
                        "wgix10","beds10","fix10",
                        "cmi10",'oxi10') 

colnames(xm)[34:44] <-c("op11","op211","op311",
                        "inp11","inp211","inp311",
                        "wgix11","beds11","fix11",
                        "cmi11",'oxi11') 
#########################2008sec================================
ymx_2008 <- cbind(y_2008_m,xm)
colnames(ymx_2008)[1] <- 'y2008'
dim(ymx_2008)
#ymx_2008 <- ymx_2008[,
#            c(1,2,13,24,35,5,16,27,38,3:4,6:7,12,8:10)]
ymx_2008 <- ymx_2008[,
               c('y2008',"op08",'op09','op10','op11',
              "inp08",'inp09','inp10','inp11',
              "op208","op308","inp208","inp308",'oxi08',
              "wgix08","beds08","fix08","cmi08")]
ymx_2008 <- as.data.frame(ymx_2008)
 
dim(xm)
dim(ymx_2008)
head(ymx_2008)
X2008 <- ymx_2008[2:18]
X2008$cons <-1
dim(X2008)
X2008 <- as.matrix(X2008)
head(X2008)
Y2008 <- log(ymx_2008[1])
Y2008 <- as.matrix(Y2008)
dim(Y2008)
class(Y2008)
m_rlt_2008 <- solve(
  t(X2008)%*%X2008,tol=1e-21)%*%t(X2008)%*%Y2008
rlt_2008 <- lm(log(y2008) ~.,data = ymx_2008 )
summary(rlt_2008)
#str(rlt)
rlt_2008$coefficients[2]
m_rlt_2008[1,1]


###########################sec2009=========================

ymx_2009 <- cbind(y_2009_m,xm)
colnames(ymx_2009)[1] <- 'y2009'
ymx_2009 <- ymx_2009[,
            c('y2009',"op08",'op09','op10','op11',
              "inp08",'inp09','inp10','inp11',
              "op209","op309","inp209","inp309",'oxi09',
              "wgix09","beds09","fix09","cmi09")]
ymx_2009 <- as.data.frame(ymx_2009)
dim(xm)
dim(ymx_2009)
head(ymx_2009)
X2009 <- ymx_2009[2:18]
X2009$cons <-1
dim(X2009)
X2009 <- as.matrix(X2009)
head(X2009)
Y2009 <- log(ymx_2009[1])
Y2009 <- as.matrix(Y2009)
dim(Y2009)
class(Y2009)
m_rlt_2009 <- solve(t(X2009)%*%X2009,tol=1e-21)%*%t(X2009)%*%Y2009
rlt_2009 <- lm(log(y2009) ~.,data = ymx_2009 )
summary(rlt_2009)
#str(rlt)
rlt_2009$coefficients[2]
m_rlt_2009[1,1]

###########################sec2010=========================
ymx_2010 <- cbind(y_2010_m,xm)
colnames(ymx_2010)[1] <- 'y2010'
ymx_2010 <- ymx_2010[,
                     c('y2010',"op08",'op09','op10','op11',
                       "inp08",'inp09','inp10','inp11',
                       "op210","op310","inp210","inp310",
                       'oxi10',"wgix10","beds10","fix10",
                       "cmi10")]
ymx_2010 <- as.data.frame(ymx_2010)
dim(xm)
dim(ymx_2010)
head(ymx_2010)
X2010 <- ymx_2010[2:18]
X2010$cons <-1
dim(X2010)
X2010 <- as.matrix(X2010)
head(X2010)
Y2010 <- log(ymx_2010[1])
Y2010 <- as.matrix(Y2010)
dim(Y2010)
class(Y2010)
m_rlt_2010 <- solve(t(X2010)%*%X2010,tol=1e-21)%*%t(X2010)%*%Y2010
rlt_2010 <- lm(log(y2010) ~.,data = ymx_2010 )
summary(rlt_2010)
#str(rlt)
rlt_2010$coefficients[2]
m_rlt_2010[1,1]

###########################sec2011=========================
ymx_2011 <- cbind(y_2011_m,xm)
colnames(ymx_2011)[1] <- 'y2011'
ymx_2011 <- ymx_2011[,
                     c('y2011',"op08",'op09','op10','op11',
                       "inp08",'inp09','inp10','inp11',
                       "op211","op311","inp211","inp311",
                       'oxi11',"wgix11","beds11","fix11",
                       "cmi11")]
ymx_2011 <- as.data.frame(ymx_2011)
dim(xm)
dim(ymx_2011)
head(ymx_2011)
X2011 <- ymx_2011[2:18]
X2011$cons <-1
dim(X2011)
X2011 <- as.matrix(X2011)
head(X2011)
Y2011 <- log(ymx_2011[1])
Y2011 <- as.matrix(Y2011)
dim(Y2011)
class(Y2011)
m_rlt_2011 <- solve(
  t(X2011)%*%X2011,tol=1e-21)%*%t(X2011)%*%Y2011
rlt_2011 <- lm(log(y2011) ~.,data = ymx_2011 )
summary(rlt_2011)
#str(rlt)
rlt_2011$coefficients[2]
m_rlt_2011[1,1]

#################         ==============================
m_pi <- cbind(m_rlt_2008,m_rlt_2009,m_rlt_2010,m_rlt_2011)
nrow(m_pi)
m_pi <- rbind(m_pi[18,],m_pi[1:17,])

rbind(ymx_2008,ymx_2009,ymx_2010,ymx_2011)


sum_rlt_2008 <- summary(rlt_2008)
sum_rlt_2009 <- summary(rlt_2009)
sum_rlt_2010 <- summary(rlt_2010)
sum_rlt_2011 <- summary(rlt_2011)

res08 <- sum_rlt_2008$residuals
res09 <- sum_rlt_2009$residuals
res10 <- sum_rlt_2010$residuals
res11 <- sum_rlt_2011$residuals


me2008 <- Y2008-X2008%*%m_rlt_2008
me2009 <- Y2009-X2009%*%m_rlt_2009
me2010 <- Y2010-X2010%*%m_rlt_2010
me2011 <- Y2011-X2011%*%m_rlt_2011

df <- sum_rlt_2008$df[2]

s208 <- 1/df*t(me2008)%*%me2008
s209 <- 1/df*t(me2009)%*%me2009
s210 <- 1/df*t(me2010)%*%me2010
s211 <- 1/df*t(me2011)%*%me2011
sqrt(s209);sqrt(s210);sqrt(s211)


sigma08 <- sum_rlt_2008$sigma
sigma09 <- sum_rlt_2009$sigma
sigma10 <- sum_rlt_2010$sigma
sigma11 <- sum_rlt_2011$sigma

s208 <- as.numeric(s208)
s209 <- as.numeric(s209)
s210 <- as.numeric(s211)
s211 <- as.numeric(s211)

var_B2008 <- s208*solve(t(X2008)%*%X2008)
var_B2009 <- s209*solve(t(X2009)%*%X2009)
var_B2010 <- s210*solve(t(X2010)%*%X2010)
var_B2011 <- s211*solve(t(X2011)%*%X2011)

class(s208)
sqrt(2.082327e-04)
1.443027e-02

sum_rlt_2008$cov.unscaled

str(sum_rlt_2008)
sum_rlt_2008$coefficients

##############################new  part=============================
colnames(X2009)
x20081 <- X2008[,c(1,5,9:17)]
x20091 <- X2009[,c(2,6,9:17)]
x20101 <- X2010[,c(3,7,9:17)]
x20111 <- X2011[,c(4,8,9:17)]

x_sur <- cbind(x20081,x20091,x20101,x20111)
class(x_sur)
dim(x_sur)
x_sur <- as.data.frame(x_sur)
x_sur$cons <- 1
x_sur <- cbind(x_sur[,45],x_sur[,1:44])
x_sur <- as.matrix(x_sur)
colnames(x_sur)[1]<-'cons'
colnames(x_sur)

y_sur <- cbind(Y2008,Y2009,Y2010,Y2011)
dim(y_sur)

m_sur <- solve(
   t(x_sur)%*%x_sur,tol=1e-21)%*%t(x_sur)%*%y_sur
dim(m_sur)

?systemfit
colnames(data_sur)
data_sur <- cbind(y_sur,x_sur)
data_sur <- as.data.frame(data_sur)
eq08 <- y2008~op08+op208+op308+inp08+inp208+inp308+oxi08+
              wgix08+beds08+fix08+cmi08
eq09 <- y2009~op09+op209+op309+inp09+inp209+inp309+oxi09+
              wgix09+beds09+fix09+cmi09
eq10 <- y2010~op10+op210+op310+inp10+inp210+inp310+oxi10+
              wgix10+beds10+fix10+cmi10
eq11 <- y2011~op11+op211+op311+inp11+inp211+inp311+oxi11+
              wgix11+beds11+fix11+cmi11

eq08 <- y2008~op08+op09+op10+op11+inp08+inp09+inp10+inp11+
              op208+op308+inp208+inp308+oxi08+
              wgix08+beds08+fix08+cmi08
eq09 <- y2009~op08+op09+op10+op11+inp08+inp09+inp10+inp11+
              op209+op309+inp209+inp309+oxi09+
              wgix09+beds09+fix09+cmi09
eq10 <- y2010~op08+op09+op10+op11+inp08+inp09+inp10+inp11+
              op210+op310+inp210+inp310+oxi10+
              wgix10+beds10+fix10+cmi10
eq11 <- y2011~op08+op09+op10+op11+inp08+inp09+inp10+inp11+
              op211+op311+inp211+inp311+oxi11+
              wgix11+beds11+fix11+cmi11

system <- list(eq08=eq08,eq09=eq09,eq10=eq10,eq11=eq11)
rlt_sur <- systemfit(system,'SUR',data=data_sur)
rlt_ols <- systemfit(system,'OLS',data=data_sur)
summary(rlt_sur)
summary(rlt_ols)

str(rlt_sur)
sur08 <- rlt_sur$coefficients[c('eq08_inp08','eq08_inp09',
                     'eq08_inp10','eq08_inp11')]
sur09 <- rlt_sur$coefficients[c('eq09_inp08','eq09_inp09',
                       'eq09_inp10','eq09_inp11')]
sur10 <- rlt_sur$coefficients[c('eq10_inp08','eq10_inp09',
                       'eq10_inp10','eq10_inp11')]
sur11 <- rlt_sur$coefficients[c('eq11_inp08','eq11_inp09',
                       'eq11_inp10','eq11_inp11')]
inp_sur <- rbind(sur08,sur09,sur10,sur11)








