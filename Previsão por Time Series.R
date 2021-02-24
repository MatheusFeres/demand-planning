# Código por Matheus Feres #

library(readxl)
library(tseries)
library(forecast)
library(prophet)
library(ggplot2)
library(stringr)

options(warn = 0)

####Setup#####

#Identificação de parâmetros


#Abrir arquivo
caminho <- "C:/Users/Matheus/Desktop/"
nomearq <- "hist_data.xlsx"
PastPerformance <- read_excel(str_c(caminho,nomearq))


#Determinação de parâmetros
SKU <- 0000000001
past_test <- 18
proj<- 6   #Tamanho do Ts_Teste
ForecastLag <- 12   #quantidade de pontos que desejamos projetar com o melhor modelo
data_inicial <- c(2019,01)
data_final <- c(2020,09)
data_testeF <- c(2020,03)
data_avaI <- c(2020,04)

##### Início do Programa ####

n_colunas <- ncol(PastPerformance)
n_linhas <- nrow(PastPerformance)
vetor_prod <-  (PastPerformance[,1])
n <- which(vetor_prod==as.character(SKU), arr.ind = T)[1]

#Projeções

PastPerformance <- t(PastPerformance)

#Informação para modelagem

n_past_p <- as.numeric(PastPerformance[2:n_colunas,n])

TS_PastPerformance <- ts(n_past_p,data_inicial, data_final ,frequency = 12)

TS_Model<-window(tsclean(TS_PastPerformance,replace.missing = T, lambda = "auto"),start = data_inicial, end = data_testeF)

TS_TEST<-window(tsclean(TS_PastPerformance),start = data_avaI)


make_prophet_A <- function(serie, horizon) {
df <- data.frame(as.numeric(serie))
dates_pr <- seq(as.Date(paste0(data_inicial[1],"-",data_inicial[2],"-01")),as.Date(paste0(data_testeF[1],"-",data_testeF[2],"-01")) ,by="months")
df[,2] <- dates_pr
colnames(df) <- c("y","ds")
m <- prophet(df, seasonality.mode = 'additive')
future <- make_future_dataframe(m, periods = horizon, freq = 'month')
Model_Prophet_A <- predict(m, future)
Model_Prophet_A <- ts(as.numeric(Model_Prophet_A$yhat[1:horizon]),data_avaI, data_final ,frequency = 12)
}

make_prophet_M <- function(serie, horizon) {
  df <- data.frame(as.numeric(serie))
  dates_pr <- seq(as.Date(paste0(data_inicial[1],"-",data_inicial[2],"-01")),as.Date(paste0(data_testeF[1],"-",data_testeF[2],"-01")) ,by="months")
  df[,2] <- dates_pr
  colnames(df) <- c("y","ds")
  m <- prophet(df, seasonality.mode = 'multiplicative')
  future <- make_future_dataframe(m, periods = horizon, freq = 'month')
  Model_Prophet_M <- predict(m, future)
  Model_Prophet_M <- ts(as.numeric(Model_Prophet_M$yhat[1:horizon]),data_avaI, data_final ,frequency = 12)
}

#############   
####### Treino #######  
#############

try(Model_NaiveF<-forecast(naive(TS_Model),h = proj),silent = T)

try(Model_MeanF<-forecast(meanf(TS_Model),h = proj),silent = T)

try(Model_DriftF<-forecast(rwf(TS_Model,drift = TRUE), h =proj),silent = T)

try(Model_DriftFalseF<-forecast(rwf(TS_Model,drift = FALSE), h =proj),silent = T)

try(Model_ETSF<- forecast(ets(TS_Model),h=proj),silent = T)

try(Model_ETSDAMPEDADDF<- forecast(ets(TS_Model,damped = TRUE,
                                       additive.only = TRUE),h=proj),silent = T)

try(Model_ETSNOTDAMPEDADDF<- forecast(ets(TS_Model,damped = FALSE,
                                          additive.only = TRUE),h=proj),silent = T)

try(Model_ETSDAMPEDMULTF<- forecast( ets(TS_Model,damped = TRUE,
                                         additive.only = FALSE),h=proj),silent = T)

try(Model_ETSNOTDAMPEDMULTF<- forecast(ets(TS_Model,damped = FALSE,
                                           additive.only = FALSE),h=proj),silent = T)

try(Model_HWAF<-forecast(HoltWinters(TS_Model,seasonal = "additive"), h =proj),silent = T)

try(Model_HWMF<- forecast(HoltWinters(TS_Model,seasonal = "multiplicative"), h =proj),silent = T)

try(Model_ARIMAF<-forecast(auto.arima(TS_Model), h = proj),silent = T)

try(Model_ARIMA2 <- Myarima(n),silent = T )

try(Model_TbatsF <- forecast(tbats(TS_Model), h = proj),silent = T)

try(Model_RWF<- forecast(rwf(TS_Model, h = proj), silent = T))

try(Model_SNAIVE<- forecast(snaive(TS_Model, h = proj)), silent = T)

try(Model_SES <- forecast(ses(TS_Model, h = proj)),silent = T)

try(Model_HOLT <- forecast(holt(TS_Model, h = proj)), silent = T)

try(Model_SPLINE <- forecast(splinef(TS_Model, h = proj)), silent = T)

try(Model_THETA <- forecast(thetaf(TS_Model, h = proj)), silent = T)

try(Model_CROSTON <- forecast(croston(TS_Model, h = proj)), silent = T)

try(Model_prophet_A <- make_prophet_A(TS_Model,proj),silent = T)

try(Model_prophet_M <- make_prophet_M(TS_Model,proj),silent = T)


#Teste de acuracidade


ModelosDispon�veis <-c("Naive","Mean","Drift","DriftFalse","ETS",
                       "ETSDampAdd","ETSNotDampAdd","ETSDampMult","ETSNotDampMult",
                       "HoltWinters_Additive","HoltWinters_Multiplicative", "ARIMA",
                       "ARIMA2", "TBATS", "RWF", "SES", "HOLT", "SPLINE", "THETA", "CROSTON", "ADDITVE_S_PROPHET","MULTIPLICATIVE_S_PROPHET")

ACC <- matrix(ncol=5,nrow=22)
ACC <- as.data.frame(ACC)
colnames(ACC)<- c("ME","RMSE","MAE","MPE","MAPE")
rownames(ACC)<- ModelosDispon�veis

ACC[1,]<- accuracy(Model_NaiveF,TS_TEST)[2,1:5]
ACC[2,]<- accuracy(Model_MeanF,TS_TEST)[2,1:5]
try(ACC[3,]<- accuracy(Model_DriftF,TS_TEST)[2,1:5], silent = T)
try(ACC[4,]<- accuracy(Model_DriftFalseF,TS_TEST)[2,1:5], silent = T)
try(ACC[5,]<- accuracy(Model_ETSF,TS_TEST)[2,1:5], silent = T)
try(ACC[6,]<- accuracy(Model_ETSDAMPEDADDF,TS_TEST)[2,1:5], silent = T)
try(ACC[7,]<- accuracy(Model_ETSNOTDAMPEDADDF,TS_TEST)[2,1:5], silent = T)
try(ACC[8,]<- accuracy(Model_ETSDAMPEDMULTF,TS_TEST)[2,1:5], silent = T)
try(ACC[9,]<- accuracy(Model_ETSNOTDAMPEDMULTF,TS_TEST)[2,1:5], silent = T)
try(ACC[10,]<- accuracy(Model_HWAF,TS_TEST)[2,1:5], silent = T)
try(ACC[11,]<- accuracy(Model_HWMF,TS_TEST)[2,1:5], silent = T)
try(ACC[12,]<- accuracy(Model_ARIMAF,TS_TEST)[2,1:5], silent = T)
try(ACC[13,]<- accuracy(Model_ARIMA2,TS_TEST)[2,1:5], silent = T)
try(ACC[14,]<- accuracy(Model_TbatsF,TS_TEST)[2,1:5], silent = T)
try(ACC[15,]<- accuracy(Model_RWF, TS_TEST)[2,1:5], silent = T)
try(ACC[16,]<- accuracy(Model_SES, TS_TEST)[2,1:5], silent = T)
try(ACC[17,]<- accuracy(Model_HOLT, TS_TEST)[2,1:5], silent = T)
try(ACC[18,]<- accuracy(Model_SPLINE, TS_TEST)[2,1:5], silent = T)
try(ACC[19,]<- accuracy(Model_THETA, TS_TEST)[2,1:5], silent = T)
try(ACC[20,]<- accuracy(Model_CROSTON, TS_TEST)[2,1:5], silent = T)
try(ACC[21,]<- accuracy(Model_prophet_A, TS_TEST)[1,1:5], silent = T)
try(ACC[22,]<- accuracy(Model_prophet_M, TS_TEST)[1,1:5], silent = T)

print(ACC[])

#Levantar qual o melhor modelo para modelar o teste
NomeModelo <- row.names(ACC)[(which.min(ACC$MAPE))]
NomeModelo

###########################
###########Forecast###########
##########################

#DS total
TS_Model<-window(tsclean(TS_PastPerformance),start = data_inicial, end = data_final)

if (sum(TS_Model) == 0) {
  (NomeModelo <- "Mean")
}

###Modelo Naive 1####
if (NomeModelo == "Naive") {
  #Model_Naive<- naive(TS_Model) 
  #Model_NaiveF<-forecast(Model_Naive,h = ForecastLag)  
  FUTURO<-forecast(naive(TS_Model,h =ForecastLag ))
} 
###Mean 2####
if (NomeModelo == "Mean") {
  #Model_Mean <- meanf(TS_Model)
  #Model_MeanF<-forecast(Model_Mean,h = ForecastLag)
  FUTURO<-forecast(meanf(TS_Model,h = ForecastLag))
}

###Drift 3####
if (NomeModelo == "Drift") {
  #Model_Drift <- rwf(TS_Model,drift = TRUE)
  #Model_DriftF<-forecast(Model_Drift, h =ForecastLag)
  FUTURO<-forecast(rwf(TS_Model,drift = TRUE,h =ForecastLag ))
  
}

###DriftFalse 4####
if (NomeModelo == "DriftFalse") {
  #Model_DriftFalse <- rwf(TS_Model,drift = FALSE)
  #Model_DriftFalseF<-forecast(Model_DriftFalse, h =ForecastLag)   
  FUTURO<-forecast(rwf(TS_Model,drift = FALSE, 
                       h =ForecastLag)) 
}

###ETS 5####
if (NomeModelo == "ETS") {
  #Model_ETS <- ets(TS_Model)
  #Model_ETSF<- forecast(Model_ETS,h=ForecastLag)
  FUTURO<- forecast(ets(TS_Model),
                    h=ForecastLag)
}

###ETS damped add 6####
if (NomeModelo == "ETSDampAdd") {
  #Model_ETSDAMPEDADD <- ets(TS_Model,damped = TRUE,additive.only = TRUE)
  #Model_ETSDAMPEDADDF<- forecast(Model_ETSDAMPEDADD,h=ForecastLag)
  FUTURO<- forecast(ets(TS_Model,damped = TRUE,
                        additive.only = TRUE),h=ForecastLag)
  
}

###ETS not damped add 7####
if (NomeModelo == "ETSNotDampAdd") {
  #Model_ETSNOTDAMPEDADD <- ets(TS_Model,damped = FALSE,additive.only = TRUE)
  #Model_ETSNOTDAMPEDADDF<- forecast(Model_ETSNOTDAMPEDADD,h=ForecastLag)
  FUTURO<- forecast(ets(TS_Model,damped = FALSE,
                        additive.only = TRUE),h=ForecastLag)
}

###ETS damped multi 8####
if (NomeModelo == "ETSDampMult") {
  #Model_ETSDAMPEDMULT <- ets(TS_Model,damped = TRUE,additive.only = FALSE)
  #Model_ETSDAMPEDMULTF<- forecast(Model_ETSDAMPEDMULT,h=ForecastLag)  
  FUTURO<- forecast(ets(TS_Model,damped = TRUE,
                        additive.only = FALSE),h=ForecastLag)  
}

###ETS not damped multi 9####
if (NomeModelo == "ETSNotDampMult") {
  #Model_ETSNOTDAMPEDMULT <- ets(TS_Model,damped = FALSE,additive.only = FALSE)
  #Model_ETSNOTDAMPEDMULTF<- forecast(Model_ETSNOTDAMPEDMULT,h=ForecastLag)  
  FUTURO<- forecast(ets(TS_Model,damped = FALSE,
                        additive.only = FALSE),h=ForecastLag)  
}

###Holtwinters Additive 10####
if (NomeModelo == "HoltWinters_Additive") {
  #Model_HW<-HoltWinters(TS_Model)
  #Model_HWF<-forecast(Model_HW, h =ForecastLag)
  FUTURO<-forecast(HoltWinters(TS_Model,seasonal = "additive"), h =ForecastLag)  
}

###Holtwinters Multiplicatvie 11####
if (NomeModelo == "HoltWinters_Multiplicative") {
  #Model_HW<-HoltWinters(TS_Model)
  #Model_HWF<-forecast(Model_HW, h =ForecastLag)
  TS_Model[TS_Model == 0] = 1e-20
  FUTURO<-forecast(HoltWinters(TS_Model, seasonal = "multiplicative"), h =ForecastLag)
}

###ARIMA 12####
if (NomeModelo == "ARIMA") {
  #Model_ARIMA <- auto.arima(TS_Model)
  #Model_ARIMAF<-forecast(Model_ARIMA, h = ForecastLag)
  FUTURO<-forecast(auto.arima(TS_Model), h = ForecastLag)
}

Myarima <- function(i)	{
  pcf_av <- (pacf(TS_Model))
  position <- which.max(abs(pcf_av$acf))
  sera <- forecast(Arima(TS_Model,order = c(position,1,12)),h = ForecastLag)
  sera
}

###AARIMA2 13####
if (NomeModelo == "ARIMA2") {
  #Model_ARIMA <- auto.arima(TS_Model)
  #Model_ARIMAF<-forecast(Model_ARIMA, h = ForecastLag)
  TS_Model[TS_Model < 0] = 1e-20
  FUTURO<- (Myarima(n))
}

###bats 14####
if (NomeModelo == "TBATS") {
  #Model_Tbats <-tbats(TS_Model)
  #Model_TbatsF <- forecast(Model_Tbats, h = ForecastLag)
  FUTURO <- forecast(Model_Tbats <-tbats(TS_Model), h = ForecastLag) 
}

###RWF 15###
if (NomeModelo == "RWF"){
  FUTURO <- forecast(Model_RWF<- rwf(TS_Model, h = ForecastLag))
}

###SES 16###
if (NomeModelo == "SES"){
  FUTURO <- forecast(Model_SES<- ses(TS_Model,h = ForecastLag))
}

###HOLT 17###
if (NomeModelo == "HOLT"){
  FUTURO <- forecast(Model_HOLT<- holt(TS_Model,h = ForecastLag))
}

###SPLINE 18###
if (NomeModelo == "SPLINE"){
  FUTURO <- forecast(Model_SPLINE<- splinef(tsclean(TS_Model),h = ForecastLag))
}

###THETA 19###
if (NomeModelo == "THETA"){
  FUTURO <- forecast(Model_THETA<- thetaf(TS_Model,h = ForecastLag))
}

###CROSTON 20###
if (NomeModelo == "CROSTON"){
  TS_Model[TS_Model < 0] = 1e-20
  FUTURO <- forecast(Model_CROSTON<- croston(TS_Model,h = ForecastLag))
}

###ADDITIVE PROPHET 21###
if (NomeModelo == "ADDITVE_S_PROPHET"){
  FUTURO <- forecast(Model_THETA<- thetaf(TS_Model,h = ForecastLag))
}

###MULTIPLICATIVE PROPHET 22###
if (NomeModelo == "MULTIPLICATIVE_S_PROPHET"){
  FUTURO <- forecast(Model_THETA<- thetaf(TS_Model,h = ForecastLag))
}

plot(FUTURO)
FUTURO$mean