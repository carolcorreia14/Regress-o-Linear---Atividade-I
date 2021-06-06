install.packages("readxl")

#importando os dados
library(readxl)
dados_atividade <- read_excel("C:/Users/macie/Downloads/dados_atividade.xlsx")
View(dados_atividade)
class(dados_atividade)
#transformando os dados em data frame
dados <- as.data.frame(dados_atividade)

dim(dados)
View(dados)

#Transformando as variáveis em numéricas

dados$`Peso(kg)`<-as.numeric(dados$`Peso(kg)`)
dados$`Altura(cm)`<-as.numeric(dados$`Altura(cm)`)
summary(dados) #saber a média, moda, mediana, max, min...

#desvio padrão
sd(dados$`Altura(cm)`)
sd(dados$`Peso(kg)`)
glimpse(dados)


#Construindo o modelo de regressão linear(PESO DEPENDE DA ALTURA?)
#peso = b0 + b1*altura

linearMod <- lm(`Peso(kg)`~ `Altura(cm)`, data=dados)  # build linear regression model on full data
print(linearMod) #o intercepto(beta0) e o beta 1
summary(linearMod)

# altura = -76.51 + 0.82*peso

Peso_kg <- dados$`Peso(kg)`
Altura_cm <- dados$`Altura(cm)`

#Gráfico de dispersão

install.packages("tidyverse")
library(tidyverse)
install.packages("ggpubr")
library(ggpubr)

#Plotar os dados
ggplot(dados,aes(x = Altura_cm, y = Peso_kg)) + geom_point()

modelo <- lm(data = dados, formula = Peso_kg ~ Altura_cm)
# o operador "~" representa "em função de"

#Visualização da reta obtida
ggplot(dados, aes(Altura_cm,Peso_kg)) + geom_point() + geom_smooth(method = lm, se = FALSE)

#Análise de resíduos

par(mfrow = c(2,2))

plot(modelo, which = c(1:4), pch = 20)

#Primeiro gráfico temos os resíduos em função dos valores estimados


#Como calcular a estatistica t e p-valores?
#Essa parte não precisa
modelSummary <- summary(linearMod)  

modelCoeffs <- modelSummary$coefficients  # model coefficients
beta.estimate <- modelCoeffs["Altura", "Estimate"]  # get beta estimate for speed
std.error <- modelCoeffs["Altura", "Std. Error"]  # get std.error for speed
t_value <- beta.estimate/std.error  # calc t statistic
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))  # calc p Value
f_statistic <- linearMod$fstatistic[1]  # fstatistic
f <- summary(linearMod)$fstatistic  # parameters for model p-value calc
model_p <- pf(f[1], f[2], f[3], lower=FALSE)


#Ver a correlação dos dados
cor(dados$`Altura(cm)`,dados$`Peso(kg)`)

summary(dados)





