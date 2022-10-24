#MODELIZACION DEL MODELO GR PARA EL ESTUDIO DE LA "INNOVACIÓN ORGANIZACIONAL"

#%%%%%%%%%%%%%%%%%%%%%%%%%%% Actividades %%%%%%%%%%%%%%%%%%%%%%%%%%%
save.image(file =".RData")

#1)Previo al Análisis
   #a)Se lee la base de datos
      library(openxlsx)
      data<-read.xlsx("Data3_Items.xlsx")
      dim(data)
         #343 := filas que representan el número de organizaciones
         #9 := columnas que representan el número de ítems
      
   #b)Frecuencia de las respuestas para los ítems dentro de cada categoría 
      apply(data, 2, table)
      
   #c)Estadísticos descriptivos de las respuestas a los ítems
      summary(data)


#2)Análisis
   #a)Se instala Stan
      #install.packages("rstan")

   #b)Se llama a la libreria 'rstan'
      library(rstan)
      options(mc.cores = parallel::detectCores())
      rstan_options(auto_write = TRUE)

   #c)Se configura las dimensiones a usar en el modelo
      J=dim(data)[1] #numero de organizaciones
      I=dim(data)[2] #numero de items
      K=length(c(1,2,3,4))
      
   #d)Se crea una lista con la información
      data_irt=list(n_student=J,n_item=I,Y=data, K=K)
      
   #c)Modelización GRM con Stan

      stanmodelcode <- "
      data{
         int<lower=1,upper=4> K;
         int<lower=0> n_student;
         int<lower=0> n_item;
         int<lower=1,upper=K> Y[n_student,n_item];
      }
      
      parameters {
         vector[n_student] theta;
         real<lower=0> a [n_item];
         ordered[K-1] b[n_item];
      }
        
      model{
         a ~ normal(0,1);
         theta ~ normal(0,1);
         for (i in 1: n_item){
         for (k in 1:(K-1)){
         b[i,k] ~ normal(0,1);
         }}
         for (i in 1:n_student){
         for (j in 1:n_item){
         Y[i,j] ~ ordered_logistic(theta[i]*a[j],b[j]);
         }}
      }
      "

      GRM<- stan(model_code = stanmodelcode, data = data_irt, iter = 6000, chains = 3)
      
   #d)Distribuciones posteriores
      #print(GRM)
      print(GRM,par='a')
      print(GRM,par='b')
      print(GRM,par='theta')
      

   #e)Se extrae la información
      #INFORMACION
      E<-summary(GRM)
      D<-data.frame(E$summary)
      
      library(writexl)
      
      #HABILIDAD de Actividades (theta)
      vector0<-seq(1,343)  
      theta<-D[vector0[],c(1,2)]
      
      InfoTheta<-as.data.frame(D[vector0[],])
      write_xlsx (InfoTheta, 'theta.xlsx')

      
      #alpha
      vector1<-seq(344,352,1)
      alpha<-D[vector1[],c(1,2)]

      InfoAlpha<-as.data.frame(D[vector1[],])
      write_xlsx (InfoAlpha,'alpha.xlsx')
      
      
      #beta
      vector2<-seq(353,379,1)
      beta<-D[vector2[],c(1,2)]
      
      InfoBeta<-as.data.frame(D[vector2[],])
      write_xlsx (InfoBeta,'beta.xlsx')
      
      
      #BETA1
      vectorA<-seq(353,379,3)
      beta1<-D[vectorA[],c(1,2)]
      InfoBeta1<-as.data.frame(D[vectorA[],])
      write_xlsx (InfoBeta1,'beta1.xlsx')
      #BETA2
      vectorB<-seq(354,379,3)
      beta2<-D[vectorB[],c(1,2)]
      InfoBeta2<-as.data.frame(D[vectorB[],])
      write_xlsx (InfoBeta2,'beta2.xlsx')
      #BETA3
      vectorC<-seq(355,379,3)
      beta3<-D[vectorC[],c(1,2)]
      InfoBeta3<-as.data.frame(D[vectorC[],])
      write_xlsx (InfoBeta3,'beta3.xlsx')


#--------------- WHITE NOISE --------------
#traceplot(GRM, pars= 'alpha',inc_warmup = FALSE)
#traceplot(GRM, pars= 'beta',inc_warmup = FALSE)
#traceplot(GRM, pars= 'theta',inc_warmup = FALSE)

#ALPHA
png("WhiteALPHA.png", width=680, height=480)
traceplot(GRM, pars= 'a',inc_warmup = FALSE)
dev.off()


#BETA 
png("WhiteBETA.png", width=680, height=480)
traceplot(GRM, pars= 'b',inc_warmup = FALSE)
dev.off()


#THETA 
#png("WhiteTETHA.png", width=680, height=480)
#traceplot(GRM, pars= 'theta',inc_warmup = FALSE)
#dev.off()

#THETAS individuales
#theta 1
png("WhiteTHETA1FI.png", width=680, height=480)
traceplot(GRM,pars= 'theta[1]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()
#theta 25
png("WhiteTHETA25FI.png", width=680, height=480)
traceplot(GRM,pars= 'theta[25]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()
#theta 50
png("WhiteTHETA50FI.png", width=680, height=480)
traceplot(GRM,pars= 'theta[50]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()


#--------------- AUTOCORRELACIÓN --------------
#ALPHA
png("AutoALPHA.png", width=680, height=480)
stan_ac(GRM,
        pars= 'a',
        inc_warmup = FALSE,
        separate_chains=FALSE)
dev.off()

#BETHA
png("AutoBETHA.png", width=680, height=480)
stan_ac(GRM,
        pars= 'b',
        inc_warmup = FALSE,
        separate_chains=FALSE)
dev.off()

#TETHA
# png("AutoTETHA.png", width=680, height=480)
# stan_ac(GRM,
#         pars= 'theta',
#         inc_warmup = FALSE,
#         separate_chains=FALSE)
# dev.off()

#THETAS individuales
#theta 1
png("AutoTHETA1FI.png", width=680, height=480)
stan_ac(GRM,pars= 'theta[1]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()
#theta 25
png("AutoTHETA25FI.png", width=680, height=480)
stan_ac(GRM,pars= 'theta[25]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()
#theta 50
png("AutoTHETA50FI.png", width=680, height=480)
stan_ac(GRM,pars= 'theta[50]',inc_warmup = FALSE,separate_chains=FALSE)
dev.off()


#--------------- PROBABILIDADES -----------------
ttecho <- as.vector(theta[,1]) #thetha_mu
a<-as.vector(alpha[,1]) #a_mu
b1<-as.vector(beta1[,1]) #b1_mu
b2<-as.vector(beta2[,1]) #b2_mu
b3<-as.vector(beta3[,1]) #b3_mu

Informacion<-as.data.frame(ttecho)
write_xlsx (Informacion, 'Actividades.xlsx')

#Leemos la libreria "ggplot2" para los gráficos
library(ggplot2)

#--------------- CCCs C301-------------------
#PARA k=1 
k1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-')))) #k=1
   k1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC301k1<-as.numeric(k1)


#PARA k=2
k2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))-
      (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-')))) #k=2
   k2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC301k2<-as.numeric(k2)


#PARA k=3
k3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))-
      (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))) #k=3
   k3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC301k3<-as.numeric(k3)


#PARA k=4
k4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[1]*outer(i,b3[1], '-'))/(1+exp(a[1]*outer(i,b3[1], '-')))) #k=4
   k4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC301k4<-as.numeric(k4)

#Resultado de probabilidades
dataliC301<-data.frame(yC301k1,yC301k2,yC301k3,yC301k4,ttecho)
names(dataliC301)=c('PC301k1','PC301k2','PC301k3','PC301k4','Habilidad')
write_xlsx (dataliC301,'ProbabilidadesC301k.xlsx')
dataliC301

#Curvas Características
png("CCCs_ActividadesC301.png", width=680, height=480)
ggplot(data=dataliC301, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC301k1, colour = "k=1")) +
   geom_smooth(aes(y=PC301k2, colour = "k=2")) +
   geom_smooth(aes(y=PC301k3, colour = "k=3")) +
   geom_smooth(aes(y=PC301k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C301 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()



#--------------- CCCs C302-------------------
#PARA k=1 
L1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-')))) #k=1
   L1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC302k1<-as.numeric(L1)


#PARA k=2
L2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))-
      (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-')))) #k=2
   L2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC302k2<-as.numeric(L2)



#PARA k=3
L3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))-
      (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))) #k=3
   L3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC302k3<-as.numeric(L3)


#PARA k=4
L4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[2]*outer(i,b3[2], '-'))/(1+exp(a[2]*outer(i,b3[2], '-')))) #k=4
   L4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC302k4<-as.numeric(L4)

#Resultado de probabilidades
dataliC302<-data.frame(yC302k1,yC302k2,yC302k3,yC302k4,ttecho)
names(dataliC302)=c('PC302k1','PC302k2','PC302k3','PC302k4','Habilidad')
write_xlsx (dataliC302,'ProbabilidadesC302k.xlsx')
dataliC302


#Curvas Características
png("CCCs_ActividadesC302.png", width=680, height=480)
ggplot(data=dataliC302, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC302k1, colour = "k=1")) +
   geom_smooth(aes(y=PC302k2, colour = "k=2")) +
   geom_smooth(aes(y=PC302k3, colour = "k=3")) +
   geom_smooth(aes(y=PC302k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C302 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()



#--------------- CCCs C303-------------------
#PARA k=1 
M1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-')))) #k=1
   M1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC303k1<-as.numeric(M1)



#PARA k=2
M2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))-
      (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-')))) #k=2
   M2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC303k2<-as.numeric(M2)


#PARA k=3
M3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))-
      (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))) #k=3
   M3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC303k3<-as.numeric(M3)



#PARA k=4
M4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[3]*outer(i,b3[3], '-'))/(1+exp(a[3]*outer(i,b3[3], '-')))) #k=4
   M4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC303k4<-as.numeric(M4)

#Resultado de probabilidades
dataliC303<-data.frame(yC303k1,yC303k2,yC303k3,yC303k4,ttecho)
names(dataliC303)=c('PC303k1','PC303k2','PC303k3','PC303k4','Habilidad')
write_xlsx (dataliC303,'ProbabilidadesC303k.xlsx')
dataliC303

#Curvas Características
png("CCCs_ActividadesC303.png", width=680, height=480)
ggplot(data=dataliC303, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC303k1, colour = "k=1")) +
   geom_smooth(aes(y=PC303k2, colour = "k=2")) +
   geom_smooth(aes(y=PC303k3, colour = "k=3")) +
   geom_smooth(aes(y=PC303k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C303 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()



#--------------- CCCs C304-------------------
#PARA k=1 
N1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-')))) #k=1
   N1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC304k1<-as.numeric(N1)



#PARA k=2
N2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))-
      (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-')))) #k=2
   N2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC304k2<-as.numeric(N2)


#PARA k=3
N3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))-
      (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))) #k=3
   N3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC304k3<-as.numeric(N3)



#PARA k=4
N4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[4]*outer(i,b3[4], '-'))/(1+exp(a[4]*outer(i,b3[4], '-')))) #k=4
   N4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC304k4<-as.numeric(N4)


#Resultado de probabilidades
dataliC304<-data.frame(yC304k1,yC304k2,yC304k3,yC304k4,ttecho)
names(dataliC304)=c('PC304k1','PC304k2','PC304k3','PC304k4','Habilidad')
write_xlsx (dataliC304,'ProbabilidadesC304k.xlsx')
dataliC304

#Curvas Características
png("CCCs_ActividadesC304.png", width=680, height=480)
ggplot(data=dataliC304, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC304k1, colour = "k=1")) +
   geom_smooth(aes(y=PC304k2, colour = "k=2")) +
   geom_smooth(aes(y=PC304k3, colour = "k=3")) +
   geom_smooth(aes(y=PC304k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C304 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()


#--------------- CCCs C305-------------------
#PARA k=1 
O1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-')))) #k=1
   O1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC305k1<-as.numeric(O1)



#PARA k=2
O2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))-
      (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-')))) #k=2
   O2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC305k2<-as.numeric(O2)


#PARA k=3
O3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))-
      (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))) #k=3
   O3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC305k3<-as.numeric(O3)



#PARA k=4
O4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[5]*outer(i,b3[5], '-'))/(1+exp(a[5]*outer(i,b3[5], '-')))) #k=4
   O4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC305k4<-as.numeric(O4)


#Resultado de probabilidades
dataliC305<-data.frame(yC305k1,yC305k2,yC305k3,yC305k4,ttecho)
names(dataliC305)=c('PC305k1','PC305k2','PC305k3','PC305k4','Habilidad')
write_xlsx (dataliC305,'ProbabilidadesC305k.xlsx')
dataliC305

#Curvas Características
png("CCCs_ActividadesC305.png", width=680, height=480)
ggplot(data=dataliC305, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC305k1, colour = "k=1")) +
   geom_smooth(aes(y=PC305k2, colour = "k=2")) +
   geom_smooth(aes(y=PC305k3, colour = "k=3")) +
   geom_smooth(aes(y=PC305k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C305 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()

#--------------- CCCs C306-------------------
#PARA k=1 
Q1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-')))) #k=1
   Q1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC306k1<-as.numeric(Q1)



#PARA k=2
Q2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-'))))-
      (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-')))) #k=2
   Q2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC306k2<-as.numeric(Q2)


#PARA k=3
Q3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))-
      (exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-')))) #k=3
   Q3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC306k3<-as.numeric(Q3)



#PARA k=4
Q4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[6]*outer(i,b3[6], '-'))/(1+exp(a[6]*outer(i,b3[6], '-')))) #k=4
   Q4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC306k4<-as.numeric(Q4)

#Resultado de probabilidades
dataliC306<-data.frame(yC306k1,yC306k2,yC306k3,yC306k4,ttecho)
names(dataliC306)=c('PC306k1','PC306k2','PC306k3','PC306k4','Habilidad')
write_xlsx (dataliC306,'ProbabilidadesC306k.xlsx')
dataliC306

#Curvas Características
png("CCCs_ActividadesC306.png", width=680, height=480)
ggplot(data=dataliC306, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC306k1, colour = "k=1")) +
   geom_smooth(aes(y=PC306k2, colour = "k=2")) +
   geom_smooth(aes(y=PC306k3, colour = "k=3")) +
   geom_smooth(aes(y=PC306k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C306 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()

#--------------- CCCs C307-------------------
#PARA k=1 
R1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-')))) #k=1
   R1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC307k1<-as.numeric(R1)



#PARA k=2
R2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-'))))-
      (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-')))) #k=2
   R2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC307k2<-as.numeric(R2)


#PARA k=3
R3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))-
      (exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-')))) #k=3
   R3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC307k3<-as.numeric(R3)



#PARA k=4
R4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[7]*outer(i,b3[7], '-'))/(1+exp(a[7]*outer(i,b3[7], '-')))) #k=4
   R4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC307k4<-as.numeric(R4)

#Resultado de probabilidades
dataliC307<-data.frame(yC307k1,yC307k2,yC307k3,yC307k4,ttecho)
names(dataliC307)=c('PC307k1','PC307k2','PC307k3','PC307k4','Habilidad')
write_xlsx (dataliC307,'ProbabilidadesC307k.xlsx')
dataliC307

#Curvas Características
png("CCCs_ActividadesC307.png", width=680, height=480)
ggplot(data=dataliC307, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC307k1, colour = "k=1")) +
   geom_smooth(aes(y=PC307k2, colour = "k=2")) +
   geom_smooth(aes(y=PC307k3, colour = "k=3")) +
   geom_smooth(aes(y=PC307k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C307 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()

#--------------- CCCs C308-------------------
#PARA k=1 
U1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-')))) #k=1
   U1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC308k1<-as.numeric(U1)



#PARA k=2
U2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-'))))-
      (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-')))) #k=2
   U2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC308k2<-as.numeric(U2)


#PARA k=3
U3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))-
      (exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-')))) #k=3
   U3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC308k3<-as.numeric(U3)



#PARA k=4
U4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[8]*outer(i,b3[8], '-'))/(1+exp(a[8]*outer(i,b3[8], '-')))) #k=4
   U4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC308k4<-as.numeric(U4)

#Resultado de probabilidades
dataliC308<-data.frame(yC308k1,yC308k2,yC308k3,yC308k4,ttecho)
names(dataliC308)=c('PC308k1','PC308k2','PC308k3','PC308k4','Habilidad')
write_xlsx (dataliC308,'ProbabilidadesC308k.xlsx')
dataliC308

#Curvas Características
png("CCCs_ActividadesC308.png", width=680, height=480)
ggplot(data=dataliC308, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC308k1, colour = "k=1")) +
   geom_smooth(aes(y=PC308k2, colour = "k=2")) +
   geom_smooth(aes(y=PC308k3, colour = "k=3")) +
   geom_smooth(aes(y=PC308k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C308 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()

#--------------- CCCs C309-------------------

#PARA k=1 
V1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-')))) #k=1
   V1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC309k1<-as.numeric(V1)



#PARA k=2
V2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-'))))-
      (exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-')))) #k=2
   V2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC309k2<-as.numeric(V2)


#PARA k=3
V3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-'))))-
      (exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-')))) #k=3
   V3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC309k3<-as.numeric(V3)



#PARA k=4
V4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[9]*outer(i,b3[9], '-'))/(1+exp(a[9]*outer(i,b3[9], '-')))) #k=4
   V4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC309k4<-as.numeric(V4)

#Resultado de probabilidades
dataliC309<-data.frame(yC309k1,yC309k2,yC309k3,yC309k4,ttecho)
names(dataliC309)=c('PC309k1','PC309k2','PC309k3','PC309k4','Habilidad')
write_xlsx (dataliC309,'ProbabilidadesC309k.xlsx')
dataliC309

#Curvas Características
png("CCCs_ActividadesC309.png", width=680, height=480)
ggplot(data=dataliC309, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC309k1, colour = "k=1")) +
   geom_smooth(aes(y=PC309k2, colour = "k=2")) +
   geom_smooth(aes(y=PC309k3, colour = "k=3")) +
   geom_smooth(aes(y=PC309k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C309 de Actividades") +
   xlab("Habilidad") +
   ylab("Probabilidad para la categoría k") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("k=1" = "blue", 
                                  "k=2" = "magenta",
                                  "k=3" = "green",
                                  "k=4" = "orange"))
dev.off()



# ----------- Informacion ítem C301------------

X1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
      (exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))^2 * 
      (1-(exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-')))))
      )+(
         (
            ((exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))*
                (1-(exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-')))))-
             (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))*
                (1-(exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-')))))
             )^2  
         )/
         (
             (exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))-
             (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))  
         )
      )+(
         (
            ((exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))*
                (1-(exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-')))))-
                (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-'))))*
                (1-(exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))))
            )^2  
         )/
            (
               (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))-
               (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-'))))  
            )
      )+(
        (
          (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-'))))*
          (1-(exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))))
           
        )^2
        / ((exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))))
      )
   X1[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC301<-as.numeric(X1)


# ----------- Informacion ítem C302------------

X2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))^2 * 
            (1-(exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-')))))
      )+(
         (
            ((exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))*
                (1-(exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-')))))-
                (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))*
                (1-(exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-')))))
            )^2  
         )/
            (
               (exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))-
                  (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))  
            )
      )+(
         (
            ((exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))*
                (1-(exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-')))))-
                (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-'))))*
                (1-(exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))))
            )^2  
         )/
            (
               (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))-
                  (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-'))))  
            )
      )+(
         (
            (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-'))))*
               (1-(exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))))
            
         )^2
         / ((exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))))
      )
   X2[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC302<-as.numeric(X2)


# ----------- Informacion ítem C303------------

X3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))^2 * 
            (1-(exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-')))))
      )+(
         (
            ((exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))*
                (1-(exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-')))))-
                (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))*
                (1-(exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-')))))
            )^2  
         )/
            (
               (exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))-
                  (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))  
            )
      )+(
         (
            ((exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))*
                (1-(exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-')))))-
                (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-'))))*
                (1-(exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))))
            )^2  
         )/
            (
               (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))-
                  (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-'))))  
            )
      )+(
         (
            (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-'))))*
               (1-(exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))))
            
         )^2
         / ((exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))))
      )
   X3[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC303<-as.numeric(X3)

# ----------- Informacion ítem C304------------

X4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))^2 * 
            (1-(exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-')))))
      )+(
         (
            ((exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))*
                (1-(exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-')))))-
                (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))*
                (1-(exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-')))))
            )^2  
         )/
            (
               (exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))-
                  (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))  
            )
      )+(
         (
            ((exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))*
                (1-(exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-')))))-
                (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-'))))*
                (1-(exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))))
            )^2  
         )/
            (
               (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))-
                  (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-'))))  
            )
      )+(
         (
            (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-'))))*
               (1-(exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))))
            
         )^2
         / ((exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))))
      )
   X4[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC304<-as.numeric(X4)

# ----------- Informacion ítem C305------------

X5 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))^2 * 
            (1-(exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-')))))
      )+(
         (
            ((exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))*
                (1-(exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-')))))-
                (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))*
                (1-(exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-')))))
            )^2  
         )/
            (
               (exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))-
                  (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))  
            )
      )+(
         (
            ((exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))*
                (1-(exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-')))))-
                (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-'))))*
                (1-(exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))))
            )^2  
         )/
            (
               (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))-
                  (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-'))))  
            )
      )+(
         (
            (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-'))))*
               (1-(exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))))
            
         )^2
         / ((exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))))
      )
   X5[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC305<-as.numeric(X5)

# ----------- Informacion ítem C306------------

X6 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-'))))^2 * 
            (1-(exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-')))))
      )+(
         (
            ((exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-'))))*
                (1-(exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-')))))-
                (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))*
                (1-(exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-')))))
            )^2  
         )/
            (
               (exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-'))))-
                  (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))  
            )
      )+(
         (
            ((exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))*
                (1-(exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-')))))-
                (exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-'))))*
                (1-(exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-')))))
            )^2  
         )/
            (
               (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))-
                  (exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-'))))  
            )
      )+(
         (
            (exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-'))))*
               (1-(exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-')))))
            
         )^2
         / ((exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-')))))
      )
   X6[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC306<-as.numeric(X6)

# ----------- Informacion ítem C307------------

X7 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-'))))^2 * 
            (1-(exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-')))))
      )+(
         (
            ((exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-'))))*
                (1-(exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-')))))-
                (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))*
                (1-(exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-')))))
            )^2  
         )/
            (
               (exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-'))))-
                  (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))  
            )
      )+(
         (
            ((exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))*
                (1-(exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-')))))-
                (exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-'))))*
                (1-(exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-')))))
            )^2  
         )/
            (
               (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))-
                  (exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-'))))  
            )
      )+(
         (
            (exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-'))))*
               (1-(exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-')))))
            
         )^2
         / ((exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-')))))
      )
   X7[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC307<-as.numeric(X7)

# ----------- Informacion ítem C308------------

X8 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-'))))^2 * 
            (1-(exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-')))))
      )+(
         (
            ((exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-'))))*
                (1-(exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-')))))-
                (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))*
                (1-(exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-')))))
            )^2  
         )/
            (
               (exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-'))))-
                  (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))  
            )
      )+(
         (
            ((exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))*
                (1-(exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-')))))-
                (exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-'))))*
                (1-(exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-')))))
            )^2  
         )/
            (
               (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))-
                  (exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-'))))  
            )
      )+(
         (
            (exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-'))))*
               (1-(exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-')))))
            
         )^2
         / ((exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-')))))
      )
   X8[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC308<-as.numeric(X8)

# ----------- Informacion ítem C309------------

X9 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-
      (
         (exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-'))))^2 * 
            (1-(exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-')))))
      )+(
         (
            ((exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-'))))*
                (1-(exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-')))))-
                (exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-'))))*
                (1-(exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-')))))
            )^2  
         )/
            (
               (exp(a[9]*outer(i,b1[9],'-'))/(1+exp(a[9]*outer(i,b1[9],'-'))))-
                  (exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-'))))  
            )
      )+(
         (
            ((exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-'))))*
                (1-(exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-')))))-
                (exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-'))))*
                (1-(exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-')))))
            )^2  
         )/
            (
               (exp(a[9]*outer(i,b2[9],'-'))/(1+exp(a[9]*outer(i,b2[9],'-'))))-
                  (exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-'))))  
            )
      )+(
         (
            (exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-'))))*
               (1-(exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-')))))
            
         )^2
         / ((exp(a[9]*outer(i,b3[9],'-'))/(1+exp(a[9]*outer(i,b3[9],'-')))))
      )
   X9[[j]]<-mean(Sujeto)
   j <- j + 1
}
IC309<-as.numeric(X9)

Habilidad <- ttecho
dataIIC <- data.frame(IC301,IC302,IC303,IC304,
                      IC305,IC306,IC307,IC308,
                      IC309,Habilidad)
write_xlsx (dataIIC,'InformacionItemAct.xlsx')


library(ggplot2)
#Curvas Características
png("IICs_Actividades.png", width=680, height=480)
ggplot(data=dataIIC, aes(x = Habilidad)) +
   geom_smooth(aes(y=IC301, colour = "C301")) +
   geom_smooth(aes(y=IC302, colour = "C302")) +
   geom_smooth(aes(y=IC303, colour = "C303")) +
   geom_smooth(aes(y=IC304, colour = "C304")) +
   geom_smooth(aes(y=IC305, colour = "C305")) +
   geom_smooth(aes(y=IC306, colour = "C306")) +
   geom_smooth(aes(y=IC307, colour = "C307")) +
   geom_smooth(aes(y=IC308, colour = "C308")) +
   geom_smooth(aes(y=IC309, colour = "C309")) +
   ggtitle("IICs para Actividades") +
   xlab("Habilidad") +
   ylab("Información del Item") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("C301" = "blue", 
                                  "C302" = "magenta",
                                  "C303" = "green",
                                  "C304" = "orange",
                                  "C305" = "black", 
                                  "C306" = "red",
                                  "C307" = "cyan",
                                  "C308" = "yellow",
                                  "C309" = "violet"))
dev.off()

# ----------- Informacion del test -----------

suma_renglones <- function (x) {
  y=integer(nrow(x))
  for(i in 1:nrow(x)){
    for (j in 1:ncol(x)) {
      if(is.numeric(x[i,j])) 
      {
        y[i] = y[i]+(x[i,j])
      }
    }
  }
  x["Total"] <- y
  return(x)
}
TestInfo<-cbind(as.data.frame(IC301),
                as.data.frame(IC302),
                as.data.frame(IC303),
                as.data.frame(IC304),
                as.data.frame(IC305),
                as.data.frame(IC306),
                as.data.frame(IC307),
                as.data.frame(IC308),
                as.data.frame(IC309))
TestInfo = suma_renglones(TestInfo)
Valores <- TestInfo$Total

#plot(ttecho,Valores, col = "blue",type = 'p')

library(ggplot2)

datosAct <- data.frame(ttecho,Valores)

png("Informacion_Test_Actividades.png", width=680, height=480)
ggplot(datosAct,aes(ttecho,Valores)) +
  geom_line(colour = 'orange')  + 
  ggtitle("Información del test ''Actividades''") +
  xlab("Habilidad") +
  ylab("Información del test") +
  #geom_point( size=2, shape=21, fill="white") +
  theme_minimal()
dev.off()