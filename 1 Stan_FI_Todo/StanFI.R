#MODELIZACION DEL MODELO GR PARA EL ESTUDIO DE LA "INNOVACIÓN ORGANIZACIONAL"

#%%%%%%%%%%%%%%%%%%%%%%%%%%% Fuentes de Información %%%%%%%%%%%%%%%%%%%%%%%%%%%

#1)Previo al Análisis
   #a)Se lee la base de datos
      library(openxlsx)
      data<-read.xlsx("Data1_Items.xlsx")
      dim(data)
         #343 := filas que representan el número de organizaciones
         #8 := columnas que representan el número de ítems
      
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
      
      #HABILIDAD de Fuentes de Financiamiento (theta)
      vector0<-seq(1,343)  
      theta<-D[vector0[],c(1,2)]
      
      InfoTheta<-as.data.frame(D[vector0[],])
      write_xlsx (InfoTheta, 'theta.xlsx')

      
      #alpha
      vector1<-seq(344,351,1)
      alpha<-D[vector1[],c(1,2)]

      InfoAlpha<-as.data.frame(D[vector1[],])
      write_xlsx (InfoAlpha,'alpha.xlsx')
      
      
      #beta
      vector2<-seq(352,375,1)
      beta<-D[vector2[],c(1,2)]
      
      InfoBeta<-as.data.frame(D[vector2[],])
      write_xlsx (InfoBeta,'beta.xlsx')
      
      
      #BETA1
      vectorA<-seq(352,375,3)
      beta1<-D[vectorA[],c(1,2)]
      InfoBeta1<-as.data.frame(D[vectorA[],])
      write_xlsx (InfoBeta1,'beta1.xlsx')
      #BETA2
      vectorB<-seq(353,375,3)
      beta2<-D[vectorB[],c(1,2)]
      InfoBeta2<-as.data.frame(D[vectorB[],])
      write_xlsx (InfoBeta2,'beta2.xlsx')
      #BETA3
      vectorC<-seq(354,375,3)
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
write_xlsx (Informacion, 'FuentesInformacion.xlsx')

#Leemos la libreria "ggplot2" para los gráficos
library(ggplot2)

#--------------- CCCs C101-------------------
#PARA k=1 
k1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-')))) #k=1
   k1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC101k1<-as.numeric(k1)


#PARA k=2
k2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))-
      (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-')))) #k=2
   k2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC101k2<-as.numeric(k2)


#PARA k=3
k3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))-
      (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))) #k=3
   k3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC101k3<-as.numeric(k3)


#PARA k=4
k4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[1]*outer(i,b3[1], '-'))/(1+exp(a[1]*outer(i,b3[1], '-')))) #k=4
   k4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC101k4<-as.numeric(k4)

#Resultado de probabilidades
dataliC101<-data.frame(yC101k1,yC101k2,yC101k3,yC101k4,ttecho)
names(dataliC101)=c('PC101k1','PC101k2','PC101k3','PC101k4','Habilidad')
write_xlsx (dataliC101,'ProbabilidadesC101k.xlsx')
dataliC101

#Curvas Características
png("CCCs_FuentesInformacionC101.png", width=680, height=480)
ggplot(data=dataliC101, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC101k1, colour = "k=1")) +
   geom_smooth(aes(y=PC101k2, colour = "k=2")) +
   geom_smooth(aes(y=PC101k3, colour = "k=3")) +
   geom_smooth(aes(y=PC101k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C101 de Fuentes de Información") +
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



#--------------- CCCs C102-------------------
#PARA k=1 
L1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-')))) #k=1
   L1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC102k1<-as.numeric(L1)


#PARA k=2
L2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))-
      (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-')))) #k=2
   L2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC102k2<-as.numeric(L2)



#PARA k=3
L3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))-
      (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))) #k=3
   L3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC102k3<-as.numeric(L3)


#PARA k=4
L4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[2]*outer(i,b3[2], '-'))/(1+exp(a[2]*outer(i,b3[2], '-')))) #k=4
   L4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC102k4<-as.numeric(L4)

#Resultado de probabilidades
dataliC102<-data.frame(yC102k1,yC102k2,yC102k3,yC102k4,ttecho)
names(dataliC102)=c('PC102k1','PC102k2','PC102k3','PC102k4','Habilidad')
write_xlsx (dataliC102,'ProbabilidadesC102k.xlsx')
dataliC102


#Curvas Características
png("CCCs_FuentesInformacionC102.png", width=680, height=480)
ggplot(data=dataliC102, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC102k1, colour = "k=1")) +
   geom_smooth(aes(y=PC102k2, colour = "k=2")) +
   geom_smooth(aes(y=PC102k3, colour = "k=3")) +
   geom_smooth(aes(y=PC102k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C102 de Fuentes de Información") +
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



#--------------- CCCs C103-------------------
#PARA k=1 
M1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-')))) #k=1
   M1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC103k1<-as.numeric(M1)



#PARA k=2
M2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))-
      (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-')))) #k=2
   M2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC103k2<-as.numeric(M2)


#PARA k=3
M3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))-
      (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))) #k=3
   M3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC103k3<-as.numeric(M3)



#PARA k=4
M4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[3]*outer(i,b3[3], '-'))/(1+exp(a[3]*outer(i,b3[3], '-')))) #k=4
   M4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC103k4<-as.numeric(M4)

#Resultado de probabilidades
dataliC103<-data.frame(yC103k1,yC103k2,yC103k3,yC103k4,ttecho)
names(dataliC103)=c('PC103k1','PC103k2','PC103k3','PC103k4','Habilidad')
write_xlsx (dataliC103,'ProbabilidadesC103k.xlsx')
dataliC103

#Curvas Características
png("CCCs_FuentesInformacionC103.png", width=680, height=480)
ggplot(data=dataliC103, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC103k1, colour = "k=1")) +
   geom_smooth(aes(y=PC103k2, colour = "k=2")) +
   geom_smooth(aes(y=PC103k3, colour = "k=3")) +
   geom_smooth(aes(y=PC103k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C103 de Fuentes de Información") +
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



#--------------- CCCs C104-------------------
#PARA k=1 
N1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-')))) #k=1
   N1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC104k1<-as.numeric(N1)



#PARA k=2
N2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))-
      (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-')))) #k=2
   N2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC104k2<-as.numeric(N2)


#PARA k=3
N3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))-
      (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))) #k=3
   N3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC104k3<-as.numeric(N3)



#PARA k=4
N4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[4]*outer(i,b3[4], '-'))/(1+exp(a[4]*outer(i,b3[4], '-')))) #k=4
   N4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC104k4<-as.numeric(N4)


#Resultado de probabilidades
dataliC104<-data.frame(yC104k1,yC104k2,yC104k3,yC104k4,ttecho)
names(dataliC104)=c('PC104k1','PC104k2','PC104k3','PC104k4','Habilidad')
write_xlsx (dataliC104,'ProbabilidadesC104k.xlsx')
dataliC104

#Curvas Características
png("CCCs_FuentesInformacionC104.png", width=680, height=480)
ggplot(data=dataliC104, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC104k1, colour = "k=1")) +
   geom_smooth(aes(y=PC104k2, colour = "k=2")) +
   geom_smooth(aes(y=PC104k3, colour = "k=3")) +
   geom_smooth(aes(y=PC104k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C104 de Fuentes de Información") +
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


#--------------- CCCs C105-------------------
#PARA k=1 
O1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-')))) #k=1
   O1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC105k1<-as.numeric(O1)



#PARA k=2
O2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))-
      (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-')))) #k=2
   O2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC105k2<-as.numeric(O2)


#PARA k=3
O3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))-
      (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))) #k=3
   O3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC105k3<-as.numeric(O3)



#PARA k=4
O4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[5]*outer(i,b3[5], '-'))/(1+exp(a[5]*outer(i,b3[5], '-')))) #k=4
   O4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC105k4<-as.numeric(O4)


#Resultado de probabilidades
dataliC105<-data.frame(yC105k1,yC105k2,yC105k3,yC105k4,ttecho)
names(dataliC105)=c('PC105k1','PC105k2','PC105k3','PC105k4','Habilidad')
write_xlsx (dataliC105,'ProbabilidadesC105k.xlsx')
dataliC105

#Curvas Características
png("CCCs_FuentesInformacionC105.png", width=680, height=480)
ggplot(data=dataliC105, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC105k1, colour = "k=1")) +
   geom_smooth(aes(y=PC105k2, colour = "k=2")) +
   geom_smooth(aes(y=PC105k3, colour = "k=3")) +
   geom_smooth(aes(y=PC105k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C105 de Fuentes de Información") +
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

#--------------- CCCs C106-------------------
#PARA k=1 
Q1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-')))) #k=1
   Q1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC106k1<-as.numeric(Q1)



#PARA k=2
Q2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[6]*outer(i,b1[6],'-'))/(1+exp(a[6]*outer(i,b1[6],'-'))))-
      (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-')))) #k=2
   Q2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC106k2<-as.numeric(Q2)


#PARA k=3
Q3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[6]*outer(i,b2[6],'-'))/(1+exp(a[6]*outer(i,b2[6],'-'))))-
      (exp(a[6]*outer(i,b3[6],'-'))/(1+exp(a[6]*outer(i,b3[6],'-')))) #k=3
   Q3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC106k3<-as.numeric(Q3)



#PARA k=4
Q4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[6]*outer(i,b3[6], '-'))/(1+exp(a[6]*outer(i,b3[6], '-')))) #k=4
   Q4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC106k4<-as.numeric(Q4)

#Resultado de probabilidades
dataliC106<-data.frame(yC106k1,yC106k2,yC106k3,yC106k4,ttecho)
names(dataliC106)=c('PC106k1','PC106k2','PC106k3','PC106k4','Habilidad')
write_xlsx (dataliC106,'ProbabilidadesC106k.xlsx')
dataliC106

#Curvas Características
png("CCCs_FuentesInformacionC106.png", width=680, height=480)
ggplot(data=dataliC106, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC106k1, colour = "k=1")) +
   geom_smooth(aes(y=PC106k2, colour = "k=2")) +
   geom_smooth(aes(y=PC106k3, colour = "k=3")) +
   geom_smooth(aes(y=PC106k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C106 de Fuentes de Información") +
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

#--------------- CCCs C107-------------------
#PARA k=1 
R1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-')))) #k=1
   R1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC107k1<-as.numeric(R1)



#PARA k=2
R2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[7]*outer(i,b1[7],'-'))/(1+exp(a[7]*outer(i,b1[7],'-'))))-
      (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-')))) #k=2
   R2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC107k2<-as.numeric(R2)


#PARA k=3
R3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[7]*outer(i,b2[7],'-'))/(1+exp(a[7]*outer(i,b2[7],'-'))))-
      (exp(a[7]*outer(i,b3[7],'-'))/(1+exp(a[7]*outer(i,b3[7],'-')))) #k=3
   R3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC107k3<-as.numeric(R3)



#PARA k=4
R4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[7]*outer(i,b3[7], '-'))/(1+exp(a[7]*outer(i,b3[7], '-')))) #k=4
   R4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC107k4<-as.numeric(R4)

#Resultado de probabilidades
dataliC107<-data.frame(yC107k1,yC107k2,yC107k3,yC107k4,ttecho)
names(dataliC107)=c('PC107k1','PC107k2','PC107k3','PC107k4','Habilidad')
write_xlsx (dataliC107,'ProbabilidadesC107k.xlsx')
dataliC107

#Curvas Características
png("CCCs_FuentesInformacionC107.png", width=680, height=480)
ggplot(data=dataliC107, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC107k1, colour = "k=1")) +
   geom_smooth(aes(y=PC107k2, colour = "k=2")) +
   geom_smooth(aes(y=PC107k3, colour = "k=3")) +
   geom_smooth(aes(y=PC107k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C107 de Fuentes de Información") +
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

#--------------- CCCs C108-------------------
#PARA k=1 
U1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-')))) #k=1
   U1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC108k1<-as.numeric(U1)



#PARA k=2
U2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[8]*outer(i,b1[8],'-'))/(1+exp(a[8]*outer(i,b1[8],'-'))))-
      (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-')))) #k=2
   U2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC108k2<-as.numeric(U2)


#PARA k=3
U3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[8]*outer(i,b2[8],'-'))/(1+exp(a[8]*outer(i,b2[8],'-'))))-
      (exp(a[8]*outer(i,b3[8],'-'))/(1+exp(a[8]*outer(i,b3[8],'-')))) #k=3
   U3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC108k3<-as.numeric(U3)



#PARA k=4
U4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[8]*outer(i,b3[8], '-'))/(1+exp(a[8]*outer(i,b3[8], '-')))) #k=4
   U4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC108k4<-as.numeric(U4)

#Resultado de probabilidades
dataliC108<-data.frame(yC108k1,yC108k2,yC108k3,yC108k4,ttecho)
names(dataliC108)=c('PC108k1','PC108k2','PC108k3','PC108k4','Habilidad')
write_xlsx (dataliC108,'ProbabilidadesC108k.xlsx')
dataliC108

#Curvas Características
png("CCCs_FuentesInformacionC108.png", width=680, height=480)
ggplot(data=dataliC108, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC108k1, colour = "k=1")) +
   geom_smooth(aes(y=PC108k2, colour = "k=2")) +
   geom_smooth(aes(y=PC108k3, colour = "k=3")) +
   geom_smooth(aes(y=PC108k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C108 de Fuentes de Información") +
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


# ----------- Información ítem C101------------

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
IC101<-as.numeric(X1)


# ----------- Informacion ítem C102------------

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
IC102<-as.numeric(X2)


# ----------- Informacion ítem C103------------

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
IC103<-as.numeric(X3)

# ----------- Informacion ítem C104------------

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
IC104<-as.numeric(X4)

# ----------- Informacion ítem C105------------

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
IC105<-as.numeric(X5)

# ----------- Informacion ítem C106------------

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
IC106<-as.numeric(X6)

# ----------- Informacion ítem C107------------

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
IC107<-as.numeric(X7)

# ----------- Informacion ítem C108------------

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
IC108<-as.numeric(X8)

Habilidad <- ttecho
dataIIC <- data.frame(IC101,IC102,IC103,IC104,
                      IC105,IC106,IC107,IC108,
                      Habilidad)
write_xlsx (dataIIC,'InformacionItemFI.xlsx')


library(ggplot2)
#Curvas Características
png("IICs_Fuentes_Informacion.png", width=680, height=480)
ggplot(data=dataIIC, aes(x = Habilidad)) +
   geom_smooth(aes(y=IC101, colour = "C101")) +
   geom_smooth(aes(y=IC102, colour = "C102")) +
   geom_smooth(aes(y=IC103, colour = "C103")) +
   geom_smooth(aes(y=IC104, colour = "C104")) +
   geom_smooth(aes(y=IC105, colour = "C105")) +
   geom_smooth(aes(y=IC106, colour = "C106")) +
   geom_smooth(aes(y=IC107, colour = "C107")) +
   geom_smooth(aes(y=IC108, colour = "C108")) +
   ggtitle("IICs para Fuentes de Información") +
   xlab("Habilidad") +
   ylab("Información del Item") +
   theme(
      legend.position = c(1,1),
      legend.justification = c("right", "top")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("C101" = "blue", 
                                  "C102" = "magenta",
                                  "C103" = "green",
                                  "C104" = "orange",
                                  "C105" = "black", 
                                  "C106" = "red",
                                  "C107" = "cyan",
                                  "C108" = "yellow"))
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
TestInfo<-cbind(as.data.frame(IC101),
                as.data.frame(IC102),
                as.data.frame(IC103),
                as.data.frame(IC104),
                as.data.frame(IC105),
                as.data.frame(IC106),
                as.data.frame(IC107),
                as.data.frame(IC108))
TestInfo = suma_renglones(TestInfo)
Valores <- TestInfo$Total

#plot(ttecho,Valores, col = "blue",type = 'p')

library(ggplot2)

datosFI <- data.frame(ttecho,Valores)

png("Informacion_Test_Fuentes_Informacion.png", width=680, height=480)
ggplot(datosFI,aes(ttecho,Valores)) +
  geom_line(colour = 'blue')  + 
  ggtitle("Información del test ''Fuentes de Información''") +
  xlab("Habilidad") +
  ylab("Información del test") +
  #geom_point( size=2, shape=21, fill="white") +
  theme_minimal()
dev.off()
