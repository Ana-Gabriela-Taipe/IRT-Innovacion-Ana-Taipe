#MODELIZACION DEL MODELO GR PARA EL ESTUDIO DE LA "INNOVACIÓN ORGANIZACIONAL"

#%%%%%%%%%%%%%%%%%%%%%%%%%%% Fuentes de Financiamiento %%%%%%%%%%%%%%%%%%%%%%%%%%%
save.image(file =".RData" )

#1)Previo al Análisis
   #a)Se lee la base de datos
      library(openxlsx)
      data<-read.xlsx("Data2_Items.xlsx")
      dim(data)
         #343 := filas que representan el número de organizaciones
         #5 := columnas que representan el número de ítems
      
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
      vector1<-seq(344,348,1)
      alpha<-D[vector1[],c(1,2)]

      InfoAlpha<-as.data.frame(D[vector1[],])
      write_xlsx (InfoAlpha,'alpha.xlsx')
      
      
      #beta
      vector2<-seq(349,363,1)
      beta<-D[vector2[],c(1,2)]
      
      InfoBeta<-as.data.frame(D[vector2[],])
      write_xlsx (InfoBeta,'beta.xlsx')
      
      
      #BETA1
      vectorA<-seq(349,363,3)
      beta1<-D[vectorA[],c(1,2)]
      InfoBeta1<-as.data.frame(D[vectorA[],])
      write_xlsx (InfoBeta1,'beta1.xlsx')
      #BETA2
      vectorB<-seq(350,363,3)
      beta2<-D[vectorB[],c(1,2)]
      InfoBeta2<-as.data.frame(D[vectorB[],])
      write_xlsx (InfoBeta2,'beta2.xlsx')
      #BETA3
      vectorC<-seq(351,363,3)
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
write_xlsx (Informacion, 'FuentesFinanciamiento.xlsx')

#Leemos la libreria "ggplot2" para los gráficos
library(ggplot2)

#--------------- CCCs C201-------------------
#PARA k=1 
k1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-')))) #k=1
   k1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC201k1<-as.numeric(k1)


#PARA k=2
k2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b1[1],'-'))/(1+exp(a[1]*outer(i,b1[1],'-'))))-
      (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-')))) #k=2
   k2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC201k2<-as.numeric(k2)


#PARA k=3
k3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[1]*outer(i,b2[1],'-'))/(1+exp(a[1]*outer(i,b2[1],'-'))))-
      (exp(a[1]*outer(i,b3[1],'-'))/(1+exp(a[1]*outer(i,b3[1],'-')))) #k=3
   k3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC201k3<-as.numeric(k3)


#PARA k=4
k4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[1]*outer(i,b3[1], '-'))/(1+exp(a[1]*outer(i,b3[1], '-')))) #k=4
   k4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC201k4<-as.numeric(k4)

#Resultado de probabilidades
dataliC201<-data.frame(yC201k1,yC201k2,yC201k3,yC201k4,ttecho)
names(dataliC201)=c('PC201k1','PC201k2','PC201k3','PC201k4','Habilidad')
write_xlsx (dataliC201,'ProbabilidadesC201k.xlsx')
dataliC201

#Curvas Características
png("CCCs_FuentesFinanciamientoC201.png", width=680, height=480)
ggplot(data=dataliC201, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC201k1, colour = "k=1")) +
   geom_smooth(aes(y=PC201k2, colour = "k=2")) +
   geom_smooth(aes(y=PC201k3, colour = "k=3")) +
   geom_smooth(aes(y=PC201k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C201 de Fuentes de Financiamiento") +
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



#--------------- CCCs C202-------------------
#PARA k=1 
L1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-')))) #k=1
   L1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC202k1<-as.numeric(L1)


#PARA k=2
L2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b1[2],'-'))/(1+exp(a[2]*outer(i,b1[2],'-'))))-
      (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-')))) #k=2
   L2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC202k2<-as.numeric(L2)



#PARA k=3
L3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[2]*outer(i,b2[2],'-'))/(1+exp(a[2]*outer(i,b2[2],'-'))))-
      (exp(a[2]*outer(i,b3[2],'-'))/(1+exp(a[2]*outer(i,b3[2],'-')))) #k=3
   L3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC202k3<-as.numeric(L3)


#PARA k=4
L4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[2]*outer(i,b3[2], '-'))/(1+exp(a[2]*outer(i,b3[2], '-')))) #k=4
   L4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC202k4<-as.numeric(L4)

#Resultado de probabilidades
dataliC202<-data.frame(yC202k1,yC202k2,yC202k3,yC202k4,ttecho)
names(dataliC202)=c('PC202k1','PC202k2','PC202k3','PC202k4','Habilidad')
write_xlsx (dataliC202,'ProbabilidadesC202k.xlsx')
dataliC202


#Curvas Características
png("CCCs_FuentesFinanciamientoC202.png", width=680, height=480)
ggplot(data=dataliC202, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC202k1, colour = "k=1")) +
   geom_smooth(aes(y=PC202k2, colour = "k=2")) +
   geom_smooth(aes(y=PC202k3, colour = "k=3")) +
   geom_smooth(aes(y=PC202k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C202 de Fuentes de Financiamiento") +
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



#--------------- CCCs C203-------------------
#PARA k=1 
M1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-')))) #k=1
   M1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC203k1<-as.numeric(M1)



#PARA k=2
M2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b1[3],'-'))/(1+exp(a[3]*outer(i,b1[3],'-'))))-
      (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-')))) #k=2
   M2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC203k2<-as.numeric(M2)


#PARA k=3
M3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[3]*outer(i,b2[3],'-'))/(1+exp(a[3]*outer(i,b2[3],'-'))))-
      (exp(a[3]*outer(i,b3[3],'-'))/(1+exp(a[3]*outer(i,b3[3],'-')))) #k=3
   M3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC203k3<-as.numeric(M3)



#PARA k=4
M4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[3]*outer(i,b3[3], '-'))/(1+exp(a[3]*outer(i,b3[3], '-')))) #k=4
   M4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC203k4<-as.numeric(M4)

#Resultado de probabilidades
dataliC203<-data.frame(yC203k1,yC203k2,yC203k3,yC203k4,ttecho)
names(dataliC203)=c('PC203k1','PC203k2','PC203k3','PC203k4','Habilidad')
write_xlsx (dataliC203,'ProbabilidadesC203k.xlsx')
dataliC203

#Curvas Características
png("CCCs_FuentesFinanciamientoC203.png", width=680, height=480)
ggplot(data=dataliC203, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC203k1, colour = "k=1")) +
   geom_smooth(aes(y=PC203k2, colour = "k=2")) +
   geom_smooth(aes(y=PC203k3, colour = "k=3")) +
   geom_smooth(aes(y=PC203k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C203 de Fuentes de Financiamiento") +
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



#--------------- CCCs C204-------------------
#PARA k=1 
N1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-')))) #k=1
   N1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC204k1<-as.numeric(N1)



#PARA k=2
N2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b1[4],'-'))/(1+exp(a[4]*outer(i,b1[4],'-'))))-
      (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-')))) #k=2
   N2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC204k2<-as.numeric(N2)


#PARA k=3
N3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[4]*outer(i,b2[4],'-'))/(1+exp(a[4]*outer(i,b2[4],'-'))))-
      (exp(a[4]*outer(i,b3[4],'-'))/(1+exp(a[4]*outer(i,b3[4],'-')))) #k=3
   N3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC204k3<-as.numeric(N3)



#PARA k=4
N4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[4]*outer(i,b3[4], '-'))/(1+exp(a[4]*outer(i,b3[4], '-')))) #k=4
   N4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC204k4<-as.numeric(N4)


#Resultado de probabilidades
dataliC204<-data.frame(yC204k1,yC204k2,yC204k3,yC204k4,ttecho)
names(dataliC204)=c('PC204k1','PC204k2','PC204k3','PC204k4','Habilidad')
write_xlsx (dataliC204,'ProbabilidadesC204k.xlsx')
dataliC204

#Curvas Características
png("CCCs_FuentesFinanciamientoC204.png", width=680, height=480)
ggplot(data=dataliC204, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC204k1, colour = "k=1")) +
   geom_smooth(aes(y=PC204k2, colour = "k=2")) +
   geom_smooth(aes(y=PC204k3, colour = "k=3")) +
   geom_smooth(aes(y=PC204k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C204 de Fuentes de Financiamiento") +
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


#--------------- CCCs C205-------------------
#PARA k=1 
O1 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- 1-(exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-')))) #k=1
   O1[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC205k1<-as.numeric(O1)



#PARA k=2
O2 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b1[5],'-'))/(1+exp(a[5]*outer(i,b1[5],'-'))))-
      (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-')))) #k=2
   O2[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC205k2<-as.numeric(O2)


#PARA k=3
O3 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <- (exp(a[5]*outer(i,b2[5],'-'))/(1+exp(a[5]*outer(i,b2[5],'-'))))-
      (exp(a[5]*outer(i,b3[5],'-'))/(1+exp(a[5]*outer(i,b3[5],'-')))) #k=3
   O3[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC205k3<-as.numeric(O3)



#PARA k=4
O4 <- list()
j <- 1
for (i in ttecho) {
   Sujeto <-(exp(a[5]*outer(i,b3[5], '-'))/(1+exp(a[5]*outer(i,b3[5], '-')))) #k=4
   O4[[j]]<-mean(Sujeto)
   j <- j + 1
}
yC205k4<-as.numeric(O4)


#Resultado de probabilidades
dataliC205<-data.frame(yC205k1,yC205k2,yC205k3,yC205k4,ttecho)
names(dataliC205)=c('PC205k1','PC205k2','PC205k3','PC205k4','Habilidad')
write_xlsx (dataliC205,'ProbabilidadesC205k.xlsx')
dataliC205

#Curvas Características
png("CCCs_FuentesFinanciamientoC205.png", width=680, height=480)
ggplot(data=dataliC205, aes(x = Habilidad)) +
   geom_smooth(aes(y=PC205k1, colour = "k=1")) +
   geom_smooth(aes(y=PC205k2, colour = "k=2")) +
   geom_smooth(aes(y=PC205k3, colour = "k=3")) +
   geom_smooth(aes(y=PC205k4, colour = "k=4")) +
   ggtitle("Curvas Característica de Categoría para el ítem C205 de Fuentes de Financiamiento") +
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

# ----------- Informacion ítem C201------------

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
IC201<-as.numeric(X1)


# ----------- Informacion ítem C202------------

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
IC202<-as.numeric(X2)


# ----------- Informacion ítem C203------------

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
IC203<-as.numeric(X3)

# ----------- Informacion ítem C204------------

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
IC204<-as.numeric(X4)

# ----------- Informacion ítem C205------------

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
IC205<-as.numeric(X5)



Habilidad <- ttecho
dataIIC <- data.frame(IC201,IC202,IC203,IC204,IC205,Habilidad)
write_xlsx (dataIIC,'InformacionItemFF.xlsx')


library(ggplot2)
#Curvas Características
png("IICs_Fuentes_Financiamiento.png", width=680, height=480)
ggplot(data=dataIIC, aes(x = Habilidad)) +
   geom_smooth(aes(y=IC201, colour = "C201")) +
   geom_smooth(aes(y=IC202, colour = "C202")) +
   geom_smooth(aes(y=IC203, colour = "C203")) +
   geom_smooth(aes(y=IC204, colour = "C204")) +
   geom_smooth(aes(y=IC205, colour = "C205")) +
   ggtitle("IICs para Fuentes de Financiamiento") +
   xlab("Habilidad") +
   ylab("Información del Item") +
   theme(
      legend.position = c(1,0),
      legend.justification = c("right", "bottom")
   )+
   scale_colour_manual(name = "Categorías", 
                       values = c("C201" = "blue", 
                                  "C202" = "magenta",
                                  "C203" = "green",
                                  "C204" = "orange",
                                  "C205" = "black" 
                                    ))
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
TestInfo<-cbind(as.data.frame(IC201),
                as.data.frame(IC202),
                as.data.frame(IC203),
                as.data.frame(IC204),
                as.data.frame(IC205))
TestInfo = suma_renglones(TestInfo)
Valores <- TestInfo$Total

#plot(ttecho,Valores, col = "green",type = 'p')

library(ggplot2)

datosFF <- data.frame(ttecho,Valores)

png("Informacion_Test_Fuentes_Financiamiento.png", width=680, height=480)
ggplot(datosFF,aes(ttecho,Valores)) +
  geom_line(colour = 'green')  + 
  ggtitle("Información del test ''Fuentes de Financiamiento''") +
  xlab("Habilidad") +
  ylab("Información del test") +
  #geom_point( size=2, shape=21, fill="white") +
  theme_minimal()
dev.off()