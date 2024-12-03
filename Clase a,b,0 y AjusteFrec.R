#-------- Modelado de frecuencias - Clase (a,b,0) y Ajuste de Frecuencias Ejemplo ---------
# MASDFyR - Casta√±eda D√≠az Carlos Omar
# Marzo-Junio 2021

#--------- Cargamos nuestros datos que se usar√°n para el ajuste-----
datos <- read.csv("SingaporeAuto.csv")
# R lo lee como un data frame, donde manipularemos todos los datos

class(datos)


#--------- Recursividad de la dist. Poisson con lambda=3-------
lambda<-3
a<-0
b<-lambda

# Este ciclo calcula las probas recursivas de la clase (a,b,0)
p <- rep(0,20)
p
# Se obtiene la probabilidad en n=0 para empezar con la f√≥rmula recursiva 
p[1]<- exp(-lambda)
# Comienzo del ciclo for
for(k in 1:19){
  p[k+1]<-(a+b/k)*p[k]  # Probabilidad del elemento i-th seg√∫n la f√≥rmula ab0
}
p

# Se pueden comparar las probas con la densidad Poisson.
p-dpois(seq(0,19, 1), lambda=3)

# Son iguales, al menos hasta m√°s de 15 decimales... por lo que se puede 
# considerar como una buena aproximaci√≥n. 

# Creemos una funci√≥n general de recursividad
# Recibe como par√°metros k el valor donde se eval√∫a la densidad, a,b y p0 el valor
# inicial de la densidad.
ab0 <- function(k,a,b,p0){
  while(k>0){
    return((a+b/k)*ab0(k-1,a,b,p0))
  }
  return(p0)
}
#Probando una Poisson cualquiera
ab0(100,0,50,exp(-50))
dpois(100, lambda=50)
#----- An√°lisis de nuestros datos ------
names(datos) #claims count / conteo de reclamaciones
dim(datos) #observaciones y variables (filas y columnas)
 
# Se tienen 15 variables, como el sexo, el tipo de veh√≠culo, sus a√±os, la frec 
# de su reclamaci√≥n, y 7483 obs

attach(datos)  # para especificar que columna quieres en la que te fijes, funciona cunado las colum. tinene distinto nombre
head(datos$SexInsured,10) #
head(SexInsured,10)

# Nos interesa analizar la frecuencia de siniestros (reclamaciones) obs, lo hacemos a trav√©s de
# la variable Clm_Count, que es el num. de accidentes automovil√≠sticos (reclamaciones) 
# por asegurado.

table(Clm_Count) #frecuencia de las reclamaciones / n˙mero de reclamaciones y cant. de personas que las tuvierÛn

# Verifiquemos el n√∫mero de p√≥lizas aseguradas
n <- length(Clm_Count)
n

#----------- Modelo de ajuste -----------------
# En este momento, se debe proponer un modelo que se crea que puede ajustar a traves,
# de estadÌstica descriptiva, realizarle todo el desarrollo y al final ver si ajusta... 
# si ajusta, we are the Champions como dirÌa Queen, sino, se debe intentar con otro...

# Intentemos con un modelo Poisson.

#----------Ajuste Poisson---------------

# Creemos que es buena idea ajustar un Poisson, asÌ que debemos saber primero su
# par·°metro lambda, que seg˙n nuestros super cursos de EstadÌstica, sabemos que
# por m·xima verosimilitud es X barra.
lambda_empirica <- mean(Clm_Count)
lambda_empirica #cant. promedio de reclamaciones por pÛliza

#En promedio se tienen lambda_empirica reclamaciones por pÛliza. 

# Eso puede ser a mano, pero... øY si hay muchos par·metros?... problem... aquÌ sabemos porque sabemos su estimador 
# Entonces usualmente un mÈtodo m·s rudo y formal, es usar 
# modelos lineales generalizados, que ya nos dan todos esos valores, 
# Se ver·n prÛximamente de sus cursos de TeorÌa del riesgo
# o de AAR AdministraciÛn actuarial del riesgo

lambda_gorro<-lambda_empirica

#IMPORTANTE Sacar primero como AJUSTA 
#Una vez que propnes el modelos ves como ajusta, como serpa la proba

# Modelamos los datos con la X barra propuesta y las probas por a,b,0
# Podr√≠a usarse pa las probas dpois(x,lambda)
tabla_estimada <- cbind(n*(ab0(0,0,lambda_gorro,exp(-lambda_gorro))),
                n*(ab0(1,0,lambda_gorro,exp(-lambda_gorro))),
                n*(ab0(2,0,lambda_gorro,exp(-lambda_gorro))),
                n*(ab0(3,0,lambda_gorro,exp(-lambda_gorro))),
                n*(1-ppois(3,lambda_gorro)))

#Ver en mi modelo que porpongo como sale la tabla de claims count. Con la proba que se obtengan 0 reclamaciones qusiera que sea un n˙mero cercano al que tenemos de 
#reclamaciones O. Lo probamos aquÌ con nuestra lamda estimada. 

tabla_estimada
# La tabla obs guarda la observaciones reales
obs <- data.frame(table(Clm_Count))[,2];
obs[5] <- 0  # Asignamos 0 al conteo de reclamaci√≥nes > que 4 en las obs.
obs

tabla2p<-rbind(c(0,1,2,3,"4+"),obs,round(tabla_estimada, digits = 2))
rownames(tabla2p) <- c("NumeroRecla","Observados", "Estimado usando Poisson")
tabla2p

# øSe ve razonable?... øUstedes que creen?
#En general un modelo que ajuste en algunos datos si y en otros no, no es bueno. No podemos compensar unos datos con otros. 

#------- Bondad de Ajuste Poisson ----------- Pruebas de hipÛtesis de estadÌstica 1
# Otro spoiler de sus super cursos de estadÌtica II, las pruebas de 
# bondad de ajuste sirven para saber que tan bien ajusta el modelo a 
# los datos, hay para discretas y para continuas. En este caso, se usa
# la Ji^2 que es la m·s famosa para discretas. Sirve para ver ajuste e independencia 
# Ho casi siempre es que ajusta.
#teoricos=estimados 
# EstadÌstica de Prueba T
diff <- obs-tabla_estimada
diff
Pearson_p <- sum(diff*diff/tabla_estimada)
Pearson_p

# P-Value de la prueba. Pvalue es el error que podrÌas cometer al rechazar H0
1-pchisq(Pearson_p, df=5-1-1)

# El valor grande del estadÌstico de bondad de ajuste 41,98438
# o el valor p pequeÒo indica que existe una gran diferencia entre 
# los recuentos reales y los anticipados con el modelo de Poisson.

#6997.2

# Recordatorio: el PValue puede verse como el error que yo tendrÌa 
# al rechazar Ho. Si usamos alpha=0.05:
# Si p<0.05 el error es aceptable as√≠ que rechazamos Ho
# Si p>=0.05 el error no es aceptable, as√≠ que no se rechaza Ho 
# por lo que la prueba ser√≠a verdadera.

# POR LO TANTO EL MODELO POISSON NO AJUSTA. 

# Entonces se tiene que proponer otro modelo :(

#------- Ajuste binomial negativa -----------

# Su tarea serÌa ver si se ajusta una BN, los par·metros ya se los doy
# porque al ser 2, el calcularlos implica usar otras funciones de R
r <- 0.874019
beta <- 0.9259551 

# Pueden usar dnbinom(k,r,beta) en lugar de la funciÛn(a,b,0) si asÌ lo deciden
# Para el P-VALUE se usa:
# 1-pchisq(Pearson_nb, df=5-2-1)






