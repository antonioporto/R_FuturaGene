# Antonio Carlos Porto, 05.09.24
# Aula 1

######################################################################################
####################### R como calculadora/ R as a Calculator ########################
######################################################################################

#sum
4+4

#subtraction
4-3

#multiplication
4*2

#division
4/2

#Exponencial
4^2
4**2

#Root

16^(1/2)
sqrt(16)   #square root
16^(1/3)   # Cube root 
16**(1/4)


# Constante
# constant number
a = 5
b <- 5


# Constante pré-definida
# predefined constant 

Pi <- pi



#######################################################################################
########################### Vetores/ Vectors #########################################
#######################################################################################

# Concatenar elementos ou sub-vetores
# Generate vectors
vetor=c(1,2,3,4) 

# repetir elementos
# repeat
rep(1,200) 

# Gerar sequências repetidas
# Generate repeat sequences 
seq(2,12)

#Vetor numérico
# Numeric Vector

c<-c(1,2,3,4,5,6)   

# Vetor alfanumérico
# String
d<-c("vq","b2","c3","d","e","f")  

# Vector calculate
sqrt(c)


#Matriz 
f<-matrix(c(1,2,3,4,5,6))
h<-matrix(c(1,2,3,4,5,6),nrow=2, ncol=3)   #nrow = linha   ncol= coluna
j<-matrix(c(1,2,3,4,5,6), 2,3)
l<-matrix(c("a","b","c","d","e","f"),2,3)


######################################################################################
####################### Algebra de matrizes/ Matrix Algebra ##########################
######################################################################################

u <- matrix(c(1,2,3,4,5,6,7,8,9),3,3)
v <- matrix(c(2,3,1,2,4,1,1,1,1),3,3)

# sum 
u+v

#subtration
u-v

# multiplication
u*v

# matrix multiplication
u%*%v

# division
u/v


# dataframe create
am <- data.frame(c,d)
ao <- cbind(c,d)


######################################################################################
########################### Criação de dados/ Creating data ##########################
######################################################################################

z <- (1:250)
k <- rnorm(1:250) # rnorm function generates data with normal distribution
y <- rnorm(1:250, mean=125, sd=50)   #mean and standard deviation

matrix(z, 25,10)

# Histogram with vector y
hist(y)


