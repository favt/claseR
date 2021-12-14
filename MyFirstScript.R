###############
### Library
library(tidyverse)


Conteo = 1
conteo_dos = 1.37
conteo <-2

conteo == Conteo
conteo != Conteo

vector1 = c(1,2,3,4,5,6)
vector2 = c(1,2,3,4,5,"x",6)


str(vector1)
sum(vector1, na.rm = T)

length(vector1)



#########
#listas
myList = list(vector1)
myList[1]
length(myList[[1]])

str(myList)

summary(vector1)

###########
#factor
estrato = 1:6
estrato_factor = as.factor(estrato)

##############
# dataframe
myStudents = data.frame("name" = c("Felipe", "Bibiana","Daniel","Alejandro", "Leidy", "Edwin", "Victor"),
                        "gender" = c("m", "f","m","m", "f", "m", "m"))

str(myStudents)

summary(myStudents$name)

dim(myStudents)

myStudents$name[1:3]
myStudents[c(1:3),]


sample = c(1,3, nrow(myStudents))
myStudents[sample, 2]

################
#tranformacion
myStudents$nota = c(5,5,4,4,2,2,3)
myStudents$nota_factor = as.factor(c(5,5,4,4,2,2,3))
myStudents$pasa = ifelse(myStudents$nota>=3,"pasa","no pasa")




###############
#functions

function1 = function(param1, param2)
{
  if(is.numeric(param1) & is.numeric(param2))
  {
    conteo = param1 + param2
    conteo
  }
  else
  {
    message = "no son variables numéricas"
    message
  }
}

function1("xx", "8")

