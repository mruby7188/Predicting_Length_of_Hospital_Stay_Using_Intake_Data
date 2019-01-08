require(readxl)
require(plyr)
require(nlme)
require(glue)
require(dplyr)

## Load Data Sets

conditions <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/AFECCIONES.csv")
deaths <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/DEFUNC.csv")
discharge <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/EGRESO.csv")
births <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/OBSTET.csv")
proceedures <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/PROCEDIMIENTOS.csv")
babies <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/PRODUCTOS.csv")


## excecises

#### Facilities in each State

count(discharge, "ENTIDAD")
count(count(discharge, c("ENTIDAD","CLUES")), "ENTIDAD")

### Merge egreso and obstet

births.fac <- merge(births, discharge, by.x = "ID")
View(births.fac)
births[births$ID==3,]
discharge[discharge$ID==3,]
