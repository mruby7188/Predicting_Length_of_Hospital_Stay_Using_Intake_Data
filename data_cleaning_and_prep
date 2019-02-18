require(foreign)
require(readxl)
require(plyr)
require(nlme)
require(glue)
require(plyr)
require(naniar)
require(tibble)
library(magrittr)
require(icd)
require(inegiR)
library(httr)
library(XML)
require(readxl)

# Load Data ---------------------------------------------------------------

## Load Data Sets

conditions <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/AFECCIONES.csv", header = T)

deaths <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/DEFUNC.csv", header = T)

discharge <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/EGRESO.csv", header = T)

births <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/OBSTET.csv", header = T)

procedures <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/PROCEDIMIENTOS.csv", header = T)

babies <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/PRODUCTOS.csv", header = T)

summary(conditions)

#### Facilities in each State

count(discharge, "ENTIDAD")
count(count(discharge, c("ENTIDAD","CLUES")), "ENTIDAD")


# Translate Variable Names ------------------------------------------------

# Discharge ---------------------------------------------------------------

dis.var <- names(discharge)


dis.var <- replace(dis.var, 4, "DISCHARGE")

dis.var <- replace(dis.var, 5, "INTAKE")

dis.var <- replace(dis.var, 6, "LENGTH.OF.STAY")

dis.var <- replace(dis.var, 10, "AGE.CODE")

dis.var <- replace(dis.var, 11, "AGE")

dis.var <- replace(dis.var, 13, "SEX")

dis.var <- replace(dis.var, 14, "WEIGHT.KGS")

dis.var <- replace(dis.var, 15, "HEIGHT.CMS")

dis.var <- replace(dis.var, 16, "INSURANCE.PROGRAM")

dis.var <- replace(dis.var, 17, "STATE")

dis.var <- replace(dis.var, 18, "MUNICIPALITY")

dis.var <- replace(dis.var, 19, "TOWN")

dis.var <- replace(dis.var, 20, "INDIGENOUS.PERSON")

dis.var <- replace(dis.var, 21, "INDIGENOUS.SPEAKER")

dis.var <- replace(dis.var, 22, "INDIGENOUS.LANGUAGE.SPOKEN")

dis.var <- replace(dis.var, 23, "SPANISH.SPEAKER")

dis.var <- replace(dis.var, 24, "SERVICE.TYPE")

dis.var <- replace(dis.var, 25, "SERVICE.CODE.1")

dis.var <- replace(dis.var, 26, "SERVICE.CODE.2")

dis.var <- replace(dis.var, 27, "SERVICE.CODE.3")

dis.var <- replace(dis.var, 28, "DISCHARGE.CODE")

dis.var <- replace(dis.var, 29, "procedure.LENGTH.HRS")

dis.var <- replace(dis.var, 30, "HOURS.IN.DELIVERY")

dis.var <- replace(dis.var, 34, "ORIGIN.OF.ADMISSION")

dis.var <- replace(dis.var, 35, "TRANSFERED.FROM")

dis.var <- replace(dis.var, 36, "DISCHARGE.REASON")

dis.var <- replace(dis.var, 37, "TRANSFER.TO")

dis.var <- replace(dis.var, 38, "INITIAL.DIAGNOSIS")

dis.var <- replace(dis.var, 39, "SECOND.DIAGNOSIS")

dis.var <- replace(dis.var, 40, "TIMES.DIAGNOSED.WITH.CONDITION")

dis.var <- replace(dis.var, 41, "INTRAHOSPITAL.INFECTION")

dis.var <- replace(dis.var, 42, "CAUSE.OF.INJURY")

dis.var <- replace(dis.var, 43, "INTENTIONALITY.OF.INJURY")

dis.var <- replace(dis.var, 44, "LOCATION.OF.INJURY.OCCURANCE")

dis.var <- replace(dis.var, 45, "STATISTICAL.MONTH")

colnames(discharge) <- dis.var

# Conditions --------------------------------------------------------------

cond.var <- names(conditions)

cond.var <- replace(cond.var, 2, "COMORBID.CONDITION")

colnames(conditions) <- cond.var


# Deaths ------------------------------------------------------------------

death.var <- names(deaths)

death.var <- replace(death.var, 3, "FIRST.COD")

death.var <- replace(death.var, 4, "FIRST.COD.TIME.KEY")

death.var <- replace(death.var, 5, "FIRST.COD.TIME")

death.var <- replace(death.var, 6, "SECOND.COD")

death.var <- replace(death.var, 7, "SECOND.COD.TIME.KEY")

death.var <- replace(death.var, 8, "SECOND.COD.TIME")

death.var <- replace(death.var, 9, "THIRD.COD")

death.var <- replace(death.var, 10, "THIRD.COD.TIME.KEY")

death.var <- replace(death.var, 11, "THIRD.COD.TIME")

death.var <- replace(death.var, 12, "FOURTH.COD")

death.var <- replace(death.var, 13, "FOURTH.COD.TIME.KEY")

death.var <- replace(death.var, 14, "FOURTH.COD.TIME")

death.var <- replace(death.var, 15, "FIFTH.COD")

death.var <- replace(death.var, 16, "FIFTH.COD.TIME.KEY")

death.var <- replace(death.var, 17, "FIFTH.COD.TIME")

death.var <- replace(death.var, 18, "SIXTH.COD")

death.var <- replace(death.var, 19, "SIXTH.COD.TIME.KEY")

death.var <- replace(death.var, 20, "SIXTH.COD.TIME")

death.var <- replace(death.var, 21, "BASIC.CAUSE")

colnames(deaths) <- death.var


# Births ---------------------------------------------------------------

birth.var <- names(births)

birth.var <- replace(birth.var, 2, "TOTAL.PREGNANCIES")

birth.var <- replace(birth.var, 3, "TOTAL.BIRTHS")

birth.var <- replace(birth.var, 4, "ABORTIONS")

birth.var <- replace(birth.var, 6, "TYPE.OF.procedure")

birth.var <- replace(birth.var, 7, "WEEKS.PREGNANT")

birth.var <- replace(birth.var, 8, "CHILDREN.BIRTHED")

birth.var <- replace(birth.var, 9, "METHOD.OF.BIRTH")

birth.var <- replace(birth.var, 10, "BIRTH.CONTROL")

colnames(births) <- birth.var

# procedures -------------------------------------------------------------

proc.var <- names(procedures)

proc.var <- replace(proc.var, 2, "MEDICAL PROCEDURE # FROM DISCHARGE")

proc.var <- replace(proc.var, 3, "PROCEDURE CODE")

proc.var <- replace(proc.var, 5, "OPERATION")

proc.var <- replace(proc.var, 6, "HOURS IN OPERATION")

proc.var <- replace(proc.var, 7, "MINUTES IN OPERATION")

names(procedures) <- proc.var

## TIPO (levels D F Q T)?

procedures$TIPO[1]

# Babies ------------------------------------------------------------------

baby.var <- names(babies)

baby.var <- replace(baby.var, 2, "BABIES.BORN")

baby.var <- replace(baby.var, 3, "BIRTH.WEIGHT.GRAMS")

baby.var <- replace(baby.var, 4, "SEX.OF.BABY")

baby.var <- replace(baby.var, 5, "BORN.ALIVE")

baby.var <- replace(baby.var, 6, "DISCHARGE.CONDITION")

baby.var <- replace(baby.var, 7, "APGAR5")

baby.var <- replace(baby.var, 8, "BABY.WAS.REVIVED")

baby.var <- replace(baby.var, 9, "LENGTH.OF.STAY.HRS")

colnames(babies) <- baby.var

# Remove NA ID's and Save new files -------------------------------------------------

## incomplete ID's

# Afecciones

cond.inc <- conditions[is.na(conditions$ID),]

cond <- conditions[!is.na(conditions$ID),]

write.csv(cond, file = "Conditions (formerly AFECCIONES).csv")

write.csv(cond.inc, file="Conditions (formerly AFECCIONES) NA ID's.csv")

# Defunc

death.inc <- deaths[is.na(deaths$ID),]

death <- deaths[!is.na(deaths$ID),]

write.csv(death, file = "Deaths (formerly DEFUNC).csv")

write.csv(death.inc, file="Deaths (formerly DEFUNC) NA ID's.csv")

# Egreso

disch.inc <- discharge[is.na(discharge$ID),]

disch <- discharge[!is.na(discharge$ID),]

write.csv(disch, file = "Discharges (formerly EGRESO).csv")

write.csv(disch.inc, file="Discharges (formerly EGRESO) NA ID's.csv")

# Obstet

birth.inc <- births[is.na(births$ID),]

birth <- births[!is.na(births$ID),]

write.csv(birth, file = "Births (formerly OBSTET).csv")

write.csv(birth.inc, file="Births (formerly OBSTET) NA ID's.csv")

# Procedimientos

proced.inc <- procedures[is.na(procedures$ID),]

proced <- procedures[!is.na(procedures$ID),]

write.csv(proced, file = "Procedures (formerly PROCEDIMIENTOS).csv")

write.csv(proced.inc, file="Procedures (formerly PROCEDIMIENTOS) NA ID's.csv")

# Productos

baby.inc <- babies[is.na(babies$ID),]

baby <- babies[!is.na(babies$ID),]

write.csv(baby, "Babies(formerly PRODUCTOS).rds")

write.csv(baby.inc, file="Babies (formerly PRODUCTOS) NA ID's.csv")


# Look for other NA values ------------------------------------------------




summary(baby)

summary(birth)

summary(cond)

summary(death)

summary(disch)

summary(proced)

## merge patient info with discharge info

## condition and discharge

cond.disch <- merge(cond, disch, by = "ID")

## procedures and discharge

proced.deaths <- merge(proced, disch, by = "ID")

## births and discharge

birth.disch <- merge(birth, disch, by = "ID")

## babies and discharge

bab.disch <- merge(baby, disch, by = "ID")

## death and discharge, expect 0 value

death.disch <- merge(death, disch, by = "ID")

##### merged df has same number of obs; so deaths are counted as discharges

summary(death.disch)

## merge condition(afecciones) and procedure(procedimientos)
?merge

proc <- proced[proced$ID %in% cond$ID,]
con <- cond[cond$ID %in% proced$ID,]
treatment <- merge(cond, proc, by = "ID")

summary(treatment)
summary(treat$ID==3)
treat$ID == 3
View(treat$ID)


### Merge egreso and obstet

births.fac <- merge(births, discharge, by.x = "ID")
View(births.fac)
births[births$ID==3,]
discharge[discharge$ID==3,]

# Load new data -----------------------------------------------------------

discharge <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/Discharges (formerly EGRESO).csv")

disc <- read.csv("C:/Users/Mich/Desktop/ECON 484/Group Project/484 Group Project/EGRESO.csv")

# Format Date -------------------------------------------------------------
 
discharge$DISCHARGE <- format(as.Date(discharge$DISCHARGE))
discharge$INTAKE <- format(as.Date(discharge$INTAKE))

# Fix out of range values in Discharge data-------------------------------------------------
### AGE == 999

discharge$AGE[discharge$AGE==999] <- NA

summary(discharge$AGE[discharge$AGE.CODE==0])  ### Hours; range [1,23]

summary(discharge$AGE[discharge$AGE.CODE==1])  ### Days; range [1,29]

summary(discharge$AGE[discharge$AGE.CODE==2])  ### Months; range [1,11]

summary(discharge$AGE[discharge$AGE.CODE==3])  ### Years; range [1,130]

summary(discharge$AGE[discharge$AGE.CODE==9])  ### check for NA's with valid ages

### replace AGE.CODE NA's

discharge$AGE.CODE[discharge$AGE.CODE==9] <- NA

### Weight

discharge$WEIGHT.KGS[discharge$WEIGHT.KGS==999] <- NA

### Height 

discharge$HEIGHT.CMS[discharge$HEIGHT.CMS==999] <- NA

## STATES

discharge$STATE[discharge$STATE==99] <- NA

## SERVICE.CODE

discharge$SERVICE.CODE.1[discharge$SERVICE.CODE.1==999] <- NA

## LOCATION.OF.INJURY.OCCURANC

discharge$LOCATION.OF.INJURY.OCCURANCE[discharge$LOCATION.OF.INJURY.OCCURANCE == 9] <- NA

## ORIGIN.OF.ADMISSION

discharge$ORIGIN.OF.ADMISSION[discharge$ORIGIN.OF.ADMISSION == 9] <- NA

## SERVHP
 
discharge$SERVHP[discharge$SERVHP == 9] <- NA

## INDIGENOUS.LANGUAGE.SPOKEN

discharge$INDIGENOUS.LANGUAGE.SPOKEN[discharge$INDIGENOUS.LANGUAGE.SPOKEN == 99] <- NA  ### only 65 indigenous languages

## DEATH bivariate, 1=YES, 0=NO

DIED <- as.data.frame(discharge$DISCHARGE.REASON)
colnames(DIED) <- "DIED"

DIED[DIED == 5] <- 0

DIED[DIED == 6] <- 0

DIED[DIED < 4] <- 0

DIED[DIED==9] <- NA

DIED[DIED==4] <- 1

DIED <- factor(DIED, levels = c(0,1), labels=c('NO','YES'))
View(DIED)


# Factor appropriate data -------------------------------------------------

## TUHPSIQ

discharge$TUHPSIQ <- factor(discharge$TUHPSIQ, levels = c(1:3, NA), labels = c('CONTINUOUS', 'PARTIAL', 'SPECIAL CARE'))

### indicator

is.tuhpsiq <- rep(1, length(discharge$TUHPSIQ))

is.tuhpsiq[is.na(discharge$TUHPSIQ)] <- 0

is.tuhpsiq <- as.factor(is.tuhpsiq, levels=c(0,1))

## SERVHC

discharge$SERVHC <- factor(discharge$SERVHC, levels = c(1:5))

### indicator

is.servhc <- rep(1, length(discharge$SERVHC))

is.servhc[is.na(discharge$SERVHC)] <- 0

is.servhc <- as.factor(is.servhc)

## SERVHP

discharge$SERVHP <- factor(discharge$SERVHP, levels = c(1:4), labels = c("DAY", "NIGHT", "WEEKEND", "OTHER"))

### indicator

is.servhp <- rep(1, length(discharge$SERVHP))

is.servhp[is.na(discharge$SERVHP)] <- 0

is.servhp <- as.factor(is.servhc)

## AGE.CODE, change to levels

discharge$AGE.CODE <- ordered(discharge$AGE.CODE, levels= c(0:3), labels=c('DAYS', 'HOURS', 'MONTHS', 'YEARS'))

## NACIOEN

discharge$NACIOEN <- factor(discharge$NACIOEN, levels = c(1,2), labels = c("YES", "NO"))

## SEX
### 1 = MALE, 0 = FEMALE

colnames(discharge)[13] <- "MALE"

discharge$MALE <- factor(discharge$MALE, levels=c(1,2), labels=c("MALE","FEMALE"))

## INSURANCE

discharge$INSURANCE.PROGRAM <- factor(discharge$INSURANCE.PROGRAM, levels = c(1:9,"G","P"))
class(discharge$INSURANCE.PROGRAM)

## MUNICIPALITY

discharge$MUNICIPALITY[discharge$MUNICIPALITY==999] <- NA
summary(discharge$MUNICIPALITY)

marginalization <- read_csv("Base_Indice_de_marginacion_municipal_90-15.csv")

## STATE

discharge$STATE <- factor(discharge$STATE, levels = c(1:32))

## INDIGENOUS.PERSON, 1 = YES, 0 = NO
### Count 3 = no respnse and 4 = don't know as NA's

discharge$INDIGENOUS.PERSON[discharge$INDIGENOUS.PERSON==c(3,4)] <- 3

discharge$INDIGENOUS.PERSON <- factor(discharge$INDIGENOUS.PERSON, levels = c(1:3), labels = c('YES', 'NO', "Doesn't Know"))

summary(discharge$INDIGENOUS.PERSON)

## INDIGENOUS.SPEAKER, 1 = YES, 0 = NO
### Count 3 = no respnse and 4 = don't know as NA's

discharge$INDIGENOUS.SPEAKER[discharge$INDIGENOUS.SPEAKER==c(3,4)] <- 3

discharge$INDIGENOUS.SPEAKER <- factor(discharge$INDIGENOUS.SPEAKER, levels = c(1:3), labels = c('YES', 'NO', "Doesn't Know"))

## INDIGENEOUS LANGUAGE

discharge$INDIGENOUS.LANGUAGE.SPOKEN <- factor(discharge$INDIGENOUS.LANGUAGE.SPOKEN,levels = c(1:67))

## indicator for indigeneous language

is.indiginous.speaker <- rep(1, length(discharge$INDIGENOUS.LANGUAGE.SPOKEN))

is.indiginous.speaker[is.na(discharge$INDIGENOUS.LANGUAGE.SPOKEN)] <- 0

is.indiginous.speaker <- as.factor(is.indiginous.speaker)

disc <- cbind(discharge, is.indiginous.speaker, is.tuhpsiq, is.servhc, is.servhp)
View(disc)

## SPANISH SPEAKER

discharge$SPANISH.SPEAKER <- disc$HABLA_ESP

discharge$SPANISH.SPEAKER[discharge$SPANISH.SPEAKER==c(3,4)] <- 3

discharge$SPANISH.SPEAKER <- factor(discharge$SPANISH.SPEAKER, levels = c(1:3), labels = c('YES', 'NO', "Doesn't Know"))

length(discharge$SPANISH.SPEAKER[is.indiginous.speaker==0])

## SERVICE.TYPE

discharge$SERVICE.TYPE <- factor(discharge$SERVICE.TYPE, c(1,2), labels = c("NORMAL","SHORT STAY"))

## ORIGIN.OF.ADMISSION

discharge$ORIGIN.OF.ADMISSION <- factor(discharge$ORIGIN.OF.ADMISSION, levels = c(1:4), labels = c('EXTERNAL CONSULT', 'URGENT', 'REFERED','OTHER'))

## TIMES.DIAGNOSED

discharge$TIMES.DIAGNOSED.WITH.CONDITION <- factor(discharge$TIMES.DIAGNOSED.WITH.CONDITION, c(1,2), labels = c("FIRST TIME", "SUBSEQUENT"))

## INTRAHOSPITAL INFECTION

discharge$INTRAHOSPITAL.INFECTION  <- factor(discharge$INTRAHOSPITAL.INFECTION, c(1,2), labels=c("YES", "DO NOT"))

## INTENTIONALITY

discharge$INTENTIONALITY.OF.INJURY  <- factor(discharge$INTENTIONALITY.OF.INJURY, c(1:4), labels = c("ACCIDENTAL","DOMESTIC VIOLENCE","NON-FAMILY VIOLENCE", "SELF-HARM"))

## LOCATION.OF.INJURY.OCCURANCE

discharge$LOCATION.OF.INJURY.OCCURANCE <- factor(discharge$LOCATION.OF.INJURY.OCCURANCE, c(1:7))

## INITIAL.CONDITION

## Capitalize "o" and change to factor by first letter

a <- substr(toupper(discharge$INITIAL.DIAGNOSIS), 1, 2)

disc[a=="",]

discharge$INITIAL.DIAGNOSIS <- as.factor(substr(toupper(discharge$INITIAL.DIAGNOSIS), 1, 1))

discharge$SECOND.DIAGNOSIS <- as.factor(substr(toupper(discharge$SECOND.DIAGNOSIS), 1, 1))

discharge$SECOND.DIAGNOSIS[discharge$SECOND.DIAGNOSIS==""] <- NA

View(Initial_Diagnosis[substr(Initial_Diagnosis$code,1,1)=="U",])

levels(discharge$CLUES)

## Hospital ID by state

hosp <- read_excel("ESTABLECIMIENTO_SALUD_201804.xlsx")

discharge$CLUES[is.na(discharge$CLUES)] <- NA

a <- na.omit(discharge$CLUES)


a[hosp$`CLAVE DE LA ENTIDAD`==a,] <- hosp$`NOMBRE DE LA ENTIDAD`

disc <- cbind(discharge, is.indiginous.speaker, is.tuhpsiq, is.servhc, is.servhp)

# ICD10 codes -------------------------------------------------------------

desc.initial <- explain_table(discharge$INITIAL.DIAGNOSIS)

desc.second <- explain_table(discharge$SECOND.DIAGNOSIS)

injury.cause <- explain_table(discharge$CAUSE.OF.INJURY)


# State Identification ----------------------------------------------------



states <- read_excel("state names.xlsx", col_names = F)
state.name <- as.data.frame(cbind(discharge$ID, discharge$STATE))

state.names <- merge(state.name, states, by.x="STATE.CODE", all.x = T)
state.names <- for(i in 2:nrow(discharge)) 
  
colnames(states) <- c("STATE.CODE", "STATE.NAME")
colnames(state.name) <- c("ID", "STATE.CODE")
states[discharge$STATE[2970812],]
states
cbind(discharge$STATE[2970812], states$X__2[discharge$STATE[2970812]])
View(states)
a <- inegi_series(asXMLNode(inegi_code(discharge$STATE)), "ID")
b <- (states[1])
states[1]
class(states)

summary(disc)


# city data ---------------------------------------------------------------
library("XML")
library(httr)
install.packages("devtools")
library(devtools)
install_github("Eflores89/inegiR")
#dependiencies: zoo, XML, plyr, jsonlite
library(inegiR)

doc <- htmlParse(rawToChar(GET(url)$content))
a <- inegi_code(discharge$TOWN)
token<-"webservice_token"
url <- "http://www3.inegi.org.mx/sistemas/api/indicadores/v1//Indicador/216064/00000/es/false/xml/"
View(a[2])

compact_inegi_series(a[1], token)

# Data to be Used ---------------------------------------------------------

disc <- discharge[,c(1:2,6:17,20:24,30,34,38:44,)]

# Save New Files ----------------------------------------------------------

saveRDS(desc.initial, file = "Initial Diagnosis.csv")
saveRDS(injury.cause, file = "Injury Cause.csv")
saveRDS(desc.second, file = "Secondary Diagnosis.csv")
saveRDS(state.names, file = "State Names.csv")
saveRDS(disc, file = "Analysis Variables.rds")
summary(disc$ORIGIN.OF.ADMISSION)
str(disc)

dd <- read.csv("Analysis Variables 1.csv")
d <- read.csv("Analysis Variables.csv")


# Marginalization Data ----------------------------------------------------

margin <- read.dbf("IMM_2015.dbf")

colnames(margin) <- c(" Key of the federative entity", "Name of the federative entity", "Total population", "Percentage of population of 15 years or more illiterate", "Percentage of population aged 15 and over without full primary education", "Percentage of occupants in dwellings without drainage or toilet", "Percentage of occupants in dwellings without electric power", "Percentage of occupants in dwellings without piped water", "Percentage of homes with some level of overcrowding", "Percentage of occupants in dwellings with dirt floors", "Percentage of population in localities with less than 5 000 inhabitants", "Percentage of employed population with income of up to 2 minimum wages", "Marginalization index", "Degree of marginalization", "Key of the municipality","Name of the municipality")


m <- margin[,c(15,16,13,14)]

dd <- merge(discharge, m, by.x = 17, by.y = 1)
