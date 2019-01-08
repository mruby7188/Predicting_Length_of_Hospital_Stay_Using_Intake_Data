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



# Format Date -------------------------------------------------------------

discharge$DISCHARGE <- format(as.Date(discharge$DISCHARGE, "%y-%m-%d"))
discharge$INTAKE <- format(as.Date(discharge$INTAKE, "%y-%m-%d"))

# Fix out of range values in Discharge data-------------------------------------------------
### AGE == 999

discharge$AGE[discharge$AGE==999] <- NA

summary(discharge$AGE[discharge$AGE.CODE==0])  ### Hours; range [1,23]

summary(discharge$AGE[discharge$AGE.CODE==1])  ### Days; range [1,29]

summary(discharge$AGE[discharge$AGE.CODE==2])  ### Months; range [1,11]

summary(discharge$AGE[discharge$AGE.CODE==3])  ### Years; range [1,130]

summary(discharge$AGE[discharge$AGE.CODE==9])  ### check for NA's with valid ages

discharge1$STATISTICAL.MONTH <- factor(discharge1$STATISTICAL.MONTH)

### replace AGE.CODE NA's

discharge$AGE.CODE[discharge$AGE.CODE==9] <- NA

### Weight

discharge$WEIGHT.KGS[discharge$WEIGHT.KGS==999] <- NA

### Height 

discharge$HEIGHT.CMS[discharge$HEIGHT.CMS==999] <- NA

## STATES

discharge$STATE[discharge$STATE==99] <- NA

## SERVICE.CODE

discharge$SERVICE.CODE.1[discharge$SERVICE.CODE.1==999] <- NULL

## LOCATION.OF.INJURY.OCCURANCE

discharge$LOCATION.OF.INJURY.OCCURANCE[discharge$LOCATION.OF.INJURY.OCCURANCE == 9] <- NA

## ORIGIN.OF.ADMISSION

discharge$ORIGIN.OF.ADMISSION[discharge$ORIGIN.OF.ADMISSION == 9] <- NA

## SERVHP

discharge$SERVHP[discharge$SERVHP == 9]

## INDIGENOUS.LANGUAGE.SPOKEN

discharge$INDIGENOUS.LANGUAGE.SPOKEN[discharge$INDIGENOUS.LANGUAGE.SPOKEN == 99] <- 0  ### only 65 indigenous languages

## CAUSE.OF.INJURY

discharge$CAUSE.OF.INJURY[discharge$CAUSE.OF.INJURY==""] <- NA

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


## AGE int -> numeric

discharge$AGE <- as.numeric(discharge$AGE)

View(disc[is.na(disc$AGE)!=is.na(discharge$AGE),])

View(discharge[is.na(discharge$AGE),])

discharge$realage <- with(discharge, interaction(discharge$AGE.CODE, discharge$AGE))

# Factor appropriate data -------------------------------------------------

## CAUSE.OF.INJURY

discharge$CAUSE.OF.INJURY <- as.factor(substr(discharge$CAUSE.OF.INJURY,1,1))

## TUHPSIQ

discharge$TUHPSIQ <- factor(discharge$TUHPSIQ, levels = c(1:3), labels = c('CONTINUOUS', 'PARTIAL', 'SPECIAL CARE'))

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

colnames(discharge1)[13] <- "WEIGHT.KGS"

colnames(discharge1)[12] <- "MALE"

discharge$MALE <- factor(discharge$MALE, levels=c(1,2))

## INSURANCE

discharge$INSURANCE.PROGRAM <- factor(discharge$INSURANCE.PROGRAM, levels = c(1:9,"G","P"))
class(discharge$INSURANCE.PROGRAM)

## MUNICIPALITY

discharge$MUNICIPALITY[discharge$MUNICIPALITY==999] <- NA
summary(discharge$MUNICIPALITY)

MUNICIPALITY <- factor(discharge$MUNICIPALITY)
vars <- cbind(vars,MUNICIPALITY)

## STATE

discharge$STATE <- factor(discharge$STATE, levels = c(1:32))

## INDIGENOUS.PERSON, 1 = YES, 0 = NO
### Count 3 = no respnse and 4 = don't know as NA's

discharge$INDIGENOUS.PERSON[discharge$INDIGENOUS.PERSON==c(3,4)] <- 2

discharge$INDIGENOUS.PERSON <- factor(discharge$INDIGENOUS.PERSON, levels = c(1,2), labels = c(1,0))

discharge$INDIGENOUS.PERSON[is.na(discharge$INDIGENOUS.PERSON)] <- 0

## INDIGENOUS.SPEAKER, 1 = YES, 0 = NO
### Count 3 = no respnse and 4 = don't know as NA's

discharge$INDIGENOUS.SPEAKER[discharge$INDIGENOUS.SPEAKER==c(2,3)] <- 2

discharge$INDIGENOUS.SPEAKER <- factor(discharge$INDIGENOUS.SPEAKER, levels = c(1,2), levels=c(1,0))

discharge$INDIGENOUS.SPEAKER[is.na(discharge$INDIGENOUS.SPEAKER)]

## INDIGENEOUS LANGUAGE

discharge$INDIGENOUS.LANGUAGE.SPOKEN <- factor(discharge$INDIGENOUS.LANGUAGE.SPOKEN,levels = c(1:67))

discharge$INDIGENOUS.LANGUAGE.SPOKEN[is.na(discharge$INDIGENOUS.LANGUAGE.SPOKEN)] <- 0

## indicator for indigeneous language

is.indiginous.speaker <- rep(1, length(discharge$INDIGENOUS.LANGUAGE.SPOKEN))

is.indiginous.speaker[is.na(discharge$INDIGENOUS.LANGUAGE.SPOKEN)] <- 0

is.indiginous.speaker <- as.factor(is.indiginous.speaker)

disc <- cbind(discharge, is.indiginous.speaker, is.tuhpsiq, is.servhc, is.servhp)
View(disc)

## SPANISH SPEAKER

discharge$SPANISH.SPEAKER[discharge$SPANISH.SPEAKER==c(3,4)] <- 2

discharge$SPANISH.SPEAKER <- factor(discharge$SPANISH.SPEAKER, levels = c(1,2), labels = c(1,0))

## SERVICE.TYPE

discharge$SERVICE.TYPE <- factor(discharge$SERVICE.TYPE, c(1,2), labels = c("NORMAL","SHORT STAY"))

## ORIGIN.OF.ADMISSION

dat$ORIGIN.OF.ADMISSION <- factor(dat$ORIGIN.OF.ADMISSION, levels = c(1:4), labels = c('EXTERNAL CONSULT', 'URGENT', 'REFERED','OTHER'))

## TIMES.DIAGNOSED

discharge$TIMES.DIAGNOSED.WITH.CONDITION <- factor(discharge$TIMES.DIAGNOSED.WITH.CONDITION, c(0,1,2), labels = c("FIRST TIME", "SUBSEQUENT"))

## INTRAHOSPITAL INFECTION

discharge$INTRAHOSPITAL.INFECTION  <- factor(discharge$INTRAHOSPITAL.INFECTION, c(1,2), labels=c("YES", "DO NOT"))

## INTENTIONALITY

discharge$INTENTIONALITY.OF.INJURY  <- factor(discharge$INTENTIONALITY.OF.INJURY, c(1:4), labels = c("ACCIDENTAL","DOMESTIC VIOLENCE","NON-FAMILY VIOLENCE", "SELF-HARM"))

## LOCATION.OF.INJURY.OCCURANCE

discharge$LOCATION.OF.INJURY.OCCURANCE <- factor(discharge$LOCATION.OF.INJURY.OCCURANCE, c(1:7))

## INITIALDIAGNOSIS

## Capitalize "o" and change to factor by first letter

discharge$INITIAL.DIAGNOSIS <- as.factor(substr(toupper(discharge$INITIAL.DIAGNOSIS), 1, 1))

## SECOND.DIAGNOSIS

discharge$SECOND.DIAGNOSIS <- as.factor(substr(toupper(discharge$SECOND.DIAGNOSIS), 1, 1))

discharge$SECOND.DIAGNOSIS[discharge$SECOND.DIAGNOSIS==""] <- NA

## REMOVE PSYCHIATRIC PATIENTS

discharge1 <- discharge1[is.na(discharge1$TUHPSIQ),]

## Hospital ID by Municipality

require(readxl)
hosp <- read_excel("ESTABLECIMIENTO_SALUD_201804.xlsx")

discharge$CLUES[is.na(discharge$CLUES)] <- NA

hosp$`CLAVE DEL MUNICIPIO` <- as.numeric(hosp$`CLAVE DEL MUNICIPIO`)

hosp.mun <- hosp$`CLAVE DEL MUNICIPIO`[match(discharge$CLUES, hosp$CLUES)]

CLUES.MUN <- as.factor(hosp.mun)
str(dat_curr)
discharge1 <- cbind(CLUES.MUN, discharge[,-c(1,2)])

discharge2 <- cbind(discharge[,c(1,2)], CLUES.STE, discharge[,c(3:ncol(discharge))])

discharge2$CLUES <- as.character(discharge2$CLUES)
str(discharge1)
####

disc <- readRDS("FINAL VARS.rds")

str(disc)

disch <- cbind(discharge2[,c(1:7)], is.tuhpsiq, discharge2[,"TUHPSIQ"], is.servhc, discharge2[,c(9)], is.servhp, discharge2[,c(10:21)], is.indiginous.speaker, discharge2[,c(22:46)])

a <- names(disch)

a <- replace(a,9, "TUHPSIQ")

a <- replace(a,11, "SERVHC")

colnames(disch) <- a

str(disch)

# ICD10 codes -------------------------------------------------------------

desc.initial <- explain_table(discharge$INITIAL.DIAGNOSIS)

desc.second <- explain_table(discharge$SECOND.DIAGNOSIS)

injury.cause <- explain_table(discharge$CAUSE.OF.INJURY)

for(i in 1:length(disch)) {
  if (is.integer(disch[,i]))
    disch[,i] <- as.numeric(disch[,i])
}

# State Identification ----------------------------------------------------

states <- read_excel("state names.xlsx", col_names = F)

state.name <- as.data.frame(cbind(discharge$ID, discharge$STATE))

state.names <- merge(state.name, states, by.x="STATE.CODE", all.x = T)

state.names <- for(i in 2:nrow(discharge)) 
  
colnames(states) <- c("STATE.CODE", "STATE.NAME")

colnames(state.name) <- c("ID", "STATE.CODE")

states[discharge$STATE[2970812],]

cbind(discharge$STATE[2970812], states$X__2[discharge$STATE[2970812]])

a <- inegi_series(asXMLNode(inegi_code(discharge$STATE)), "ID")
b <- (states[1])

summary(disc$WEIGHT.KGS)

## Change to numeric

dis <- discharge
for(i in 1:ncol(discharge)){
  if(class(discharge[,i])=="character")
    discharge[,i] <- as.numeric(discharge[,i])
}

# Data to be Used ---------------------------------------------------------

discharge3 <- discharge1[, c(1,5:8, 12:15,17,19,22,23,37:40,44)]
names(discharge1)
str(vars)
# Save New Files ----------------------------------------------------------

saveRDS(desc.initial, file = "Initial Diagnosis.csv")
saveRDS(injury.cause, file = "Injury Cause.csv")
saveRDS(desc.second, file = "Secondary Diagnosis.csv")
saveRDS(state.names, file = "State Names.csv")
saveRDS(discharge3, file = "VARS 2.rds")

save
