require(foreign)
require(neuralnet)
install.packages("bigmemory")
require(bigmemory)
?bigmemory
gc()
vars <- readRDS("VARS.rds")

vars$CLUES.STATE <- CLUES.STATE

summary(b$INTENTIONALITY.OF.INJURY)

saveRDS(vars, "FINAL VARS.rds")

dat2 <- na.omit(vars)
length(vars$WEIGHT.KGS)
class(vars$WEIGHT.KGS)
summary(is.na(vars))

vars$INDIGENOUS.LANGUAGE.SPOKEN[vars$INDIGENOUS.SPEAKER=="NO"] <- "NONE"
levels(vars$INDIGENOUS.LANGUAGE.SPOKEN)

vars$INDIGENOUS.LANGUAGE.SPOKEN[dat2$INDIGENOUS.SPEAKER=="NO"] <- "NONE"

str(a)

vars$INDIGENOUS.PERSON <- as.factor(vars$INDIGENOUS.PERSON, levels=c("YES", "NO", "DOESN'T KNOW"))

dat <- discharge3

dat$INIG.SPAN <- with(dat, interaction(is.indiginous.speaker, SPANISH.SPEAKER))

dat$SPANISH.SPEAKER[is.na(dat$SPANISH.SPEAKER)] <- 0
                  
dat$INDIGENOUS.LANGUAGE.SPOKEN[is.na(dat$INDIGENOUS.LANGUAGE.SPOKEN)] <- 0               

dat$INDIGENOUS.PERSON <- factor(dat$INDIGENOUS.PERSON, levels = c(0,1))

dat$SPANISH.SPEAKER <- factor(dat$SPANISH.SPEAKER, levels = c(0,1))

dat$INDIGENOUS.LANGUAGE.SPOKEN <- factor(dat$INDIGENOUS.LANGUAGE.SPOKEN, levels = c(seq(0,67)))

dat <- dat[,-c(27,28)]

dat_curr$CAUSE.OF.INJURY <- substr(dat_curr$CAUSE.OF.INJURY,1,1)

dat_curr$TRANSFERED.FROM <- as.character(substr(dat_curr.orig$TRANSFERED.FROM,1,1))

dat_curr$LENGTH.OF.STAY <- as.numeric(dat_curr$LENGTH.OF.STAY)

dat_curr$HEIGHT.CMS <- as.numeric(dat_curr$HEIGHT.CMS)

dat_curr$WEIGHT.KGS <- as.numeric(dat_curr$WEIGHT.KGS)

dat[is.na(dat)] <- 0
sapply(discharge$MUNICIPALITY, summary)
names(dat_curr)
dat_curr[,10] <- substr(dat_curr.orig$CAUSE.OF.INJURY,1,1)

colnames(dat_curr)[10] <- "CAUSE.OF.INJURY"
saveRDS(dat_curr, "VAR.ageint.rds")
names(dat_curr)

## neuralnet

d <- discharge3[,-c(7,8)]]

## create L.O.S. qualatative catagories

d$LENGTH.OF.STAY <- as.numeric(discharge$LENGTH.OF.STAY)

d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=0] <- "No Stay"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=1] <- "One Day"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=2] <- "Two Days"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=3] <- "Three Days"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=4] <- "Four Days"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=5] <- "Five Days"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=6] <- "Six Days"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=7] <- "One Week"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=7] <- "Two Weeks"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=14] <- "Three Weeks"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=21] <- "One Month"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=30] <- "Two Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=60] <- "Three Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=90] <- "Four Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=120] <- "Five Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=150] <- "Six Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=180] <- "Seven Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=210] <- "Eight Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=240] <- "Nine Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=270] <- "Ten Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=300] <- "Eleven Months"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>=330] <- "One Year"
d$LENGTH.OF.STAY[d$LENGTH.OF.STAY1>365] <- "Over 1 Year"


d$LENGTH.OF.STAY <- ordered(d$LENGTH.OF.STAY, labels=c("No Stay", "One Day", "Two Days", "Three Days", "Four Days", "Five Days", "Six Days", "One Week", "Two Weeks", "Three Weeks", "One Month", "Two Months", "Three Months", "Four Months", "Five Months", "Six Months", "Seven Months", "Eight Months", "Nine Months", "Eleven Months", "One Year", "Over 1 Year"))

levels(d$LENGTH.OF.STAY)

set.seed(1)

train_idx <- sample(1:nrow(a[,-16]), nrow(a[,-16])*.8)

train <- a[train_idx,]

test <- a[-train_idx,]

set.seed(1)
nn <- neuralnet(eq, train, hidden = c(4,10), linear.output = T)

View(model.matrix(train))

sapply(dat_curr, summary)

str(dat_curr.orig)
dat_curr.orig$CLUES.MUN <- as.numeric(dat_curr.orig$CLUES.MUN)

dat <- dat_curr.orig

str(dat_curr.orig)
dat_second <- na.omit(dat_curr.orig)
a <- model.matrix(dat_second)
str(dat_second)
?model.matrix
dep <- a[-1]

a$INTRAHOSPITAL.INFECTION <- as.numeric(dat_second$INTRAHOSPITAL.INFECTION)

str(a)
summary(a$INITIAL.DIAGNOSIS)
dat_second$INITIAL.DIAGNOSIS <- factor(substr(dat_second$INITIAL.DIAGNOSIS,1,1))
dat_second$SECOND.DIAGNOSIS <- factor(substr(dat_second$SECOND.DIAGNOSIS,1,1))
a <- model.matrix(dat_second)

eq <- as.formula(paste(c("LENGTH.OF.STAY~",dep), collapse = "+"))
eq


fit1 <- glm(eq, train)
predlm <- predict(fit1,test$LENGTH.OF.STAY)

sum((predlm-test)^2)

plot(test[,-1], predlm)
names(test)

## Random Forest
set.seed(1)
rf <- randomForest(eq, train, mtry=3, importance=T, na.action=na.omit)

gc(full=T)
memory.limit()
