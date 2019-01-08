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

Babies$APGAR5[Babies$APGAR5 ==99] <- NA

Babies$BORN.ALIVE <- factor(Babies$BORN.ALIVE, levels = c(1,2), labels = c(1,0))
