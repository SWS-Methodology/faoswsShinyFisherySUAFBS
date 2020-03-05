# packages
.libPaths( c("/usr/local/lib64/R-3.1.2/library","/work/SWS_R_Share/shiny/Rlib/3.1",.libPaths()))

library(data.table)
library(DT)
library(faosws)
library(faoswsFlag)
library(faoswsProcessing)
library(faoswsUtil)
library(faoswsImputation)
library(ggplot2)
library(rhandsontable)
library(shiny)
library(shinyWidgets)

#setwd('R:/shiny-app/shinyFisheriesSUAFBS')

source('R/recalculateFunctions.R')
source('R/ErCalc.R')
source('R/InputCalc.R')
source('R/FoodProcCalc.R')

#-- Token QA ----
# if(CheckDebug()){
# 
#   library(faoswsModules)
#   SETTINGS = ReadSettings("sws.yml")
# 
#   ## If you're not on the system, your settings will overwrite any others
#   R_SWS_SHARE_PATH = SETTINGS[["share"]]
# 
#   ## Define where your certificates are stored
#   SetClientFiles(SETTINGS[["certdir"]])
# 
#   ## Get session information from SWS. Token must be obtained from web interface
#   GetTestEnvironment(baseUrl = SETTINGS[["server"]],
#                      token = '54992801-519f-4d80-89f4-2de4aadada87')
# 
# }

R_SWS_SHARE_PATH = "Z:"
SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                   token = "54992801-519f-4d80-89f4-2de4aadada87")


#-- Encoding ----

replaceforeignchars <- function(dat)
{
  fromto <- read.table(text="
                       from to
                       š s
                       Å A
                       œ oe
                       ž z
                       ß ss
                       þ y
                       à a
                       á a
                       â a
                       ã a
                       ä a
                       å a
                       æ ae
                       ç c
                       è e
                       é e
                       ê e
                       ë e
                       ì i
                       í i
                       î i
                       ï i
                       ð d
                       ñ n
                       ò o
                       ó o
                       ô o
                       õ o
                       ö o
                       ø oe
                       ù u
                       ú u
                       û u
                       ü u
                       ý y
                       ÿ y
                       ğ g",
                       header=TRUE)
  
  for(i in 1:nrow(fromto) ) {
    dat <- gsub(fromto$from[i],fromto$to[i],dat)
  }
  dat
}
#-- Lists ----

# Tokens
tokens <- ReadDatatable('fi_sua_fbs_token')
tokenSuaU <- tokens$token[1]
tokenSuaB <- tokens$token[2]
tokenFbs <- tokens$token[3]

# Country input
M49 <- GetCodeList(domain ="FisheriesCommodities", dataset = "commodities_total", dimension = "geographicAreaM49_fi") #, codes = countrylist$geographicaream49_fi)
M49 <- M49[ type == "country", .( description, code)]
M49$description <- replaceforeignchars(M49$description)

country_input <-  sort(sprintf("%s - %s", M49$description, as.numeric(M49$code)))   # unique(commodityDB$geographicAreaM49)
country_input <- data.table(label = country_input, code = sub(" ", "", sub(".*-", "", country_input)))
country_input <- rbind(data.table(label = "", code = "-"), country_input)

# Element groups

elementGroups <- c('Single', 'All', 'Primary', 'SUA', 'Nutrients')

# Element input

allElements <- GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs")[, .(description, code)]

# FBS elements
included <- data.table(code = c('5510', '5610', '5910', '5302', '5071', '5153', '5141', '5038', '511', '264', '274', '284'))
included[ , idx := as.numeric(row.names(included))]
elements <- merge(allElements, included, by = 'code')
elements <- elements[order(elements$idx)]
element_label <-  sprintf("%s - %s", elements$description, as.numeric(elements$code))
element_input <- data.table(label = element_label, code = elements$code)

# SUA elements
SUAincluded <- data.table(code = c('5302', '5423', '5510', '5610', '5071', '5910', '5520', '5525', '5023', 
                                   '5141', '5166',  '261', '264', '271', '274', '281', '284'))

SUAincluded[ , idx := as.numeric(row.names(SUAincluded))]
SUAelements <- merge(allElements, SUAincluded, by = 'code')
SUAelements <- SUAelements[order(SUAelements$idx)]
sua_element_label <-  sprintf("%s - %s", SUAelements$description, as.numeric(SUAelements$code))
sua_element_input <- data.table(label = sua_element_label, code = SUAelements$code)

# Group input
groups <- GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredItemFaostat_L2")[, .(description, code)]
groups <- groups[order(as.numeric(code)),]
groups_label <-  sprintf("%s - %s", groups$description, as.numeric(groups$code))
groups_input <- data.table(label = groups_label, code = groups$code)

# Map Faostat L1 and Faostat L2 codes
l2l1 <- ReadDatatable('ics_l1_2_ics_l2')
l2l1 <- l2l1[order(as.numeric(code_l2)),]
# Map Asfis - Isscaap - Ics
map_asfis <- ReadDatatable('map_asfis')
setnames(map_asfis, c("ics", "asfis"), c("ICSProd", "fisheriesAsfis"))

# Map Isscfc - Ics
map_isscfc <- ReadDatatable('map_isscfc')
setnames(map_isscfc, c("ics","measured_item_isscfc"), c("ICSProd","measuredItemISSCFC"))

