
library(data.table)
library(faosws)

#-- Token QA ----
if(CheckDebug()){
  
  library(faoswsModules)
  SETTINGS = ReadSettings("sws.yml")
  
  ## If you're not on the system, your settings will overwrite any others
  R_SWS_SHARE_PATH = SETTINGS[["share"]]
  
  ## Define where your certificates are stored
  SetClientFiles(SETTINGS[["certdir"]])
  
  ## Get session information from SWS. Token must be obtained from web interface
  GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                     token = SETTINGS[["token"]])
  
}



groups <- GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredItemFaostat_L2")[, .(description, code)]

L1 <- groups[code %in% as.character(c(seq(10, 90, by = 10), 99))]
L2 <- groups[!code %in% as.character(c(seq(10, 90, by = 10), 99))]

L2[ , group := '0']
L2[code %in% c(as.character(1501:1513), '1585', '1586'), group := '10' ]
L2[code %in% c(as.character(1514:1526)), group := '20' ]
L2[code %in% c(as.character(1527:1539), '1577', '1597'), group := '30' ]
L2[code %in% c(as.character(1540:1552), '1560', '1561', '1569', '1578', '1599'), group := '40' ]
L2[code %in% c(as.character(1553:1559), '1593'), group := '50' ]
L2[code %in% c(as.character(1562:1568), '1592'), group := '60' ]
L2[code %in% c(as.character(1570:1576)), group := '70' ]
L2[code %in% c(as.character(1579:1584)), group := '80' ]
L2[code %in% c(as.character(1587:1591), '1598'), group := '90' ]
L2[code %in% c(as.character(1594:1596)), group := '99' ]


tab <- merge(L2, L1, by.x = 'group', by.y = 'code', all = TRUE )
setcolorder(tab, c('description.y', 'group', 'description.x', 'code'))
setnames(tab, c('description.y', 'group', 'description.x', 'code'),
         c('label_l1', 'code_l1', 'label_l2', 'code_l2'))
write.csv(tab, 'ics_l1_2_ics_l2.csv', row.names = FALSE)
