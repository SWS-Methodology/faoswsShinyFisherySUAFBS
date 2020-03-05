# Functions 

GPrecalc <- function(GP, map_asfis, new_map_asfis, year = input$btn_year){

  # Map to ICS
  t1 <- Sys.time()
  gpMap <- merge(GP, map_asfis, by = c("fisheriesAsfis"), all.x = TRUE)
  
  if(nrow(new_map_asfis) > 0){
    
    new_map_asfis[ end_year == 'LAST', end_year := year]
  newMapping <- merge(gpMap, new_map_asfis, 
                      by.x = c('geographicAreaM49_fi', 'fisheriesAsfis'),
                      by.y = c('country', 'asfis'), all.x = TRUE)
  
  unchanged <- newMapping[is.na(from_code)]
  tochange <- newMapping[!is.na(from_code)]
  
  tochange[ timePointYears < end_year & timePointYears > start_year & ratio == 1, ics := to_code]
  tochange[ timePointYears < end_year & timePointYears > start_year & ratio != 1, Value := Value * (1-ratio)]
  
  duplicate <- tochange[ timePointYears < end_year & timePointYears > start_year & ratio != 1]
  if(nrow(duplicate) > 0){
  duplicate[ , c('ics', 'Value') := list(to_code, Value * ratio)]
  }
  
  changed <- rbind(tochange, duplicate)
  
  gpMap_new <- rbind(unchanged, changed) 
  gpMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
  
  
  } else {
    gpMap_new  <- gpMap
  }
  
  globalProductionAggr <- gpMap_new[, list(Value = sum(Value, na.rm = TRUE),
                                           flagObservationStatus = max(flagObservationStatus),
                                           flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                                        timePointYears,
                                                                        measuredElement,
                                                                        ics)]
  
  globalProductionAggr <- globalProductionAggr[!is.na(ics), ]
  
  t2 <- Sys.time()
  message(paste("GP, okay", t2-t1))
  
  return(globalProductionAggr)

}

CDBrecalc <- function(CDB, map_isscfc, new_map_isscfc, year = input$btn_year){
  
  t1 <- Sys.time()
  commodityDBIcs <- merge(CDB, map_isscfc, by = "measuredItemISSCFC")
  commodityDBIcs$measuredItemISSCFC <- as.character(commodityDBIcs$measuredItemISSCFC)
  
  old_map_isscfc <-  ReadDatatable('cdb_mapping', where = paste("country = '", unique(commodityDBIcs$geographicAreaM49_fi), "'", sep = ''))
  
  # If an updated has been done the the new datatable is used, 
  # otherwise if the CDB tab has not even been opened
  # the current SWS datatable is used
  
  if(nrow(new_map_isscfc) > 0){
    
    new_map_isscfc <- new_map_isscfc
    
  } else {
    
    new_map_isscfc <- old_map_isscfc
    
  }
  
  # Account for commodity deviation
  if(nrow(new_map_isscfc) > 0){
    new_map_isscfc[ end_year == 'LAST', end_year := year]
    
    newMappingCDB <- merge(commodityDBIcs, new_map_isscfc,
                           by.x = c('geographicAreaM49_fi', 'measuredElement','measuredItemISSCFC'),
                           by.y = c('country', 'element','isscfc'), all.x = TRUE)
    
    
    unchangedCDB <- newMappingCDB[is.na(from_code)]
    tochangeCDB <- newMappingCDB[!is.na(from_code)]
    
    tochangeCDB[ timePointYears < end_year & timePointYears > start_year & ratio == 1, ics := to_code]
    tochangeCDB[ timePointYears < end_year & timePointYears > start_year & ratio != 1, Value := Value * (1-as.numeric(ratio))]
    
    duplicateCDB <- tochangeCDB[!is.na(Value) & timePointYears < end_year & timePointYears > start_year & ratio != 1]
    duplicateCDB[ , c('ics', 'Value') := list(to_code, Value * as.numeric(ratio))]
    
    changedCDB <- rbind(tochangeCDB, duplicateCDB)
    
    cdbMap_new <- rbind(unchangedCDB, changedCDB[ics != '9999']) 
    cdbMap_new[ , c('from_code', 'to_code', 'start_year', 'end_year', 'ratio'):= NULL]
    # Sum by ICS, no commodities anymore
  
    } else {
    cdbMap_new <- commodityDBIcs
  }
  

  # Link table for special period ICS group changes
  link_table <- ReadDatatable("link_table")
  
  ## Checks on link table
  # quantity different from 100% allocated
  link_table[ , check := sum(percentage), by=c("geographic_area_m49","flow","start_year","end_year","from_code")]
  
  linkCorrespondence <- data.table(flow = c("PRD", "TRD", "TRD", "EXP", "IMP", "ALL", "ALL", "ALL"), 
                                   measuredElement = c("5510", "5910", "5610",  "5910", "5610", "5510", "5910", "5610"))
  
  link_table2 <- merge(link_table, linkCorrespondence, by = "flow", allow.cartesian = TRUE)
  
  link_table2$end_year <- ifelse(link_table2$end_year == "LAST", max(as.numeric(cdbMap_new$timePointYears)),
                                 link_table2$end_year)
  
  # Change ICS codes
  
  commodityDBLink <- merge(cdbMap_new, link_table2, 
                           by.x = c("geographicAreaM49_fi", "measuredElement", "ics"),
                           by.y = c("geographic_area_m49", "measuredElement", "from_code"), 
                           all.x = TRUE, allow.cartesian = TRUE)
  setkey(commodityDBLink)
  commodityDBLink <- unique(commodityDBLink)
  
  # Avoid NAs for periods
  commodityDBLink$start_year <- ifelse(is.na(commodityDBLink$start_year), "1900", commodityDBLink$start_year)
  commodityDBLink$end_year <- ifelse(is.na(commodityDBLink$end_year), "9999", commodityDBLink$end_year)
  
  commodityDBLink <- commodityDBLink[timePointYears >= start_year, ]
  commodityDBLink <- commodityDBLink[timePointYears <= end_year]
  
  # Change ICS for defined periods
  
  commodityDBLink[!is.na(to_code) & 
                    as.numeric(timePointYears) >= as.numeric(start_year) &
                    as.numeric(timePointYears) <= as.numeric(end_year), ics := to_code]
  
  commodityDBLink[!is.na(percentage) , Value := Value*percentage]
  
  # remove unnecessary dimensions
  commodityDBLink <- commodityDBLink[ , c("flow", "start_year", "end_year", "percentage", "to_code", "check") := NULL]
  
  # Some commodities are not imported for food porpuses (e.g. "ornamental fish").
  # Those flow are deviated to "other utilizations"
  
  otherUses <- ReadDatatable('other_uses')
  
  commodityDBotherUses <- merge(commodityDBLink, otherUses, 
                                by.x = c( "measuredItemISSCFC", "measuredElement", "ics"),
                                by.y = c("isscfc", "measured_element_orig", "ics"))
  
  commodityDBotherUses$measuredElement <- ifelse(is.na(commodityDBotherUses$measured_element_dest),
                                                 commodityDBotherUses$measuredElement,
                                                 commodityDBotherUses$measured_element_dest)
  
  commodityDBotherUses <- commodityDBotherUses[ , c("label", "measured_element_dest", "fias_code") := NULL]
  
  commodityDBdeviated <- rbind(commodityDBLink, commodityDBotherUses)
  
  
  commodityDBAggr <- commodityDBdeviated[ , list(Value = sum(Value, na.rm = TRUE),
                                        flagObservationStatus = max(flagObservationStatus),
                                        flagMethod = "s"),
                                 by = list(geographicAreaM49_fi,
                                           timePointYears,
                                           measuredElement,
                                           ics)]
  
  
  t2 <- Sys.time()
  message(paste("CDB, okay", t2-t1))
  return(commodityDBAggr)
}

SUAunbalCalc <- function(globalProductionAggr, commodityDBAggr){
t1 <- Sys.time()
  SUA <- rbind(globalProductionAggr, commodityDBAggr)
  setnames(SUA, "ics", "measuredItemFaostat_L2")
  
  SUA <- SUA[ , list(Value = sum(Value, na.rm = TRUE),
                     flagObservationStatus = max(flagObservationStatus),
                     flagMethod = "s"), by = list(geographicAreaM49_fi,
                                                  timePointYears,
                                                  measuredElement,
                                                  measuredItemFaostat_L2)]
  setnames(SUA, 'measuredElement', 'measuredElementSuaFbs')
  SUA <- SUA[!is.na(Value)]
  
  elementSignTable <- ReadDatatable('element_sign_table')
  setnames(elementSignTable, 'measured_element', 'measuredElementSuaFbs')
  
  # Now not considering food processing (as in plugin FP calculated later)
  SUAexpanded <- merge(SUA[measuredElementSuaFbs != "5023"], 
                       elementSignTable[ , .(measuredElementSuaFbs, sign)], 
                       by = "measuredElementSuaFbs", all.x = TRUE)
  
  SUAexpanded[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  initialUnbal <- SUAexpanded[availability < 0]
  t2 <- Sys.time()
  message(paste("SUA unbal, okay", t2-t1))
 return(list(SUAunbal = SUA,
             initialUnbal = initialUnbal))
}

SUAbalCalc <- function(SUA, eR, use){
  t1 <- Sys.time()
  SUAno131 <- SUA[ measuredElementSuaFbs != "5023"]
  SUA131 <- SUA[ measuredElementSuaFbs == "5023"]
  
  elementSignTable <- ReadDatatable('element_sign_table')
  setnames(elementSignTable, 'measured_element', 'measuredElementSuaFbs')
  
  # Now only considering production, import and export to compute availability
  # then after calculations we compare official food processing data with calculations

  SUAexpanded <- merge(SUAno131, elementSignTable[ , .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)

  SUAexpanded[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  # Check no negative primary availability. 
  # Now many production data are missing in the commodity DB in SWS so 
  # there are negative primary availabilities
  map_asfis <- ReadDatatable('map_asfis')
  setnames(map_asfis, c("asfis", "ics"), c("fisheriesAsfis", "measuredItemFaostat_L2"))
  primary <- unique(map_asfis$measuredItemFaostat_L2)
  primaryneg <- SUAexpanded[availability < 0 & measuredItemFaostat_L2 %in% primary]
  
  if(nrow(primaryneg) > 0){
    countriesneg <- unique(primaryneg$geographicAreaM49_fi)
    measuredItemFaostat_L2neg <- unique(primaryneg$measuredItemFaostat_L2)
    msg2email4 <- paste0('There are negative primary availabilities. Check ICS group(s): ',
                         paste0(measuredItemFaostat_L2neg, collapse = ", "))
    message(msg2email4)
  } else {
    
    msg2email4 <- ''
  }
  
  rou <- copy(primaryneg)
  rou[ , c('measuredElementSuaFbs', 'Value') := list('5166', - availability)]
  
  SUAexpanded <- rbind(SUAexpanded, rou)
  
  secondaryneg <- SUAexpanded[availability < 0 & !measuredItemFaostat_L2 %in% primary]
  setkey(secondaryneg)
  secondaryneg <- secondaryneg[!duplicated(secondaryneg)]
  
  # Delete old imbalances stored
  imbalance_store <- ReadDatatable('imbalance_tab', readOnly = FALSE)
  if(nrow(imbalance_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ]) > 0){
    changeset <- Changeset('imbalance_tab')
    AddDeletions(changeset, imbalance_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ])
    Finalise(changeset)
  }
  
  #Consider both primary and secondary imbalance
  imbalanceCompliant <- rbind(secondaryneg, primaryneg)
  imbalanceCompliant <- imbalanceCompliant[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                timePointYears, availability)]
  setkey(imbalanceCompliant)
  imbalanceCompliant <- unique(imbalanceCompliant)
  
  setnames(imbalanceCompliant,
           c('geographicAreaM49_fi', 'timePointYears',
             'measuredItemFaostat_L2'),
           c('geographicaream49_fi', 'timepointyears',
             'measureditemfaostat_l2'))
  
  changeset <- Changeset('imbalance_tab')
  AddInsertions(changeset, imbalanceCompliant)
  Finalise(changeset)
  
  if(nrow(secondaryneg) > 0){
    countriessecneg <- unique(secondaryneg$geographicAreaM49_fi)
    measuredItemFaostat_L2secneg <- unique(secondaryneg$measuredItemFaostat_L2)
    msg2email5 <- paste0('There are still negative secondary availabilities for ICS group(s): ',
                         paste0(measuredItemFaostat_L2secneg, collapse = ", "))
    message(msg2email5)
  } else {
    
    msg2email5 <- ''
  }
  
  mealCodes <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')
  
  if(any(secondaryneg$measuredItemFaostat_L2 %in% mealCodes)){
    
    mealsUnbal <- secondaryneg[measuredItemFaostat_L2 %in% mealCodes]
    message('Unbalance for meal products!')
    secondaryneg <- secondaryneg[!measuredItemFaostat_L2 %in% mealCodes]
    
  }
  
  if(nrow(secondaryneg) > 0){
    
    # Make sure all production (5510) values have been imputed
    
    icsneg <- unique(secondaryneg$measuredItemFaostat_L2)
    # ics2add <- icsneg[!(icsneg %in% unique(secondaryneg[measuredElementSuaFbs == "5510"]$measuredItemFaostat_L2)) ]
    setkey(secondaryneg, geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability)
    prod2add <- unique(secondaryneg[ , .(geographicAreaM49_fi, timePointYears,  measuredItemFaostat_L2, availability) ])
    
    # add production element with NA values and flags then estimate as in Francesca code with estimation flags
    prod2add[ , ':=' (measuredElementSuaFbs = '5510', Value = - availability,
                      flagObservationStatus = as.factor('I'), flagMethod = 'i', sign = 1)]
    
    # SUA with all production values
    SUAwithProdupd <- merge(secondaryneg, prod2add, by = c('geographicAreaM49_fi',
                                                           'timePointYears',
                                                           'measuredItemFaostat_L2',
                                                           'availability',
                                                           'measuredElementSuaFbs'),
                            suffixes = c('', '_added'), all = TRUE)
    SUAwithProdupd$sign_added <- as.integer(SUAwithProdupd$sign_added)
    
    
    SUAwithProdupd[measuredElementSuaFbs == '5510' , c("Value", "sign", "flagObservationStatus",
                                                       "flagMethod") := list(ifelse(is.na(Value), Value_added, 
                                                                                    Value+Value_added),
                                                                             sign_added,
                                                                             flagObservationStatus_added,
                                                                             flagMethod_added)]
    
    # Putting together values with negative and positive availability which had been separated before
    SUAwithProd <- rbind(SUAwithProdupd[ , .(geographicAreaM49_fi, timePointYears,
                                             measuredItemFaostat_L2, availability,
                                             measuredElementSuaFbs, Value,
                                             flagObservationStatus, flagMethod)],
                         SUAexpanded[availability >= 0, .(geographicAreaM49_fi, timePointYears,
                                                          measuredItemFaostat_L2, availability,
                                                          measuredElementSuaFbs, Value,
                                                          flagObservationStatus, flagMethod)])
  
    
    } else {
    
    SUAwithProd <- SUAexpanded
    SUAwithProd[ , sign := NULL ]
    
  }

  tree <- ReadDatatable('fi_commodity_tree')

  yearVals <- as.character(min(unique(as.numeric(as.character(SUAwithProd$timePointYears)))):max(unique(as.numeric(as.character(SUAwithProd$timePointYears)))))
  SUAvalEr <- SUAwithProd[measuredElementSuaFbs == '5423' & timePointYears != max(yearVals)]
  
  message("fi_SUA-FBS: Calculating extraction rates")
  SUAwithEr <- eRcomputation(data = SUAwithProd, tree = tree[parent %in% primary ], primary = primary,
                             oldEr = SUAvalEr, years = yearVals)
  
  # If updating extraction rates
  if(use == 1){
SUAnewEr <- merge(SUAwithEr, eR, by = c('geographicAreaM49_fi',
                            'measuredItemFaostat_L2',
                            'measuredElementSuaFbs',
                            'timePointYears'), all.x = TRUE,
                  suffixes = c('','New'))
  
SUAnewEr[measuredElementSuaFbs == '5423', Value := ifelse(!is.na(ValueNew) & Value != ValueNew, ValueNew, Value)]
SUAnewEr[ , c('ValueNew', 'flagObservationStatusNew', 'flagMethodNew'):=NULL]
  } 
  else {
    SUAnewEr <- copy(SUAwithEr)
    
}
  
  message("fi_SUA-FBS: Calculating input element")
  SUAinput <- inputComputation(data = SUAnewEr, primary = primary, use)
  
  newTree <- merge(tree, unique(SUAinput[ measuredElementSuaFbs == '5423' & !is.na(Value),
                                          .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, Value)]), 
                   by.x = 'child', by.y = 'measuredItemFaostat_L2', all.x = TRUE, allow.cartesian = TRUE)
  newTree[ , extraction_rate := Value ]
  newTree[ , Value:= NULL]
  
  #--Food processing ----
  message("fi_SUA-FBS: Calculating food processing")
  FPdata_all <- foodProcessingComputation(SUAinput = SUAinput, treeNewER = newTree, primary = primary)
  FPdata <- FPdata_all$result
  FPdata <- FPdata[Value != 0]
  FPproblems <- FPdata_all$problems
  FPdata <- FPdata[ , availability := NULL ]
  FPdata[ , c("flagObservationStatus", "flagMethod") := list("E", "i")]
  
  SUAinput[ , availability := NULL]
  
  # If processing value changed in the shiny, i.e. flagged as (E,f)
  # then value flagged (E,f) prevail on the computed one
  FPdataupd <- merge(SUA131, FPdata, by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2',
                                            'timePointYears', 'measuredElementSuaFbs'),
                     all = TRUE, suffixes = c('', 'Recalc'))
  FPdataupd[is.na(Value) | flagMethod != 'f', c('Value',
                                                'flagObservationStatus',
                                                'flagMethod') := list(ValueRecalc,
                                                                      flagObservationStatusRecalc,
                                                                      flagMethodRecalc)]
  FPdataupd[ , c('ValueRecalc',
                 'flagObservationStatusRecalc',
                 'flagMethodRecalc') := NULL]
  
  
  SUAunbal <- rbind(SUAinput[!is.na(Value), ], FPdataupd[!is.na(Value), ])
  SUAunbal$flagObservationStatus <- as.character(SUAunbal$flagObservationStatus)

  
  # R_SWS_SHARE_PATH <- Sys.getenv("R_SWS_SHARE_PATH")
  # 
  # saveRDS(FPproblems,
  #         file.path(R_SWS_SHARE_PATH, "taglionic", "FPfisheries", "FoodProcessingFeedback.rds")
  # )
  
  #-- Balancing ----
  
  SUAunbal <-  merge(SUAunbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)
  SUAunbal$timePointYears <- as.character(SUAunbal$timePointYears)
  SUAunbal <- SUAunbal[measuredElementSuaFbs != '5037']
  if(any(is.na(SUAunbal$sign))){
    message(paste(SUAunbal[is.na(SUAunbal$sign), ], 
                  ' is an element in the SUA not included in the availability calculation.'))
  }
  
  SUAunbal[, availability := sum(Value * sign, na.rm = TRUE), 
           by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  balancingElements <- ReadDatatable('balancing_elements')
  setnames(balancingElements, names(balancingElements), c("geographicAreaM49_fi", 
                                                          "measuredItemFaostat_L2",
                                                          "measuredElementSuaFbs",
                                                          "start_year", "end_year", "share"))
  
  balancingElements[ end_year == "LAST"]$end_year <- as.character(max(unique(as.numeric(SUAunbal$timePointYears))))
  
  balancingValues <- unique(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , measuredItemFaostat_L2, availability) ])
  
  balancing <- merge(balancingElements, 
                     balancingValues, by = c("geographicAreaM49_fi","measuredItemFaostat_L2"),
                     all.y = TRUE)
  setnames(balancing, c("availability"), c("Value"))
  
  if(any(is.na(balancing$measuredElementSuaFbs)) & any(balancing[is.na(measuredElementSuaFbs)]$availability != 0)){
    message('Balancing elements missing!')
    message(balancing[is.na(measuredElementSuaFbs) & availability != 0])
  }
  
  balancing2merge <- balancing[ as.numeric(timePointYears) >= as.numeric(start_year) & as.numeric(timePointYears) <= as.numeric(end_year), Value := Value*share]
  balancing2merge[ , c('start_year', 'end_year', 'share') := NULL]
  balancing2merge[ , c('flagObservationStatus', 'flagMethod') := list('E','b')]
  
  # Balancing cannot be negative
  balancingproblems <- balancing2merge[round(Value,3) < 0,]
  
  balancingproblems_store <- ReadDatatable('balancing_problems_tab', readOnly = FALSE)
  if(nrow(balancingproblems_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ]) > 0){
    changeset <- Changeset('balancing_problems_tab')
    AddDeletions(changeset, balancingproblems_store[ geographicaream49_fi %in% unique(secondaryneg$geographicAreaM49_fi), ])
    Finalise(changeset)
  }
  # Add new imbalances
  
  if(length(FPproblems$NotCovered) > 0){
    toupload <- copy(FPproblems$NotCovered)
    toupload[ , measuredElementSuaFbs := '5023']
    setnames(toupload, c('parent_primary', 'UncoveredQuantity'),
             c('measuredItemFaostat_L2', 'Value'))
    balancingproblemsCompliant <- rbind(toupload, balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                                         timePointYears, measuredElementSuaFbs, Value)])
  } else {
    balancingproblemsCompliant <- rbind(balancingproblems[ , .(geographicAreaM49_fi, measuredItemFaostat_L2,
                                                               timePointYears, measuredElementSuaFbs, Value)])
  }
  
  if(nrow(balancingproblemsCompliant) > 0){
  setkey(balancingproblemsCompliant)
  balancingproblemsCompliant <- unique(balancingproblemsCompliant)
  
  setnames(balancingproblemsCompliant,
           c('geographicAreaM49_fi', 'timePointYears',
             'measuredItemFaostat_L2', 'measuredElementSuaFbs', 'Value'),
           c('geographicaream49_fi', 'timepointyears',
             'measureditemfaostat_l2', 'measuredelementsuafbs', 'value'))
  
  changeset <- Changeset('balancing_problems_tab')
  AddInsertions(changeset, balancingproblemsCompliant)
  Finalise(changeset)
  }
  # SUAbal1 <- rbind(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
  #                               measuredItemFaostat_L2, measuredElementSuaFbs, 
  #                               Value, flagObservationStatus, flagMethod)], balancing2merge)
  # # Sum if there is a balancing elements that was already present there
  SUAbal <- merge(SUAunbal[ , .(geographicAreaM49_fi, timePointYears , 
                                measuredItemFaostat_L2, measuredElementSuaFbs, 
                                Value, flagObservationStatus, flagMethod)], balancing2merge,
                  by = c('geographicAreaM49_fi', 'timePointYears', 
                         'measuredItemFaostat_L2', 'measuredElementSuaFbs'),
                  suffixes = c('','Bal'), 
                  all = TRUE)
  
  SUAbal[is.na(ValueBal), ValueBal := 0 ]
  SUAbal[is.na(Value), Value := 0]
  SUAbal[ , Value := Value + ValueBal ]
  SUAbal$flagObservationStatus <- as.character(SUAbal$flagObservationStatus)
  SUAbal[is.na(flagObservationStatus) , flagObservationStatus := 'E']
  SUAbal[is.na(flagMethod) , flagMethod := 'b']
  SUAbal <- SUAbal[ , c('ValueBal', 
                        'flagObservationStatusBal',
                        'flagMethodBal') := NULL]
  
  SUAbalAvail <- merge(SUAbal, elementSignTable[, .(measuredElementSuaFbs, sign)], by = "measuredElementSuaFbs", all.x = TRUE)
  
  SUAbalAvail[, availability := sum(Value * sign, na.rm = TRUE), 
              by = list(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)]
  
  if(any(round(SUAbalAvail$availability) != 0)){
    message("fi_SUA-FBS: Balancing was not successful for some products.")
    msg2email7 <- paste0('Problem with products:', 
                         paste0(unique(SUAbalAvail[round(availability) != 0, ]$measuredItemFaostat_L2), collapse = ", "))
    
  } else {
    msg2email7 <-  ''
  }
  
  SUAbalAvail[, c("sign", "availability"):=NULL]
  t2 <- Sys.time()
  message(paste("SUAbal, okay", t2-t1))
  
  list( NegAv = secondaryneg,
        FPproblems = FPproblems,
        SUA = SUAbalAvail,
        msg = list(msg1 = msg2email4, msg2 = msg2email5, msg3 = msg2email7))
}

SUAnutrCalc <- function(SUAbalAvail, popSWS){
t1 <- Sys.time()
  ## Add NutrientFactors
  nutrientFactors <- ReadDatatable("fishery_nutrient")
  nutrientFactors$calories <- as.numeric(nutrientFactors$calories)
  nutrientFactors$proteins <- as.numeric(nutrientFactors$proteins)
  nutrientFactors$fats <- as.numeric(nutrientFactors$fats)
  nutrientFactors[is.na(proteins), proteins := 0]
  
  SUA_with_nutrient <- merge(SUAbalAvail, nutrientFactors, by.x = "measuredItemFaostat_L2", by.y = "ics", all.x = TRUE)
  
  SUA_with_nutrient[measuredElementSuaFbs=="5141", calories:=Value*calories/100]
  SUA_with_nutrient[measuredElementSuaFbs=="5141", proteins:=Value*proteins/100]
  SUA_with_nutrient[measuredElementSuaFbs=="5141", fats:=Value*fats/100]
  SUA_with_nutrient[measuredElementSuaFbs!="5141",`:=`(c("calories", "proteins", "fats"),list(0,0,0) )]
  
  # Get "calories", "proteins" and "fats" and make them in the dataset format
  SUAnutrients <-  melt.data.table(SUA_with_nutrient[measuredElementSuaFbs=="5141", ],
                                   id.vars = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'timePointYears'),
                                   measure.vars = c('calories', 'proteins','fats'),
                                   variable.name = 'measuredElementSuaFbs', value.name = 'Value')
  SUAnutrients$measuredElementSuaFbs <- as.character(SUAnutrients$measuredElementSuaFbs)
  SUAnutrients$measuredElementSuaFbs <- ifelse(SUAnutrients$measuredElementSuaFbs == 'calories', '261',
                                               ifelse(SUAnutrients$measuredElementSuaFbs == 'proteins', '271',
                                                      ifelse(SUAnutrients$measuredElementSuaFbs == 'fats', '281', SUAnutrients$measuredElementSuaFbs)))
  
  SUAnutrients[ , c('flagObservationStatus', 'flagMethod') := list('E','i')]
  SUAnutrients <- unique(SUAnutrients)
  food <- SUA_with_nutrient[measuredElementSuaFbs=="5141", .(measuredItemFaostat_L2, measuredElementSuaFbs,
                                                             geographicAreaM49_fi, timePointYears, Value,
                                                             flagObservationStatus, flagMethod)]
  
  SUAnutrients <- rbind(SUAnutrients, food)
 
  SUAnutrientCapita <- merge(SUAnutrients, popSWS, by=c("geographicAreaM49_fi","timePointYears"), suffixes = c("","_pop"))  
  SUAnutrientCapita[measuredElementSuaFbs !="5141" , Value := (Value*1000)/(Value_pop*365)]
  SUAnutrientCapita[measuredElementSuaFbs =="5141" , Value := Value/Value_pop]
  SUAnutrientCapita <- SUAnutrientCapita[ , .(geographicAreaM49_fi,
                                              timePointYears,
                                              measuredItemFaostat_L2,
                                              measuredElementSuaFbs,
                                              Value, flagObservationStatus,
                                              flagMethod)]
  
  SUAnutrientCapita[measuredElementSuaFbs=="261",measuredElementSuaFbs:="264"]
  SUAnutrientCapita[measuredElementSuaFbs=="281",measuredElementSuaFbs:="284"]
  SUAnutrientCapita[measuredElementSuaFbs=="271",measuredElementSuaFbs:="274"]
  SUAnutrientCapita[measuredElementSuaFbs=="5141",measuredElementSuaFbs:="5037"]
  
  SUA_with_nutrient[ , c('calories', 'proteins','fats') := NULL] 
  
  
  # bind SUA with "calories", "proteins" and "fats" elements
  SUAallNutr <- rbind(SUAnutrients[measuredElementSuaFbs!="5141"], SUAnutrientCapita)
  SUANoPop <- rbind(SUA_with_nutrient, SUAallNutr)
  Pop2include <- merge(unique(SUANoPop[ , .(measuredItemFaostat_L2,
                                            geographicAreaM49_fi,
                                            timePointYears)]), popSWS, by = c('geographicAreaM49_fi', 
                                                                              'timePointYears'))
  
  SUA2save <- rbind(SUANoPop[measuredElementSuaFbs != '5038'], Pop2include)
  
  t2 <- Sys.time()
  message(paste("SUAnut, okay", t2-t1))
  
  return(SUA2save)
}

FBScalc <- function(SUA2save, popSWS){
t1 <- Sys.time()
  # get all conversion factors (or extration rates) from commodity tree
  message('Get commodity tree')
  tree <- ReadDatatable('fi_commodity_tree')
  
  primary <- unique(tree[!parent %in% child]$parent)
  
  extrRates <- unique(SUA2save[ measuredElementSuaFbs == '5423', .(measuredItemFaostat_L2, geographicAreaM49_fi, timePointYears, Value)])
  
  compareEr <- merge(unique(tree[parent %in% primary , .(child, extraction_rate)]), extrRates, by.x = 'child', by.y = 'measuredItemFaostat_L2', all.y = TRUE)
  
  compareEr[is.na(Value), Value := extraction_rate]
  
  updatedEr <- compareEr[ , .(child, Value, geographicAreaM49_fi, timePointYears)]
  
  updatedtree <- merge(unique(tree[parent %in% primary , .(parent, child, weight)]), updatedEr, by= 'child', all.y = TRUE, allow.cartesian = TRUE)
  
  # primary <- unique(updatedtree[!parent %in% child]$parent)
  setkey(updatedtree)
  convFact <- unique(updatedtree[weight == TRUE & !is.na(Value) , .(geographicAreaM49_fi, timePointYears, parent, child, Value)])
  
  # add primary ICS with Er equal 1
  primaryTree1 <- data.table(geographicAreaM49_fi = rep(unique(convFact$geographicAreaM49_fi), each = length(unique(convFact$timePointYears))), 
                             timePointYears = rep(unique(convFact$timePointYears), length(unique(convFact$geographicAreaM49_fi))) )
  
  primaryTree2 <- data.table(geographicAreaM49_fi = rep(unique(convFact$geographicAreaM49_fi), each = length(unique(primary))), parent = primary)
  
  primaryTree <- merge(primaryTree1, primaryTree2, by = 'geographicAreaM49_fi', allow.cartesian = TRUE)  
  primaryTree[ , `:=` (child= parent, Value = 1)]
  
  convFact <- rbind(convFact, primaryTree)
  setnames(convFact, new = 'extraction_rate', old = 'Value')
  # SUA with standardized element, no zero weight elements
  
  SUAstand_prep <- merge(SUA2save, convFact, by.x = c('geographicAreaM49_fi', 'timePointYears', 'measuredItemFaostat_L2'),
                         by.y = c('geographicAreaM49_fi', 'timePointYears', 'child'))
  SUAstand_prep <- SUAstand_prep[!is.na(Value)]
  setkey(SUAstand_prep)
  SUAstand_prep <- unique(SUAstand_prep)
  # Standardised value is Value/eR except from input values which are already in primary equivalent
  SUAstand_prep[! measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value/extraction_rate]
  SUAstand_prep[measuredElementSuaFbs %in% c('5302', '261', '264', '271', '274', '281', '284', '511'), Value_stand := Value]
  
  # take only all primary elements and for secondary exclude production and input
  SUAstand <-  rbind(SUAstand_prep[measuredItemFaostat_L2 %in% primary, ], 
                     SUAstand_prep[!measuredElementSuaFbs %in% c('5510', '5302') & !measuredItemFaostat_L2 %in% primary, ])
  
  
  
  ##-- FAOSTAT ----
  # Aggregate SUA by parent meals included for FAOSTAT
  
  SUAstandAggr0 <- SUAstand[ , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                 flagObservationStatus, flagMethod, parent, Value_stand)]
  
  
  
  SUAstandAggr0$flagObservationStatus <- factor(SUAstandAggr0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
  
  SUAstandAggr1 <- SUAstandAggr0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                                                                       flagObservationStatusAggr = max(flagObservationStatus),
                                                                       flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                                                     "timePointYears", "parent")]
  
  setnames(SUAstandAggr1, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
           c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))
  Pop2SUAaggr <- SUAstandAggr0[measuredElementSuaFbs == '511']
  setnames(Pop2SUAaggr, c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
  SUAstandAggr <- rbind(SUAstandAggr1, Pop2SUAaggr)
  
  IcsGroups <- unique(SUAstandAggr[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
  
  faostat_pop2merge <- merge(IcsGroups, popSWS, by=c("geographicAreaM49_fi","timePointYears"), all = TRUE) 
  
  faostatfbsPOP <- rbind(SUAstandAggr, faostat_pop2merge)
  
  # mapp for FBS groups
  sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = c("1501", "1514", "1527", "1540", "1553", "1562", "1570", "1587"),
                                fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))
  
  fbsFaostatL1faostat <- merge(faostatfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
  fbsFaostatL1faostat[ , measuredItemFaostat_L2 := NULL]
  setnames(fbsFaostatL1faostat, "fbs", "measuredItemFaostat_L2")
  
  #-- FAOSTAT FBS standardization ----
  
  message("Starting Faostat standardization")
  faostatGroups <- data.table(measuredElementSuaFbs = c("5510", "5610", "5910", "5912", "5520",
                                                        "5525", "5166", "5071", "5016", "5141", "5038", "264",
                                                        "511"),
                              faostat = c('Production', 'Imports', 'Exports', 'Exports', 'Feed', 'Bait',
                                          'Other net uses', 'Stock Variation', 'Loss',
                                          'Total food consumption', 'Food quantity/day/capita (kg)',
                                          'Food consumption (kcal)', 'Population'),
                              measuredElementFaostat = c("5510", "5610", "5910", "5910", "5520",
                                                         "5525", "5153", "5071", "5016", "5141", "294", 
                                                         "664", "511"))
  
  FBSfaostat0 <- merge(fbsFaostatL1faostat[!measuredElementSuaFbs %in% c('271', '281')],
                       faostatGroups, by = "measuredElementSuaFbs")
  FBSfaostat1 <- FBSfaostat0[measuredElementFaostat != '511' , list(Value = sum(Value, na.rm = TRUE),
                                                                    flagObservationStatus = max(flagObservationStatus),
                                                                    flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                                                              "timePointYears", "measuredItemFaostat_L2",
                                                                                              "faostat", "measuredElementFaostat")]
  
  FBSfaostat <- rbind(FBSfaostat1, FBSfaostat0[measuredElementFaostat == '511', -'measuredElementSuaFbs', with = FALSE])
  setnames(FBSfaostat, c("faostat", "measuredElementFaostat"), c("element_description", "measuredElementSuaFbs"))

  faostatFBS2save <- FBSfaostat[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2,
                                     measuredElementSuaFbs, Value, flagObservationStatus, flagMethod)]
  
  #-- FIAS ----
  # Aggregate SUA by parent code take away meals
  mealCodes <- c('1508', '1521', '1534', '1547', '1558', '1566', '1575', '1589')
  
  # SUA with elements and groups to aggregates
  SUAstandAggrFias0 <- SUAstand[ !measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                            flagObservationStatus, flagMethod, parent, Value_stand)]
  
  # take only input element 5302
  mealsInput0 <- SUA2save[measuredElementSuaFbs == '5302' & measuredItemFaostat_L2 %in% mealCodes , .(measuredElementSuaFbs, geographicAreaM49_fi, timePointYears,
                                                                                                      flagObservationStatus, flagMethod, 
                                                                                                      measuredItemFaostat_L2, Value)]
  
  mealsInput <- merge(mealsInput0, unique(tree[parent %in% primary, .(parent, child)]), 
                      by.x = 'measuredItemFaostat_L2', by.y = 'child', all.x = TRUE)
  
  mealsInput[ , measuredItemFaostat_L2 := NULL]
  setnames(mealsInput, c("parent"), c("measuredItemFaostat_L2"))
  
  SUAstandAggrFias0$flagObservationStatus <- factor(SUAstandAggrFias0$flagObservationStatus, levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), ordered = TRUE)
  
  SUAstandAggrFias1 <- SUAstandAggrFias0[measuredElementSuaFbs != '511' , list(Value = sum(Value_stand, na.rm = TRUE),
                                                                               flagObservationStatusAggr = max(flagObservationStatus),
                                                                               flagMethodAggr = 's'), by = c("measuredElementSuaFbs", "geographicAreaM49_fi",
                                                                                                             "timePointYears", "parent")]
  
  setnames(SUAstandAggrFias1, c('parent', 'flagObservationStatusAggr', 'flagMethodAggr'), 
           c('measuredItemFaostat_L2', 'flagObservationStatus', 'flagMethod'))
  
  Pop2SUAaggFias <- SUAstandAggrFias0[measuredElementSuaFbs == '511']
  setnames(Pop2SUAaggFias,c('Value_stand', 'parent'), c('Value', 'measuredItemFaostat_L2'))
  SUAstandAggrFias <- rbind(SUAstandAggrFias1, Pop2SUAaggFias)
  
  
  fiasFbsTot <- rbind(SUAstandAggrFias, mealsInput)
  IcsGroups <- unique(fiasFbsTot[ , .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2)])
  
  # Introduce population data
  pop2merge <- merge(popSWS, IcsGroups, by = c("geographicAreaM49_fi", "timePointYears"), all = TRUE)
  
  fiasfbsPOP <- rbind(fiasFbsTot, pop2merge)
  
  # mapp for FBS groups
  sua_fbs_mapping <- data.table(measuredItemFaostat_L2 = c("1501", "1514", "1527", "1540", "1553", "1562", "1570", "1587"),
                                fbs = c(  "10",  "20",  "30",  "40",  "50",  "60",  "70",  "90"))
  
  fbsFaostatL1 <- merge(fiasfbsPOP, sua_fbs_mapping, by = "measuredItemFaostat_L2")
  fbsFaostatL1[ , measuredItemFaostat_L2 := NULL]
  setnames(fbsFaostatL1, "fbs", "measuredItemFaostat_L2")
  
  #-- FIAS FBS standardization ---- 
  message("fi_SUA-FBS: Starting Fias standardization")
  fiasGroups <- data.table(measuredElementSuaFbs = c("5510", "5302", "5610", "5910", "5912", "5520", 
                                                     "5525", "5166", "5016", "5071", "5141", "511", "5038", "264",
                                                     "274", "284"),
                           fias = c('Production', 'Meals input', 'Imports', 'Exports', 'Exports', 'Other non-food uses', 
                                    'Other non-food uses', 'Other non-food uses', 'Other non-food uses', 'Stock variations',
                                    'Total food supply', 'Population', 'Per capita food',
                                    'Calories', 'Proteins', 'Fats'),
                           measuredElementFias = c("5510", "5302", "5610", "5910", "5910", "5153", 
                                                   "5153", "5153", "5153", "5071", "5141", "511", "5038", # or "5036" instead of 261 
                                                   "264", "274", "284"))
  
  # FBS
  FBSfias0 <- merge(fbsFaostatL1[!measuredElementSuaFbs %in% c('261', '271', '281')], 
                    fiasGroups, by = "measuredElementSuaFbs")
  FBSfias1 <- FBSfias0[measuredElementFias != '511' , list(Value = sum(Value, na.rm = TRUE),
                                                           flagObservationStatus = max(flagObservationStatus),
                                                           flagMethod = 's'), by = c("geographicAreaM49_fi",
                                                                                     "timePointYears", "measuredItemFaostat_L2",
                                                                                     "fias", "measuredElementFias")]
  
  FBSfias <- rbind(FBSfias1, FBSfias0[measuredElementFias == '511', -'measuredElementSuaFbs', with = FALSE])
  fiasFBS2save <-  FBSfias[, .(geographicAreaM49_fi, timePointYears, measuredItemFaostat_L2, 
                               measuredElementFias, Value, flagObservationStatus, flagMethod)]
  
  setnames(fiasFBS2save, "measuredElementFias", "measuredElementSuaFbs")
  
  t2 <- Sys.time()
  message(paste("FBS, okay", t2-t1))
  
  #-- To update ----
  
  return(list(faostat = faostatFBS2save, fias =  fiasFBS2save))
}
