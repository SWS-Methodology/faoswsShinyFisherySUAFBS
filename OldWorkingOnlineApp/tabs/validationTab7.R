# Seventh tab, data validation
recalc_value <- reactiveValues(SUAinit = data.table(),
                               SUAmodtab = data.table())

workaround <- reactiveValues(V = 0)

#-- SUA reactive ----
suaTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)

  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs_tab7]$code)
  
  if('All' %in% input$btn_ics_prod_tab7){
    sel_ics_prod <- as.character(l2l1[code_l1 %in% sel_group_fbs ]$code_l2)
  } else {
    sel_ics_prod <- as.character(groups_input[label %in% input$btn_ics_prod_tab7]$code)
  }
  
  sel_element_sua <- as.character(sua_element_input[ label %in% input$btn_sua_elem_tab7]$code)
  
  if(nrow(recalc_value$SUAmodtab) == 0){
  
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
  #                      token = tokenSuaB)
  #   
  # }
  
  R_SWS_SHARE_PATH = "Z:"
  SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
  GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                     token = tokenSuaB)

  KeySUA <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = sel_element_sua), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_sua_balanced","measuredElementSuaFbs" )[,code])
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = sel_ics_prod),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'SUA balanced data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUA <- GetData(KeySUA)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })

  recalc_value$SUAinit <- SUA
  
  } else {
    
    SUA0 <- recalc_value$SUAmodtab
    SUA <- SUA0[geographicAreaM49_fi == sel_country &
                 measuredElementSuaFbs %in% sel_element_sua &
                 measuredItemFaostat_L2 %in% sel_ics_prod &
                 timePointYears %in% sel_years, ]
    
  }
  tab2show <- merge(SUA, l2l1[ , .(code_l1, code_l2)], 
                    by.x = 'measuredItemFaostat_L2', by.y = 'code_l2')
  
  if(nrow(new_extr_rate$eR) > 0){
  updER <- new_extr_rate$eR[measuredItemFaostat_L2 %in% sel_ics_prod & timePointYears %in% sel_years]
  
  tab2show[measuredElementSuaFbs == '5423', c("Value", 
                                              "flagMethod"):= list(updER$Value, updER$flagMethod)]
  }
 
  return(list(sua2show = tab2show))
})


#-- SUA renderRHandsontable ----

output$sua_tab7 <-  renderRHandsontable({
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  sua_tab <- copy(suaTab_reac()$sua2show)

  setnames(sua_tab, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                      'measuredElementSuaFbs', 'timePointYears', 
                      'flagObservationStatus', 'flagMethod', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'F1', 'F2', 'FBSgroup'))
  
  sua_tab <- merge(sua_tab, SUAelements[ , .(code, idx)], by.x = 'Element', 
                         by.y = 'code', all.x = TRUE)
  
  sua_tab <- sua_tab[order(as.numeric(idx)),]
  # sua_tab <- sua_tab[ , idx := NULL]

  sua_tab_out <- dcast(sua_tab, Country + FBSgroup + ICSprod + idx + Element ~ Year, value.var = c("Value"))
  rhandsontable(sua_tab_out[ , idx := NULL], rowHeaders = NULL, width = 'auto', height = 'auto') %>%
    hot_cols(fixedColumnsLeft = 4) # number of columns to freeze # columnSorting = TRUE, BUG MALEDETTO
})

#-- Availability table ---- 
output$textAv <- renderText({
  req(input$btn_country, input$btn_year, input$btn_start_year, input$btn_group_fbs_tab7,
      input$btn_ics_prod_tab7)

  "Availability table:"
})

output$availability <- renderRHandsontable({
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  if(is.null(input$sua_tab7)) return(NULL)

  tab_updated <- rhandsontable::hot_to_r(input$sua_tab7)

  tab2calc <- melt(tab_updated, id.vars = c('Country', 'ICSprod', 'Element', 'FBSgroup'),
       measure.vars = names(tab_updated)[!names(tab_updated) %in% c('Country', 'ICSprod', 'Element', 'FBSgroup', 'sign')],
       variable.name = 'Year', value.name = 'Value')
   
  elementSignTable <- ReadDatatable('element_sign_table')
  
    # Now only considering production, import and export to compute availability
    # then after calculations we compare official food processing data with calculations
    # Actually not expanded
    sua_avail <- merge(tab2calc, elementSignTable[ , .(measured_element, sign)], 
                       by.x = 'Element', by.y = "measured_element", all.x = TRUE)
   
    sua_avail <- sua_avail[, availability := round(sum(Value * sign, na.rm = TRUE), 3), 
                by = list(Country, FBSgroup, ICSprod, Year)]
    sua_avail[ , c('Element', 'Value', 'sign') := NULL]
    setkey(sua_avail)
    avail2show <- unique(sua_avail)
    avail2show <- dcast(avail2show, Country + FBSgroup + ICSprod  ~ Year, value.var = c("availability"))
  rhandsontable(avail2show, rowHeaders = NULL, width = 'auto', height = 'auto') # %>%
  #   hot_col('availability', renderer = 'green')
  
})

#-- FP Feedback ----

feedback <- reactiveValues(FP = readRDS('FoodProcessingFeedback.rds'))

# output$FPtxt1 <- renderText({ 
#   req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7)
#   msg1 <- feedback$FP[[1]]$msg
#   paste("Primary availability problems:", 
#         msg1, sep = "/n")
# 
#   })

output$FPtab1 <- DT::renderDataTable( server = FALSE, {
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)

  tab1 <- feedback$FP$primary$tab
  sel_country <- country_input[country_input$label == input$btn_country, code]
  if(!is.null(tab1) |nrow(tab1) > 0){
  tab1$Value <- round(tab1$Value,3)
  DT::datatable(tab1[geographicAreaM49_fi == sel_country], extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
  } else {
    
    DT::datatable(data.table(), extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(pageLength = 25,
                                                   dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf')))
    
  }
 
})

# output$FPtxt2 <- renderText({ 
#   req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7)
#   msg2 <- feedback$FP[[2]]$msg
#   paste("Total secondary availability problems:", 
#         msg2, sep = "/n")
#   
# })

output$FPsecPar <- DT::renderDataTable( server = FALSE, {
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  
  tab2 <- feedback$FP$secondary$tab
  tab3 <- feedback$FP$tertiary$tab
  tab4 <- feedback$FP$quaternary$tab
  
  if(!is.null(tab4) & !is.null(tab3) & !is.null(tab2)){
 
    tab <- rbind(tab2, tab3)
    tab <- rbind(tab, tab4)
    
  DT::datatable(tab[geographicAreaM49_fi == sel_country], extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
  
  } else if(is.null(tab4) & !is.null(tab3) & !is.null(tab2)) {
    
    tab <- rbind(tab2, tab3)
   
    DT::datatable(tab[geographicAreaM49_fi == sel_country], extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(pageLength = 25,
                                                   dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf')))
  } else if(is.null(tab4) & is.null(tab3) & !is.null(tab2)) {
    
    tab <- tab2
    
    DT::datatable(tab[geographicAreaM49_fi == sel_country], extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(pageLength = 25,
                                                   dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf')))
    
  } else {
    
    DT::datatable(data.table(), extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(pageLength = 25,
                                                   dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf')))
  }
})

# output$FPtxt35 <- renderText({ 
#   req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7)
#   msg3 <- feedback$FP[[3]]$msg
#   msg4 <- feedback$FP[[4]]$msg
#   msg5 <- feedback$FP[[5]]$msg
#   paste(c(msg3, msg4, msg5))
#   
# })

output$FPtabUncov <- DT::renderDataTable( server = FALSE, {
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]

  tabUncov <- feedback$FP$NotCovered
  
  if(nrow(tabUncov) > 0){
  DT::datatable(tabUncov[geographicAreaM49_fi == sel_country], extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
  } else {
    
    DT::datatable(data.table(), extensions = 'Buttons', filter = 'top',
                  rownames = FALSE, options = list(pageLength = 25,
                                                   dom = 'Bfrtip',
                                                   buttons = c('csv', 'excel', 'pdf')))
    
  }

})

#-- Save & recalculate ----

observeEvent(input$save, {

  if(input$radioErVSinput == 3){
    
    showModal(modalDialog(
      title = "Please select 'Extr rate' or 'Input' from the tab." ,
      sprintf("This will confirm if you wish the Input or the Extraction rate figures to prevail.")
    ))
  } else {
  
  newMapGP <- updated_mappings$GP
  newMapCDB <-  updated_mappings$CDB
 
  showModal(modalDialog(
    title = "Recalculating!" ,
    sprintf("Please wait for the calculations to be completed.")
  ))
  
  withProgress(message = 'Calculation in progress',
               value = 0, {
                 Sys.sleep(0.25)
                 incProgress(0.15)         
  map_isscfc <- ReadDatatable('map_isscfc')
  setnames(map_isscfc, "measured_item_isscfc", "measuredItemISSCFC")
  
  map_asfis <- ReadDatatable('map_asfis')
  setnames(map_asfis, c("asfis"), c("fisheriesAsfis"))
 
  #-- Needed datasets ----
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  ## Get global production (from Production environment)

  #-- Get whole Global production needed ----
  
  keyDim <- c("geographicAreaM49_fi", "fisheriesAsfis", "measuredElement", "timePointYears")
  
  KeyGlobal <- DatasetKey(domain = "Fisheries", dataset = "fi_global_production", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
    fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
    measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  globalProduction <- GetData(KeyGlobal)
  
  # Aggregate by fisheriesCatchArea
  # Convert flags into ordinal factor so that simple aggregation is possible
  # The function aggregateObservationFlag is too slow so flag are transformed into factors
  
  globalProduction$flagObservationStatus <- factor(globalProduction$flagObservationStatus, 
                                                   levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                                   ordered = TRUE)
  
  globalProduction <- globalProduction[ , list(ValueAggr = sum(Value, na.rm = TRUE), 
                                               flagObservationStatusAggr = max(flagObservationStatus),
                                               flagMethodAggr = "s"),
                                        by=c("geographicAreaM49_fi",
                                             "fisheriesAsfis",
                                             "measuredElement",
                                             "timePointYears")]
  
  setnames(globalProduction, names(globalProduction), c("geographicAreaM49_fi", "fisheriesAsfis",
                                                        "measuredElement", "timePointYears",
                                                        "Value", "flagObservationStatus",
                                                        "flagMethod"))
  
  # Hard code change from FI_001 to 5510, both are Production in tonnes.
  globalProduction$measuredElement <- ifelse(globalProduction$measuredElement == "FI_001", "5510", globalProduction$measuredElement)
  
  #-- Start processing global production ----
  newGP <- GPrecalc(GP = globalProduction, map_asfis = map_asfis, new_map_asfis = newMapGP, year = input$btn_year)
  
  Sys.sleep(0.25)
  incProgress(0.3) 
  #-- Get Commodities data ----
  
  KeyComm <- DatasetKey(domain = "Fisheries Commodities", dataset = "commodities_total", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredItemISSCFC = Dimension(name = "measuredItemISSCFC", 
                                   keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredItemISSCFC" )[,code]),
    measuredElement = Dimension(name = "measuredElement", 
                                keys = GetCodeList("FisheriesCommodities", "commodities_total","measuredElement" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  commodityDB <- GetData(KeyComm)
  commodityDB$flagObservationStatus <- factor(commodityDB$flagObservationStatus,
                                              levels = c('M', 'O', 'N', '', 'X', 'T', 'E', 'I'), 
                                              ordered = TRUE)
  
  # No need of value elements "5622" and "5922"
  commodityDB <- commodityDB[!measuredElement %in% c("5622", "5922", "5930", 
                                                     "5937", "5923", "5931",
                                                     "5940", "5630", "5637")]
  
  # Re-export in Export
  commodityDB <- commodityDB[measuredElement %in% c("5912", "5907",
                                                    "5906"), measuredElement := '5910']
  
  # Other import
  commodityDB <- commodityDB[measuredElement == "5607", measuredElement := '5610']
  
  
  #-- Start processing commodity DB ----
  newCDB <- CDBrecalc(CDB = commodityDB, map_isscfc = map_isscfc, new_map_isscfc = newMapCDB, year = input$btn_year)
  
  Sys.sleep(0.25)
  incProgress(0.45) 
  #-- SUA unbalanced ----

  SUAunbalResults <- SUAunbalCalc(globalProductionAggr = newGP, commodityDBAggr = newCDB)
  
  SUAunbal <- SUAunbalResults$SUAunbal
  initialUnbal <- SUAunbalResults$initialUnbal
  
  modifiedSUA0 <-  rhandsontable::hot_to_r(input$sua_tab7)
  modifiedSUA <- melt(modifiedSUA0, 
                      id.vars = c(1,3:4),
                      measure.vars = 5:ncol(modifiedSUA0),
                      variable.name = 'timePointYears',
                      value.name = 'Value',
                      na.rm = TRUE)
  
  setnames(modifiedSUA, c('Country', 'ICSprod', 'Element'),
           c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'measuredElementSuaFbs'))
  
  modifiedSUA <- modifiedSUA[!measuredElementSuaFbs %in% c('261', '264', '271', '274', '281', '284'),]
  
  # First compare what changed with respect to the orignial table
  
  
  if(nrow(recalc_value$SUAmodtab) == 0){
    SUAinit <- recalc_value$SUAinit[!measuredElementSuaFbs %in% c('261', '264', '271', '274', '281', '284'),]
  } else {
    SUAinit <- recalc_value$SUAmodtab[!measuredElementSuaFbs %in% c('261', '264', '271', '274', '281', '284'),]
  }
  
  SUAcomp <- merge(SUAinit, modifiedSUA, 
                   by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                          'measuredElementSuaFbs', 'timePointYears'),
                   suffixes = c('Init', 'Mod'),
                   all = TRUE)
  SUAcomp[ , c('Value', 
               'flagObservationStatus', 
               'flagMethod') := list(ifelse(round(ValueInit,2) != ValueMod | is.na(ValueInit), ValueMod, ValueInit),
                                     ifelse(round(ValueInit,2) != ValueMod | is.na(ValueInit), 'E', flagObservationStatus),
                                     ifelse(round(ValueInit,2) != ValueMod | is.na(ValueInit), 'f', flagMethod))]
  SUAcomp[ , c('ValueInit', 'ValueMod') := NULL]
  
  SUA2replace <- SUAcomp
  # SUAbal$Value <- round(SUAbal$Value, 2)
  # modifiedSUA$Value <- round(modifiedSUA$Value, 2)
  
  SUAunbalMod <- merge(SUAunbal, SUA2replace, 
                     by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                            'measuredElementSuaFbs', 'timePointYears'), 
                     all = TRUE, suffixes = c('','Mod'))
  
  SUAunbalMod[round(Value,2) != ValueMod  | is.na(Value),
            c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                                                                      flagMethodMod)]
  
  # SUAunbalMod[round(Value, 2) != round(ValueMod, 2),
  #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
  #                                                                     flagMethodMod)]
  
  SUAunbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]

  
  Sys.sleep(0.25)
  incProgress(0.5)

  #-- SUA balanced ----
  
  eR <- SUAunbalMod[measuredElementSuaFbs == '5423']
  
  # validate(
  #   need(length(input$radioErVSinput == 1),
  #        'Choose only to update Extraction rates or Input'
  #        )
  # )
  
  primary <- unique(map_asfis$ics)
  SUAbalResults <- SUAbalCalc(SUA = SUAunbalMod, eR = eR, use = input$radioErVSinput)
  SUAbal <- SUAbalResults$SUA
  
  FPproblems <- SUAbalResults$FPproblems$NotCovered
  SecNegAv <- SUAbalResults$NegAv
  updated_table$NegAv <- SecNegAv
  updated_table$FPproblems <- FPproblems
  
  # Compare with modifiable table
  Sys.sleep(0.25) 
  incProgress(0.65) 

  # NO!!! I modified value and recalculate, no need to reput old values
  # SUAbal$Value <- round(SUAbal$Value, 2)
  # SUA2replace$Value <- round(SUA2replace$Value, 2)
  # 
  # SUAbalMod <- merge(SUAbal, SUA2replace, 
  #                    by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
  #                           'measuredElementSuaFbs', 'timePointYears'), 
  #                    all = TRUE, suffixes = c('','Mod'))
  # 
  # SUAbalMod[is.na(Value) | Value != ValueMod,
  #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
  #                                                                     flagMethodMod)]
  # 
  # # SUAbalMod[Value != ValueMod, 
  # #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
  # #                                                                                        flagMethodMod)]
  # 
  # SUAbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]
  # Get population data 
  
  elemKeys <- "511"
  
  keyPop <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
    geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = sel_country),
    measuredElement = Dimension(name = "measuredElement", keys = elemKeys),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years)
  ))
  
  popSWS <- GetData(keyPop)
  setnames(popSWS,c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))
  
  SUAwithNutr <- SUAnutrCalc(SUAbalAvail = SUAbal, popSWS = popSWS)
  recalc_value$SUAmodtab <- SUAwithNutr
  Sys.sleep(0.25) 
  incProgress(0.80)
  
  #-- FBS ----
  FBS <- FBScalc(SUA2save = SUAwithNutr, popSWS = popSWS)
  
  faostatFBS <- FBS$faostat
  fiasFBS <- FBS$fias
  Sys.sleep(0.25) 
  incProgress(0.95)
               })
  #-- New SUA ----
  updated_data$SUAunbal <- SUAunbal
  updated_data$SUAbal <- SUAwithNutr
  updated_data$FBSaostat <- faostatFBS
  updated_data$FBSfias <- fiasFBS
  
  showModal(modalDialog(
    title = "Recalculation completed!" ,
    sprintf("Please check the new results.")
  ))
  
  workaround$V <- 1
  }
  
})