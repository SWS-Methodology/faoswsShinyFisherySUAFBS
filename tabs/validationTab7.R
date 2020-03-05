# Seventh tab, data validation
recalc_value <- reactiveValues(SUAinit = data.table(),
                               SUAmodtab = data.table(),
                               SUAupload = data.table())

workaround <- reactiveValues(V = 0)

# -- SUA reactive ----

# Tab to show and modify
suaTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs_tab7]$code)
  sel_ics <- input$btn_ics_prod_tab7
  
  
  if('All' %in% sel_ics){
    sel_ics_prod <- as.character(l2l1[code_l1 %in% sel_group_fbs ]$code_l2)
  } else {
    sel_ics_prod <- as.character(groups_input[label %in% sel_ics]$code)
  }
  
  sel_element_sua <- as.character(sua_element_input[ label %in% input$btn_sua_elem_tab7]$code)
  
  if(nrow(recalc_value$SUAmodtab) == 0){
    
    # load SUA balanced if needed
    SUA0 <- reloadDataToken(data = live_data$SUAb, 
                            keycountry = sel_country, 
                            minyear = input$btn_start_year, 
                            maxyear = input$btn_year,
                            keydomain = domainComm, 
                            keydataset = datasetSUABlive,
                            keytoken = tokenSuaB)
    if(!is.null(SUA0)){
      live_data$SUAb <- SUA0
      recalc_value$SUAinit <- SUA0
    } else {
      SUA0 <- live_data$SUAb
      recalc_value$SUAinit <- live_data$SUAb
    }
    
    SUA <- SUA0[measuredItemFaostat_L2 %in% sel_ics_prod & measuredElementSuaFbs %in% sel_element_sua , ]
    
    
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
  
  
  setnames(tab2show, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                       'measuredElementSuaFbs', 'timePointYears', 
                       'flagObservationStatus', 'flagMethod', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'F', 'Fm', 'FBSgroup'))
  
  tab2show <- merge(tab2show, SUAelements[ , .(code, idx)], by.x = 'Element', 
                    by.y = 'code', all.x = TRUE)
  
  tab2show <- tab2show[order(as.numeric(idx)),]
  # tab2show[ , official := ifelse(F1 == '', 1, -1)]
  
  # sua_tab <- sua_tab[ , idx := NULL]
  
  validate(need(
    nrow(tab2show) > 0,
    'No data to show for this product.'
  ))
  
  sua_tab_out <- dcast(tab2show, Country + FBSgroup + ICSprod + idx + Element ~ Year, value.var = c("Value", "F"))
  
  
  colflag <- c((ncol(sua_tab_out) - (ncol(sua_tab_out) -5)/2 + 1):ncol(sua_tab_out))
  colvalue <- c(6:((ncol(sua_tab_out) -5)/2 + 5))
  colorder <- as.vector(matrix(c(colvalue, colflag), nrow = 2, byrow = TRUE))
  
  neworder <- c(names(sua_tab_out)[1:5], names(sua_tab_out)[colorder])
  setcolorder(sua_tab_out, neworder)
  setnames(sua_tab_out, names(sua_tab_out), sub("Value_", "", names(sua_tab_out)))
  
  
  return(list(sua2show = sua_tab_out[ , idx := NULL]))
})

# -- SUA renderRHandsontable ----

output$sua_tab7 <-  renderRHandsontable({
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7,
      input$btn_country, input$btn_year, input$btn_start_year)
  
  sua_tab_out <- suaTab_reac()$sua2show
  
  validate(need(
    nrow(sua_tab_out) > 0,
    'No data to show for this product.'
  ))
  
  
  col2hide <-  seq(6, ncol(sua_tab_out), by = 2)
  colvalue <- seq(5, ncol(sua_tab_out) - 1, by = 2)
  
  rhandsontable(sua_tab_out, rowHeaders = NULL, width = 'auto', height = 'auto') %>%
    hot_cols(fixedColumnsLeft = 4)  # number of columns to freeze = 4
  
  #  , renderer = "
  # function (instance, td, row, col, prop, value, cellProperties) {
  #   Handsontable.renderers.NumericRenderer.apply(this, arguments);
  #   if (value == '') {
  #   td.style.background = 'lightgreen';
  #   }
  #  }"
  #   ) 
  
  
})

# -- Availability table ---- 
output$textAv <- renderText({
  req(input$btn_group_fbs_tab7,
      input$btn_ics_prod_tab7) # input$btn_country, input$btn_year, input$btn_start_year,
  
  "Availability table:"
})

output$availability <- renderRHandsontable({
  #req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7) #,
  # input$btn_country, input$btn_year, input$btn_start_year)
  if(is.null(input$sua_tab7)) return(NULL)
  
  tab_updated <- rhandsontable::hot_to_r(input$sua_tab7)
  tab_updated <- tab_updated[ , -grep("F_", colnames(tab_updated)), with = FALSE ]
  
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

# -- Upload/Download ----

output$contents <- renderTable({
  
  inFile <- input$updatedSUA
  
  if (is.null(inFile))
    return(NULL)
  
  # req(input$updatedSUA)
  
  tryCatch(
    {
      df <- read.csv(inFile$datapath,
                     header = TRUE,
                     colClasses = "character"#input$header #,
                     #   sep = input$sep,
                     #  quote = input$quote
      )
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )
  recalc_value$SUAupload <- df
  return(df)
  
})

output$downloadData <- downloadHandler(
  filename = function() {
    paste("EditedTable-", Sys.Date(), ".csv", sep="")
  },
  # what should go in place of table
  content = function(file) {
    table <- suaTab_reac()$sua2show
    table <- table[ , -grep("F_", colnames(table)), with = FALSE ]
    write.csv(table, file, row.names = FALSE)
  }
)

# -- FP Feedback ----

feedback <- reactiveValues(FP = readRDS('FoodProcessingFeedback.rds'))

# output$FPtxt1 <- renderText({ 
#   req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7)
#   msg1 <- feedback$FP[[1]]$msg
#   paste("Primary availability problems:", 
#         msg1, sep = "/n")
# 
#   })

output$FPtab1 <- DT::renderDataTable( server = FALSE, {
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7) #,
  # input$btn_country, input$btn_year, input$btn_start_year)
  
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
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7) #,
  #input$btn_country, input$btn_year, input$btn_start_year)
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
  req(input$btn_group_fbs_tab7, input$btn_ics_prod_tab7, input$btn_sua_elem_tab7) #,
  #   input$btn_country, input$btn_year, input$btn_start_year)
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

# -- Save & recalculate ----


observeEvent(input$save, { # Opening Observe event
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label %in% input$btn_group_fbs_tab7]$code)
  sel_ics <- input$btn_ics_prod_tab7
  
  if(input$radioErVSinput == 3){ # Choose Input or Er
    
    showModal(modalDialog(
      title = "Please select 'Extr rate' or 'Input' from the tab." ,
      sprintf("This will confirm if you wish the Input or the Extraction rate figures to prevail.")
    ))
    
  } else {
  
  # Load population data (almost always useful)
  if(nrow(live_data$Pop) == 0)
  {
    elemKeys <- "511"
    
    keyPop <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
      geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = sel_country),
      measuredElement = Dimension(name = "measuredElement", keys = elemKeys),
      timePointYears = Dimension(name = "timePointYears", keys = sel_years)
    ))
    
    popSWS <- GetData(keyPop)
    setnames(popSWS,c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))
    live_data$Pop <- popSWS
  } else if(nrow(live_data$Pop) > 0 &
            unique(live_data$Pop$geographicAreaM49_fi) != sel_country |
            min(unique(live_data$Pop$timePointYears)) != input$btn_start_year |
            max(unique(live_data$Pop$timePointYears)) != input$btn_year)
  {
    
    elemKeys <- "511"
    
    keyPop <- DatasetKey(domain = "population", dataset = "population_unpd", dimensions = list(
      geographicAreaM49 = Dimension(name = "geographicAreaM49", keys = sel_country),
      measuredElement = Dimension(name = "measuredElement", keys = elemKeys),
      timePointYears = Dimension(name = "timePointYears", keys = sel_years)
    ))
    
    popSWS <- GetData(keyPop)
    setnames(popSWS,c("geographicAreaM49", "measuredElement"),c("geographicAreaM49_fi", "measuredElementSuaFbs"))
    live_data$Pop <- popSWS
    
    
  } else 
  {
    popSWS <- live_data$Pop
  }
  
  # -- No recalculation ----
  if(input$reprocess == 'No')
  {# Opening input$reprocess == 'No'
    
    # Only showing message to say to choose another option  
    showModal(modalDialog(
      title = "Please select the type of recalculation." ,
      sprintf("Select: 'Complete' if there are changes in the GP or CDB mappings; 
            'Only SUAbalanced' if you want to see changes at SUA level (only in this tab);
            'Since SUAbalanced' if there is no change at GP and CDB level but you want to see effects at FBS level;
            'Only Nutrients & FBS' if you want to take the SUA as it appears in this tab with no change and recalculate nutrients and standardization.")
    ))
    
    # -- Complete recalculation ----
    
  } else if (input$reprocess == 'Complete')
  {  # Closing input$reprocess == 'No' & Opening input$reprocess == 'Complete'
    
    newMapGP <- updated_mappings$GP
    newMapCDB <-  updated_mappings$CDB
    
    showModal(modalDialog(
      title = "Recalculating!" ,
      sprintf("Please wait for the calculations to be completed.")
    ))
    
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   Sys.sleep(0.1)
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
                   
                   
                   if(nrow(InitialDatasets$GP) == 0)
                   {
                     
                     KeyGlobal <- DatasetKey(domain = domainGP, dataset = datasetGP, dimensions = list(
                       geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
                       fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
                       fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
                       measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
                       timePointYears = Dimension(name = "timePointYears", keys = sel_years)))
                     
                     globalProduction <- GetData(KeyGlobal)
                     
                   } else if(nrow(InitialDatasets$GP) > 0 &
                             unique(InitialDatasets$GP$geographicAreaM49_fi) != sel_country |
                             min(unique(InitialDatasets$GP$timePointYears)) != input$btn_start_year |
                             max(unique(InitialDatasets$GP$timePointYears)) != input$btn_year)
                   {
                     
                     
                     KeyGlobal <- DatasetKey(domain = domainGP, dataset = datasetGP, dimensions = list(
                       geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
                       fisheriesAsfis = Dimension(name = "fisheriesAsfis", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesAsfis" )[,code]),
                       fisheriesCatchArea = Dimension(name = "fisheriesCatchArea", keys = GetCodeList("Fisheries", "fi_global_production","fisheriesCatchArea" )[,code]),
                       measuredElement = Dimension(name = "measuredElement", keys = c("FI_001")),
                       timePointYears = Dimension(name = "timePointYears", keys = sel_years)))
                     
                     globalProduction <- GetData(KeyGlobal)
                     
                   } else 
                   {
                     globalProduction <- InitialDatasets$GP
                   } 
                   
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
                   
                   Sys.sleep(0.1)
                   incProgress(0.3) 
                   #-- Get Commodities data ----
                   
                   if(nrow(InitialDatasets$CDB) == 0)
                   {
                     
                     KeyComm <- DatasetKey(domain = domainComm, 
                                           dataset = datasetCDB, 
                                           dimensions = list(geographicAreaM49_fi = Dimension(name = 'geographicAreaM49_fi', 
                                                                                              keys = sel_country),
                                                             measuredElement = Dimension(name = 'measuredElement', 
                                                                                         GetCodeList(domainComm, 
                                                                                                     datasetCDB,
                                                                                                     'measuredElement')[,code]),
                                                             measuredItemISSCFC = Dimension(name = 'measuredItemISSCFC', 
                                                                                            GetCodeList(domainComm, 
                                                                                                        datasetCDB,
                                                                                                        'measuredItemISSCFC')[,code]),
                                                             timePointYears = Dimension(name = 'timePointYears', keys =  sel_years )))
                     
                     withProgress(message = 'Data loading in progress',
                                  value = 0, {
                                    Sys.sleep(0.25)
                                    incProgress(0.25)
                                    commodityDB <- GetData(KeyComm)
                                    Sys.sleep(0.75)
                                    incProgress(0.95)
                                  })
                     
                     InitialDatasets$CDB <- commodityDB
                     
                   } else if(nrow(InitialDatasets$CDB) > 0 &
                             unique(InitialDatasets$CDB$geographicAreaM49_fi) != sel_country |
                             min(unique(InitialDatasets$CDB$timePointYears)) != input$btn_start_year |
                             max(unique(InitialDatasets$CDB$timePointYears)) != input$btn_year)
                   {
                     
                     KeyComm <- DatasetKey(domain = domainComm, 
                                           dataset = datasetCDB, 
                                           dimensions = list(geographicAreaM49_fi = Dimension(name = 'geographicAreaM49_fi', 
                                                                                              keys = sel_country),
                                                             measuredElement = Dimension(name = 'measuredElement', 
                                                                                         GetCodeList(domainComm, 
                                                                                                     datasetCDB,
                                                                                                     'measuredElement')[,code]),
                                                             measuredItemISSCFC = Dimension(name = 'measuredItemISSCFC', 
                                                                                            GetCodeList(domainComm, 
                                                                                                        datasetCDB,
                                                                                                        'measuredItemISSCFC')[,code]),
                                                             timePointYears = Dimension(name = 'timePointYears', keys =  sel_years )))
                     withProgress(message = 'Data loading in progress',
                                  value = 0, {
                                    Sys.sleep(0.25)
                                    incProgress(0.25)
                                    commodityDB <- GetData(KeyComm)
                                    Sys.sleep(0.75)
                                    incProgress(0.95)
                                  })
                     
                     InitialDatasets$CDB <- commodityDB
                     
                   } else 
                   {
                     commodityDB <- InitialDatasets$CDB
                   }
                   
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
                   
                   Sys.sleep(0.1)
                   incProgress(0.45) 
                   #-- SUA unbalanced ----
                   
                   SUAunbalResults <- SUAunbalCalc(globalProductionAggr = newGP, commodityDBAggr = newCDB)
                   
                   SUAunbal <- SUAunbalResults$SUAunbal
                   initialUnbal <- SUAunbalResults$initialUnbal
                   
                   if(input$csv_online == 1)
                   {
                     modifiedSUA0 <-  rhandsontable::hot_to_r(input$sua_tab7)
                     modifiedSUA0 <- modifiedSUA0[ , -grep("F_", colnames(modifiedSUA0)), with = FALSE ]
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     setnames(modifiedSUA, c('Country', 'ICSprod', 'Element'),
                              c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'measuredElementSuaFbs'))
                     
                     
                   } else if(input$csv_online == 2)
                   {
                     
                     #-- Pulling uploaded file ----
                     modifiedSUA0 <- recalc_value$SUAupload
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(1,3,4)], c('geographicAreaM49_fi',
                                                                             'measuredItemFaostat_L2',
                                                                             'measuredElementSuaFbs'))
                     # Drop flag columns
                     colchosen <-  names(modifiedSUA0)[!grepl('F_', names(modifiedSUA0))]
                     setDT(modifiedSUA0)
                     modifiedSUA0 <- modifiedSUA0[ , colchosen, with = FALSE]
                     
                     yearNames <- sub("X", '', names(modifiedSUA0)[c(5:ncol(modifiedSUA0))])
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(5:ncol(modifiedSUA0))], yearNames)
                     
                     modifiedSUA0 <- as.data.table(modifiedSUA0)
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     modifiedSUA <- as.data.table(modifiedSUA)
                     modifiedSUA$Value <- as.numeric(modifiedSUA$Value)
                     
                   }
                   
                   modifiedSUA <- modifiedSUA[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   
                   # First compare what changed with respect to the original table
                   
                   if(nrow(recalc_value$SUAmodtab) == 0){
                     SUAinit <- recalc_value$SUAinit[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   } else {
                     SUAinit <- recalc_value$SUAmodtab[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   }
                   
                   SUAcomp <- merge(SUAinit, modifiedSUA, 
                                    by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                           'measuredElementSuaFbs', 'timePointYears'),
                                    suffixes = c('', 'Mod'),
                                    all = TRUE)
                   setDT(SUAcomp)
                   SUAcomp <- SUAcomp[][!is.na(ValueMod) & Value != ValueMod | is.na(Value) , c('Value', 
                                                                                                'flagObservationStatus', 
                                                                                                'flagMethod') := list(ValueMod,
                                                                                                                      'E',
                                                                                                                      'f')]
                   
                   SUAcomp <- SUAcomp[ , c('ValueMod') := NULL]
                   
                   SUA2replace <- SUAcomp
                   # SUAbal$Value <- round(SUAbal$Value, 2)
                   # modifiedSUA$Value <- round(modifiedSUA$Value, 2)
                   
                   SUAunbalMod <- merge(SUAunbal, SUA2replace, 
                                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                               'measuredElementSuaFbs', 'timePointYears'), 
                                        all = TRUE, suffixes = c('','Mod'))
                   
                   SUAunbalMod <- SUAunbalMod[][Value != ValueMod  | is.na(Value),
                                                c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                                                                                                          flagMethodMod)]
                   
                   # SUAunbalMod[round(Value, 2) != round(ValueMod, 2),
                   #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                   #                                                                     flagMethodMod)]
                   
                   SUAunbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]
                   
                   
                   Sys.sleep(0.1)
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
                   newMessages <- SUAbalResults$msg
                   FPproblems <- SUAbalResults$FPproblems$NotCovered
                   SecNegAv <- SUAbalResults$NegAv
                   updated_table$NegAv <- SecNegAv
                   updated_table$FPproblems <- FPproblems
                   
                   # Compare with modifiable table
                   Sys.sleep(0.1) 
                   incProgress(0.65) 
                   
                   SUAwithNutr <- SUAnutrCalc(SUAbalAvail = SUAbal, popSWS = popSWS)
                   recalc_value$SUAmodtab <- SUAwithNutr
                   Sys.sleep(0.1) 
                   incProgress(0.80)
                   
                   #-- FBS ----
                   FBS <- FBScalc(SUA2save = SUAwithNutr, popSWS = popSWS)
                   
                   faostatFBS <- FBS$faostat
                   fiasFBS <- FBS$fias
                   Sys.sleep(0.1) 
                   incProgress(0.95)
                 })
    
    #-- New SUA ----
    updated_data$SUAunbal <- SUAunbal
    updated_data$SUAbal <- SUAwithNutr
    updated_data$FBSaostat <- faostatFBS
    updated_data$FBSfias <- fiasFBS
    
    showModal(modalDialog(
      title = "Recalculation completed!" ,
      sprintf("Please check the new results.", newMessages$msg1, newMessages$msg2, newMessages$msg3)
    ))
    
    workaround$V <- 1
    
    # -- Only SUA balanced recalculation ----
    
  } else if(input$reprocess == 'SUAb')
  { ## Closing input$reprocess == 'Complete' & Opening input$reprocess == 'SUAb'
    
    showModal(modalDialog(
      title = "Recalculating!" ,
      sprintf("Please wait for the calculations to be completed.")
    ))
    
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   
                   Sys.sleep(0.1)
                   incProgress(0.25)
                   
                   
                   SUAunbal <- reloadDataToken(data = live_data$SUAu, 
                                               keycountry = sel_country, 
                                               minyear = input$btn_start_year, 
                                               maxyear = input$btn_year,
                                               keydomain = domainComm, 
                                               keydataset = datasetSUAUlive,
                                               keytoken = tokenSuaU)
                   
                   if(!is.null(SUAunbal)){
                     live_data$SUAu <- SUAunbal
                   } else {
                     
                     SUAunbal <- live_data$SUAu
                   }
                   
                   # SUAunbal <- SUAunbalResults$SUAunbal
                   # initialUnbal <- SUAunbalResults$initialUnbal
                   if(input$csv_online == 1)
                   {
                     modifiedSUA0 <-  rhandsontable::hot_to_r(input$sua_tab7)
                     modifiedSUA0 <- modifiedSUA0[ , -grep("F_", colnames(modifiedSUA0)), with = FALSE ]
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     setnames(modifiedSUA, c('Country', 'ICSprod', 'Element'),
                              c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'measuredElementSuaFbs'))
                     
                     
                   } else if(input$csv_online == 2)
                   {
                     
                     #-- Pulling uploaded file ----
                     modifiedSUA0 <- recalc_value$SUAupload
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(1,3,4)], c('geographicAreaM49_fi',
                                                                             'measuredItemFaostat_L2',
                                                                             'measuredElementSuaFbs'))
                     # Drop flag columns
                     colchosen <-  names(modifiedSUA0)[!grepl('F_', names(modifiedSUA0))]
                     setDT(modifiedSUA0)
                     modifiedSUA0 <- modifiedSUA0[ , colchosen, with = FALSE]
                     
                     yearNames <- sub("X", '', names(modifiedSUA0)[c(5:ncol(modifiedSUA0))])
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(5:ncol(modifiedSUA0))], yearNames)
                     
                     modifiedSUA0 <- as.data.table(modifiedSUA0)
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     modifiedSUA <- as.data.table(modifiedSUA)
                     modifiedSUA$Value <- as.numeric(modifiedSUA$Value)
                     
                   }
                   
                   modifiedSUA <- modifiedSUA[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   
                   # First compare what changed with respect to the original table
                   
                   if(nrow(recalc_value$SUAmodtab) == 0){
                     SUAinit <- recalc_value$SUAinit[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   } else {
                     SUAinit <- recalc_value$SUAmodtab[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   }
                   
                   SUAcomp <- merge(SUAinit, modifiedSUA, 
                                    by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                           'measuredElementSuaFbs', 'timePointYears'),
                                    suffixes = c('', 'Mod'),
                                    all = TRUE)
                   setDT(SUAcomp)
                   SUAcomp <- SUAcomp[][!is.na(ValueMod) & Value != ValueMod | is.na(Value) , c('Value', 
                                                                                                'flagObservationStatus', 
                                                                                                'flagMethod') := list(ValueMod,
                                                                                                                      'E',
                                                                                                                      'f')]
                   
                   SUAcomp <- SUAcomp[ , c('ValueMod') := NULL]
                   
                   SUA2replace <- SUAcomp
                   # SUAbal$Value <- round(SUAbal$Value, 2)
                   # modifiedSUA$Value <- round(modifiedSUA$Value, 2)
                   
                   SUAunbalMod <- merge(SUAunbal, SUA2replace, 
                                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                               'measuredElementSuaFbs', 'timePointYears'), 
                                        all = TRUE, suffixes = c('','Mod'))
                   
                   SUAunbalMod <- SUAunbalMod[][Value != ValueMod  | is.na(Value),
                                                c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                                                                                                          flagMethodMod)]
                   
                   # SUAunbalMod[round(Value, 2) != round(ValueMod, 2),
                   #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                   #                                                                     flagMethodMod)]
                   
                   SUAunbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]
                   
                   
                   Sys.sleep(0.1)
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
                   newMessages <- SUAbalResults$msg
                   FPproblems <- SUAbalResults$FPproblems$NotCovered
                   SecNegAv <- SUAbalResults$NegAv
                   updated_table$NegAv <- SecNegAv
                   updated_table$FPproblems <- FPproblems
                   
                   # Compare with modifiable table
                   Sys.sleep(0.1) 
                   incProgress(0.65) 
                   
                   SUAwithNutr <- SUAnutrCalc(SUAbalAvail = SUAbal, popSWS = popSWS)
                   recalc_value$SUAmodtab <- SUAwithNutr
                   #################################################
                   # Sys.sleep(0.80) 
                   # incProgress(0.80)
                   # 
                   #-- FBS ----
                   # FBS <- FBScalc(SUA2save = SUAwithNutr, popSWS = popSWS)
                   # 
                   # faostatFBS <- FBS$faostat
                   # fiasFBS <- FBS$fias
                   #######################################################
                   Sys.sleep(0.1) 
                   incProgress(0.95)
                 })
    
    #-- New SUA ----
    # updated_data$SUAunbal <- SUAunbal
    updated_data$SUAbal <- SUAwithNutr
    # updated_data$FBSaostat <- faostatFBS
    # updated_data$FBSfias <- fiasFBS
    
    showModal(modalDialog(
      title = "Recalculation completed!" ,
      sprintf("Please check the new results.", newMessages$msg1, newMessages$msg2, newMessages$msg3)
    ))
    
    workaround$V <- 1
    
    # -- SUA balanced + FBS recalculation ----
    
  } else if(input$reprocess == 'SUAbTot')
  {  # Closing input$reprocess == 'SUAb' & Opening input$reprocess == 'SUAbTot'
    
    showModal(modalDialog(
      title = "Recalculating!" ,
      sprintf("Please wait for the calculations to be completed.")
    ))
    
    
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   
                   Sys.sleep(0.1)
                   incProgress(0.25)
                   #put SUAunbal #SUAunbalCalc(globalProductionAggr = newGP, commodityDBAggr = newCDB)
                   
                   SUAunbal <- reloadDataToken(data = live_data$SUAu, 
                                               keycountry = sel_country, 
                                               minyear = input$btn_start_year, 
                                               maxyear = input$btn_year,
                                               keydomain = domainComm, 
                                               keydataset = datasetSUAUlive,
                                               keytoken = tokenSuaU)
                   
                   if(!is.null(SUAunbal)){
                     live_data$SUAu <- SUAunbal
                   } else {
                     
                     SUAunbal <- live_data$SUAu
                   }
                   
                   if(input$csv_online == 1){
                     modifiedSUA0 <-  rhandsontable::hot_to_r(input$sua_tab7)
                     modifiedSUA0 <- modifiedSUA0[ , -grep("F_", colnames(modifiedSUA0)), with = FALSE ]
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     setnames(modifiedSUA, c('Country', 'ICSprod', 'Element'),
                              c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'measuredElementSuaFbs'))
                     
                     
                   } else if(input$csv_online == 2){
                     
                     #-- Pulling uploaded file ----
                     modifiedSUA0 <- recalc_value$SUAupload
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(1,3,4)], c('geographicAreaM49_fi',
                                                                             'measuredItemFaostat_L2',
                                                                             'measuredElementSuaFbs'))
                     # Drop flag columns
                     colchosen <-  names(modifiedSUA0)[!grepl('F_', names(modifiedSUA0))]
                     setDT(modifiedSUA0)
                     modifiedSUA0 <- modifiedSUA0[ , colchosen, with = FALSE]
                     
                     yearNames <- sub("X", '', names(modifiedSUA0)[c(5:ncol(modifiedSUA0))])
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(5:ncol(modifiedSUA0))], yearNames)
                     
                     modifiedSUA0 <- as.data.table(modifiedSUA0)
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     modifiedSUA <- as.data.table(modifiedSUA)
                     modifiedSUA$Value <- as.numeric(modifiedSUA$Value)
                     
                   }
                   
                   modifiedSUA <- modifiedSUA[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   
                   # First compare what changed with respect to the original table
                   
                   if(nrow(recalc_value$SUAmodtab) == 0){
                     SUAinit <- recalc_value$SUAinit[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   } else {
                     SUAinit <- recalc_value$SUAmodtab[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   }
                   
                   SUAcomp <- merge(SUAinit, modifiedSUA, 
                                    by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                           'measuredElementSuaFbs', 'timePointYears'),
                                    suffixes = c('', 'Mod'),
                                    all = TRUE)
                   setDT(SUAcomp)
                   SUAcomp <- SUAcomp[][!is.na(ValueMod) & Value != ValueMod | is.na(Value) , c('Value', 
                                                                                                'flagObservationStatus', 
                                                                                                'flagMethod') := list(ValueMod,
                                                                                                                      'E',
                                                                                                                      'f')]
                   
                   SUAcomp <- SUAcomp[ , c('ValueMod') := NULL]
                   
                   SUA2replace <- SUAcomp
                   # SUAbal$Value <- round(SUAbal$Value, 2)
                   # modifiedSUA$Value <- round(modifiedSUA$Value, 2)
                   
                   SUAunbalMod <- merge(SUAunbal, SUA2replace, 
                                        by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                               'measuredElementSuaFbs', 'timePointYears'), 
                                        all = TRUE, suffixes = c('','Mod'))
                   
                   SUAunbalMod <- SUAunbalMod[][Value != ValueMod  | is.na(Value),
                                                c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                                                                                                          flagMethodMod)]
                   
                   # SUAunbalMod[round(Value, 2) != round(ValueMod, 2),
                   #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                   #                                                                     flagMethodMod)]
                   
                   SUAunbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]
                   
                   
                   Sys.sleep(0.1)
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
                   newMessages <- SUAbalResults$msg
                   FPproblems <- SUAbalResults$FPproblems$NotCovered
                   SecNegAv <- SUAbalResults$NegAv
                   updated_table$NegAv <- SecNegAv
                   updated_table$FPproblems <- FPproblems
                   
                   # Compare with modifiable table
                   Sys.sleep(0.1) 
                   incProgress(0.65) 
                   
                   SUAwithNutr <- SUAnutrCalc(SUAbalAvail = SUAbal, popSWS = popSWS)
                   recalc_value$SUAmodtab <- SUAwithNutr
                   Sys.sleep(0.1) 
                   incProgress(0.80)
                   
                   #-- FBS ----
                   FBS <- FBScalc(SUA2save = SUAwithNutr, popSWS = popSWS)
                   
                   faostatFBS <- FBS$faostat
                   fiasFBS <- FBS$fias
                   Sys.sleep(0.1) 
                   incProgress(0.95)
                 }) 
    
    
    #-- New SUA ----
    updated_data$SUAunbal <- SUAunbal
    updated_data$SUAbal <- SUAwithNutr
    updated_data$FBSaostat <- faostatFBS
    updated_data$FBSfias <- fiasFBS
    
    showModal(modalDialog(
      title = "Recalculation completed!" ,
      sprintf("Please check the new results.", newMessages$msg1, newMessages$msg2, newMessages$msg3)
    ))
    
    workaround$V <- 1
    
    # -- Only FBS recalculation ----
  } else if(input$reprocess == 'NutFbs')
  { # Closing input$reprocess == 'SUAbTot' & Opening input$reprocess == 'NutFbs'
    
    showModal(modalDialog(
      title = "Recalculating!" ,
      sprintf("Please wait for the calculations to be completed.")
    ))
    
    withProgress(message = 'Calculation in progress',
                 value = 0, {
                   Sys.sleep(0.1)
                   incProgress(0.15) 
                   
                   
                   SUAbal <- reloadDataToken(data = live_data$SUAb, 
                                             keycountry = sel_country, 
                                             minyear = input$btn_start_year, 
                                             maxyear = input$btn_year,
                                             keydomain = domainComm, 
                                             keydataset = datasetSUABlive,
                                             keytoken = tokenSuaB)
                   
                   if(!is.null(SUAbal)){
                     live_data$SUAb <- SUAbal
                   } else {
                     SUAbal <- live_data$SUAb
                   }
                   
                   # Once session Sua balanced loaded, SUA fron tab is pulled (no nutrients)
                   if(input$csv_online == 1){
                     modifiedSUA0 <-  rhandsontable::hot_to_r(input$sua_tab7)
                     modifiedSUA0 <- modifiedSUA0[ , -grep("F_", colnames(modifiedSUA0)), with = FALSE ]
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     setnames(modifiedSUA, c('Country', 'ICSprod', 'Element'),
                              c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 'measuredElementSuaFbs'))
                     
                     
                   } else if(input$csv_online == 2){
                     
                     #-- Pulling uploaded file ----
                     modifiedSUA0 <- recalc_value$SUAupload
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(1,3,4)], c('geographicAreaM49_fi',
                                                                             'measuredItemFaostat_L2',
                                                                             'measuredElementSuaFbs'))
                     # Drop flag columns
                     colchosen <-  names(modifiedSUA0)[!grepl('F_', names(modifiedSUA0))]
                     setDT(modifiedSUA0)
                     modifiedSUA0 <- modifiedSUA0[ , colchosen, with = FALSE]
                     
                     yearNames <- sub("X", '', names(modifiedSUA0)[c(5:ncol(modifiedSUA0))])
                     
                     setnames(modifiedSUA0, names(modifiedSUA0)[c(5:ncol(modifiedSUA0))], yearNames)
                     
                     modifiedSUA0 <- as.data.table(modifiedSUA0)
                     
                     modifiedSUA <- melt(modifiedSUA0, 
                                         id.vars = c(1,3:4),
                                         measure.vars = 5:ncol(modifiedSUA0),
                                         variable.name = 'timePointYears',
                                         value.name = 'Value',
                                         na.rm = TRUE)
                     
                     modifiedSUA <- as.data.table(modifiedSUA)
                     modifiedSUA$Value <- as.numeric(modifiedSUA$Value)
                     
                   }
                   
                   modifiedSUA <- modifiedSUA[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   
                   # First compare what changed with respect to the original table
                   
                   if(nrow(recalc_value$SUAmodtab) == 0){
                     SUAinit <- recalc_value$SUAinit[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   } else {
                     SUAinit <- recalc_value$SUAmodtab[!measuredElementSuaFbs %in% elkeyNot2consider,]
                   }
                   
                   SUAcomp <- merge(SUAinit, modifiedSUA, 
                                    by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                           'measuredElementSuaFbs', 'timePointYears'),
                                    suffixes = c('', 'Mod'),
                                    all = TRUE)
                   setDT(SUAcomp)
                   SUAcomp <- SUAcomp[][!is.na(ValueMod) & Value != ValueMod | is.na(Value) , c('Value', 
                                                                                                'flagObservationStatus', 
                                                                                                'flagMethod') := list(ValueMod,
                                                                                                                      'E',
                                                                                                                      'f')]
                   
                   SUAcomp <- SUAcomp[ , c('ValueMod') := NULL]
                   
                   SUA2replace <- SUAcomp
                   # SUAbal$Value <- round(SUAbal$Value, 2)
                   # modifiedSUA$Value <- round(modifiedSUA$Value, 2)
                   
                   SUAbalMod <- merge(SUAbal, SUA2replace, 
                                      by = c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                                             'measuredElementSuaFbs', 'timePointYears'), 
                                      all = TRUE, suffixes = c('','Mod'))
                   
                   SUAbalMod <- SUAbalMod[][Value != ValueMod  | is.na(Value),
                                            c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                                                                                                      flagMethodMod)]
                   
                   # SUAunbalMod[round(Value, 2) != round(ValueMod, 2),
                   #           c('Value', 'flagObservationStatus', 'flagMethod') := list(ValueMod, flagObservationStatusMod,
                   #                                                                     flagMethodMod)]
                   
                   SUAbalMod[ , c('ValueMod', 'flagObservationStatusMod', 'flagMethodMod') := NULL]
                   
                   
                   Sys.sleep(0.1)
                   incProgress(0.5)
                   
                   SUAwithNutr <- SUAnutrCalc(SUAbalAvail = SUAbalMod, popSWS = popSWS)
                   recalc_value$SUAmodtab <- SUAwithNutr
                   Sys.sleep(0.1) 
                   incProgress(0.80)
                   
                   #-- FBS ----
                   FBS <- FBScalc(SUA2save = SUAwithNutr, popSWS = popSWS)
                   
                   faostatFBS <- FBS$faostat
                   fiasFBS <- FBS$fias
                   Sys.sleep(0.1) 
                   incProgress(0.95)
                 })
    
    #-- New SUA ----
    # updated_data$SUAunbal
    updated_data$SUAbal <- SUAwithNutr
    updated_data$FBSaostat <- faostatFBS
    updated_data$FBSfias <- fiasFBS
    
    showModal(modalDialog(
      title = "Recalculation completed!" ,
      sprintf("Please check the new results.") #, newMessages$msg1, newMessages$msg2, newMessages$msg3)
    ))
    
    workaround$V <- 1
    
  }  # Closing input$reprocess == 'NutFbs'
  }
  
})  # Closing Observe event




