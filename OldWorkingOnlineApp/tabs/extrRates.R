extrR_reac <- reactive({
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
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

  KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = '5423'), 
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'Extraction rate data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAbalEr <- GetData(KeySUAbal)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  
  
  return(SUAbalEr)
})

output$extrR <-  renderRHandsontable({
  
  table <- extrR_reac()
  
  rhandsontable(table, rowHeaders = NULL, width = 'auto', height = 'auto') 
})

new_extr_rate <- reactiveValues(eR = data.table())

observeEvent(input$updER, {
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = '5423'), 
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList("FisheriesCommodities", "fi_sua_balanced_control","measuredItemFaostat_L2" )[,code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  SUAbalEr <- GetData(KeySUAbal)
  
  # copy to compare tables with right decimals number
  
  SUAbalErComp <- copy(SUAbalEr)
  SUAbalErComp$Value <- round(SUAbalErComp$Value, 4)   
  updER <- rhandsontable::hot_to_r(input$extrR)
  
  changed <- length(updER[Value != SUAbalErComp$Value, ]$Value)
  
  newValue <- updER[Value != SUAbalErComp$Value, ]
  
  # Radio button for the update type 
  if(input$radioErUpdt == 1){
    
    eRupd <- merge(SUAbalEr, newValue, by = c('geographicAreaM49_fi', 'measuredElementSuaFbs', 
                                              'measuredItemFaostat_L2', 'timePointYears'),
                   all = TRUE, suffixes = c('Old', 'New'))
    
    eRupd[!is.na(ValueNew) & ValueNew != ValueOld, c('ValueOld',
                                                     'flagObservationStatusOld',
                                                     'flagMethodOld') := list(ValueNew, 'E', 'f')]
    setnames(eRupd, c('ValueOld',
                      'flagObservationStatusOld',
                      'flagMethodOld'),
             c('Value',
               'flagObservationStatus',
               'flagMethod'))
    
    eRupd <- eRupd[ , c('ValueNew',
                        'flagObservationStatusNew',
                        'flagMethodNew') := NULL]
    
    # Not good for the decimal problem
    #eRupd <- SUAbalEr[Value != updER$Value, c("Value", 
    #                                          "flagObservationStatus",
    #                                          "flagMethod"):= list(updER[Value != SUAbalEr$Value, ]$Value, 'E', 'f')]
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
    
  } else if(input$radioErUpdt == 2 & changed == 1) {
    
    eRupd <- SUAbalEr[timePointYears %in% sel_years & 
                        measuredItemFaostat_L2 %in% newValue$measuredItemFaostat_L2, 
                      c('Value', 'flagObservationStatus', 'flagMethod') := list(newValue$Value, 'E', 'f')]
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
    
  } else if(input$radioErUpdt == 3 & changed == 1){
    
    eRupd <- SUAbalEr[timePointYears %in% as.character(1961:as.numeric(input$btn_year)) & 
                        measuredItemFaostat_L2 %in% newValue$measuredItemFaostat_L2, 
                      c('Value', 'flagObservationStatus','flagMethod') := list(newValue$Value, 'E', 'f')]
    
    showModal(modalDialog(
      title = "Extraction rates updated." ,
      sprintf("The new figures have been saved.")
    ))
    
  } else if (input$radioErUpdt != 1 & changed > 1){
    
    showModal(modalDialog(
      title = "Extraction cannot be updated." ,
      sprintf("Please change only one rate if you want a time series to be changed.")
    ))
    
    eRupd <- copy(SUAbalEr)
    
  }
  
  new_extr_rate$eR <- eRupd
  
  # SaveData(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", data = compare)
  
})