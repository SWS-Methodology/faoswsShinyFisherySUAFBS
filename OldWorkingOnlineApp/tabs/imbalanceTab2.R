# Create datatable
SUAimbalanceTab_reac <- reactive({
  
  req(input$btn_country, input$btn_year, input$btn_start_year)
  
  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  
  imbalance_tab <- ReadDatatable('imbalance_tab')
  imbalance_tab_country <- imbalance_tab[geographicaream49_fi == sel_country & timepointyears %in% sel_years]

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
                                      keys = '5510'), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code])
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList('FisheriesCommodities', 'fi_sua_balanced', 'measuredItemFaostat_L2')[, code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'SUA balanced data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAbal <- GetData(KeySUAbal)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  
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
  #                      token = tokenSuaU)
  #   
  # }
  
  R_SWS_SHARE_PATH = "Z:"
  SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
  GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                       token = tokenSuaU)
    

  
  KeySUAunbal <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_unbalanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = '5510'), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code])
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = GetCodeList('FisheriesCommodities', 'fi_sua_balanced', 'measuredItemFaostat_L2')[, code]),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'SUA unbalanced data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAunbal <- GetData(KeySUAunbal)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               }) 
  
  prodCompare <- merge(SUAunbal, SUAbal, by = c("geographicAreaM49_fi","measuredElementSuaFbs", 
                                                "measuredItemFaostat_L2", "timePointYears"),
                       suffixes = c('Unbal', 'Bal'), all = TRUE)
  
  
  return(list(imb = imbalance_tab_country, prod = prodCompare))
  
})


output$sua_imb_tab2 <- DT::renderDataTable( server = FALSE, {
  
  req(input$btn_country, input$btn_year, input$btn_start_year)
  sua_imb_tab_out <- copy(SUAimbalanceTab_reac()$imb)
  
  validate(
    need(nrow(sua_imb_tab_out) > 0, 
         'No imbalance to show.')
  )
  
  setnames(sua_imb_tab_out, c('geographicaream49_fi', 'measureditemfaostat_l2',
                              'timepointyears'),
           c('Country', 'ICSprod', 'Year'))
  setcolorder(sua_imb_tab_out, c('Country', 'ICSprod', 'Year', 'availability'))
  DT::datatable(sua_imb_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
  # Put in red primary!!!!!!!!!!!!
})



output$gg_plot_tab2bis <- renderPlot({
  
  req(input$btn_country, input$btn_year, input$btn_start_year)
  sua_imb_tab_out <- copy(SUAimbalanceTab_reac()$imb)
  
  validate(
    need(nrow(sua_imb_tab_out) > 0, 
         'No imbalance to show.')
  )
  
  ggplot(data = sua_imb_tab_out, aes(x = timepointyears, y = availability)) + 
    geom_point(aes(color = measureditemfaostat_l2)) +
    facet_wrap( ~ measureditemfaostat_l2, scales="free") +
    labs(x = 'Year')
  
}) 


output$sua_prod_diff_tab2 <- DT::renderDataTable( server = FALSE, {
  
  req(input$btn_country, input$btn_year, input$btn_start_year)

  prodCompare <- copy(SUAimbalanceTab_reac()$prod)

  setnames(prodCompare, c('geographicAreaM49_fi', 'measuredItemFaostat_L2',
                              'timePointYears', 'measuredElementSuaFbs'),
           c('Country', 'ICSprod', 'Year', 'Element'))
  
  prodCompare[ , Diff := (ValueUnbal - ValueBal)]
  
  DT::datatable(prodCompare, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
  # Put in red primary!!!!!!!!!!!!
})