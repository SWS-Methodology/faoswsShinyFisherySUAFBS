# Fourth tab, ICS product by element tab

SUAelemTab_reac <- reactive({
  
  req(input$btn_group_fbs_tab4, input$btn_ics_prod_tab4, input$btn_element_sua_tab4,
      input$btn_year, input$btn_country, input$btn_start_year)

  sel_country <- country_input[country_input$label == input$btn_country, code]
  sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  sel_group_fbs <- as.character(groups_input[label == input$btn_group_fbs_tab4]$code)
  sel_ics_prod <- as.character(groups_input[label == input$btn_ics_prod_tab4]$code)
  sel_element_sua <- as.character(element_input[ label %in% input$btn_element_sua_tab4]$code)
  
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
  
  if(length(sel_element_sua) > 0){  
  KeySUAbalElem <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
    geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
    measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                      keys = sel_element_sua), # Or, if all elements, GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code])
    measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                       keys = sel_ics_prod),
    timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  
  withProgress(message = 'SUA balanced data loading in progress',
               value = 0, {
                 
                 Sys.sleep(0.25)
                 incProgress(0.25)
                 SUAbalElem <- GetData(KeySUAbalElem)
                 Sys.sleep(0.75)
                 incProgress(0.95)
               })
  
  tab2show <- merge(SUAbalElem, l2l1[ , .(code_l1, code_l2)], 
                    by.x = 'measuredItemFaostat_L2', by.y = 'code_l2')
  
  return(tab2show)
  }
  
})


output$sua_elem_tab4 <- DT::renderDataTable( server = FALSE, {
  
  sua_elem_tab_out <- copy(SUAelemTab_reac())
  
  setnames(sua_elem_tab_out, c('geographicAreaM49_fi', 'measuredItemFaostat_L2', 
                              'measuredElementSuaFbs', 'timePointYears', 
                              'flagObservationStatus', 'flagMethod', 'code_l1'),
           c('Country', 'ICSprod', 'Element', 'Year', 'F1', 'F2', 'FBSgroup'))
  setcolorder(sua_elem_tab_out, c('Country', 'FBSgroup', 'ICSprod', 'Element', 'Year', 
                                 'Value', 'F1', 'F2'))
  DT::datatable(sua_elem_tab_out, extensions = 'Buttons', filter = 'top',
                rownames = FALSE, options = list(pageLength = 25,
                                                 dom = 'Bfrtip',
                                                 buttons = c('csv', 'excel', 'pdf')))
})


output$gg_plot_tab4 <- renderPlot({
  req(input$btn_group_fbs_tab4, input$btn_ics_prod_tab4,
      input$btn_year, input$btn_country, input$btn_start_year)
  
  suaElem_data <- copy(SUAelemTab_reac())
  
  # suaElem_data[ ,x_pos := as.numeric(input$btn_year)]
  # valuesY <- suaElem_data[timePointYears == as.numeric(input$btn_year), .(measuredElementSuaFbs, Value)]
  # setnames(valuesY, 'Value', 'y_pos')
  # suaElem_data <- merge(suaElem_data, valuesY, by = 'measuredElementSuaFbs')
  
  # Make grand total
  ggplot(data = suaElem_data, aes(x = timePointYears, y = Value)) + 
    geom_line(aes(group = measuredElementSuaFbs, color = measuredElementSuaFbs), size = 0.7) +
    # geom_text(data = suaElem_data[timePointYears == as.numeric(input$btn_year)],
    #           aes(label = measuredElementSuaFbs, color = measuredElementSuaFbs, check_overlap = TRUE),
    #           hjust = 0.7, vjust = 1, show_guide  = F) +
    labs(x = 'Year', color = '')
  
})