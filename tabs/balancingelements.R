# link table recall

baltable_reac <- reactive({
  req(input$btn_year, input$btn_country, input$btn_start_year)
  sel_country <- country_input[country_input$label == input$btn_country, code]
  where <- paste("geographic_area_m49_fi = '", sel_country, "'", sep = "")
  baltable <- ReadDatatable('balancing_elements', where = where, readOnly = FALSE)
  
  if(nrow(baltable) == 0){
    baltable <- rbind(baltable, data.table(geographic_area_m49_fi =sel_country),
                       fill = T)
  }
  
  return(baltable)
})

output$balancingelements <-  renderRHandsontable({
  
  table <- baltable_reac()
  setkeyv(table, c("geographic_area_m49_fi",
                   "measured_item_faostat_l2",
                   "measured_element",
                   "start_year",
                   "end_year",
                   "share"))

  if(table[,.N] != table[!duplicated(table),.N]){
  showModal(modalDialog(
    title = "There are duplicates in the table." ,
    sprintf("Please check the balancing elements and correct duplicates.")
  ))
  }
  
  rhandsontable(table, 
                rowHeaders = NULL, width = 'auto', height = 'auto') %>%
    hot_col(c("__id", "__ts"), colWidths = c(rep(0.1,2),rep(150,6)), readOnly = TRUE)
})

observeEvent(input$updBal, {

  updbalTable <- rhandsontable::hot_to_r(input$balancingelements)
  setkey(updbalTable)
  updbalTable <- unique(updbalTable)
 # browser()
  sel_country <- country_input[country_input$label == input$btn_country, code]
  where <- paste("geographic_area_m49_fi = '", sel_country, "'", sep = "")
  baltable2del <- ReadDatatable('balancing_elements', where = where, readOnly = FALSE)
  
  changeset <- Changeset('balancing_elements')
  AddDeletions(changeset, baltable2del)
  Finalise(changeset)
  
  changeset <- Changeset('balancing_elements')
  AddInsertions(changeset, updbalTable)
  Finalise(changeset)
  
  showModal(modalDialog(
    title = "Balancing element table updated." ,
    sprintf("The new version of is now available on the SWS.")
  ))
  
})