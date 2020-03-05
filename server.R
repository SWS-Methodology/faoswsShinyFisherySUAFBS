shinyServer(function(input, output, session) {
  
  # -- Insert token ---- 
  output$btn_token1 <- renderUI({
    textInput(inputId = 'btn_token1', label = "Insert the 'SUA unbalanced' session token", value = NA)
  })
  
  output$btn_token2 <- renderUI({
    textInput(inputId = 'btn_token2', label = "Insert the 'SUA balanced' session token", value = NA)
  })
  
  output$btn_token3 <- renderUI({
    textInput(inputId = 'btn_token3', label = "Insert the 'FBS fias' session token", value = NA)
  })
  
  token_reac <- reactive({
    
    tokenTab <- ReadDatatable('fi_sua_fbs_token')
    return(tokenTab)
  })
  
  
  output$token_tab <- DT::renderDataTable( server = FALSE, {
    
    tokenOut <- token_reac()
    DT::datatable(tokenOut)
    
  })
  
  observeEvent(input$btn_upd_token, {
    
    tokenTab <- ReadDatatable('fi_sua_fbs_token', readOnly = FALSE)
    
    t1 <- ifelse(is.na(input$btn_token1), tokenTab$token[1], input$btn_token1)
    t2 <- ifelse(is.na(input$btn_token2), tokenTab$token[2], input$btn_token2)
    t3 <- ifelse(is.na(input$btn_token3), tokenTab$token[3], input$btn_token3)
    date <- as.character(Sys.Date())
    
    tokenTab[ , token := c(t1, t2, t3) ]
    tokenTab[ , last_upd := date]
    
    changeset <- Changeset('fi_sua_fbs_token')
    AddModifications(changeset, tokenTab)
    Finalise(changeset)
    
    tokenSuaU <<- t1
    tokenSuaB <<- t2
    tokenFbs <<- t3
    
    
    showModal(modalDialog(
      title = "Token updated." ,
      sprintf("The chosen session will be used in the following tabs.")
    ))
    
  })
  
  # -- Update button ----
  # Check what production to update and which to keep official and manage imbalance by stocks!
  
  
  #-- Selected year button ----
  output$btn_year  <- renderUI({
    
    # Country button required
    req(input$btn_country)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(sel_country != "-") {
      currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
      years_input <- as.character(sort(1961:currentYear, decreasing = TRUE))
      
      # Input details
      selectInput(inputId = "btn_year",
                  label = 'End year',
                  choices = c("", years_input)#,
                  #selected = '2015'
      )
    }
  })
  
  #-- Start year button ----
  
  output$btn_start_year <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    if(is.null(input$btn_year)){
      min <- '1961'
    } else {
      min <- input$btn_year
    }
    
    if(sel_country != "-") {
      currentYear <- as.numeric(gsub("\\-[0-9]*", "", Sys.Date()))
      years_input <- as.character(sort(1961:currentYear, decreasing = TRUE))
      start_year_input <- years_input[years_input < min]
      
      selectInput(inputId = "btn_start_year",
                  label = 'Start year',
                  choices = c("", start_year_input) #,
                  #  selected = '2010'
      )
    }
  })
  
  
  
  # -- Load FBS frozen ----
  
  frozen_data <- reactiveValues(FBS = data.table(),
                                SUA = data.table())
  
  live_data <- reactiveValues(FBS = data.table(),
                              SUAb = data.table(),
                              SUAu = data.table(),
                              Pop = data.table())
  
  observeEvent(input$btn_start_year, {
    
    if(input$btn_start_year != ""){
      sel_country <- country_input[country_input$label == input$btn_country, code]
      sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
      
      KeyFBS <- DatasetKey(domain = domainComm, dataset = datasetFBSfrozen, dimensions = list(
        geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
        measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                          GetCodeList(domainComm, datasetFBSfrozen,"measuredElementSuaFbs" )[,code]),
        measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                           GetCodeList(domainComm, datasetFBSfrozen,"measuredItemFaostat_L2" )[,code]),
        timePointYears = Dimension(name = "timePointYears", keys =  as.character(sel_years) )))
      
      withProgress(message = 'Frozen FBS data loading in progress',
                   value = 0, {
                     
                     Sys.sleep(0.25)
                     incProgress(0.25)
                     FBSfrozen <- GetData(KeyFBS)
                     Sys.sleep(0.75)
                     incProgress(0.95)
                   })
      
      frozen_data$FBS <- FBSfrozen[geographicAreaM49_fi == sel_country]
      
      if(localrun){
        if(CheckDebug()){
          library(faoswsModules)
          SETTINGS = ReadSettings("sws.yml")
          R_SWS_SHARE_PATH = SETTINGS[["share"]]
          SetClientFiles(SETTINGS[["certdir"]])
          GetTestEnvironment(baseUrl = SETTINGS[["server"]],
                             token = tokenFbs)
        }
      } else {
        R_SWS_SHARE_PATH = "Z:"
        SetClientFiles("srv/shiny-server/shinyFi_SUAFBS")
        GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                           token = tokenFbs)
      }
      
      KeyFBSfias <- DatasetKey(domain = domainComm, dataset = datasetFBSlive, dimensions = list(
        geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
        measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
                                          keys = GetCodeList(domainComm, datasetFBSlive,"measuredElementSuaFbs" )[,code]),
        measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
                                           keys = GetCodeList(domainComm, datasetFBSlive,"measuredItemFaostat_L2" )[,code]),
        timePointYears = Dimension(name = "timePointYears", keys = as.character(sel_years))))
      
      withProgress(message = 'FBS live data loading in progress',
                   value = 0, {
                     
                     Sys.sleep(0.25)
                     incProgress(0.25)
                     FBSfias <- GetData(KeyFBSfias)
                     Sys.sleep(0.75)
                     incProgress(0.95)
                   })
      
      live_data$FBS <- FBSfias[geographicAreaM49_fi == sel_country]
      ###
      
    }
  })
  
  
  #-- Element button ----
  
  output$btn_element_fbs <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(sel_country != "-") {
      selectInput(inputId = "btn_element_fbs",
                  label = 'FBS element',
                  choices = c("", element_input$label))
    }
    
  })
  
  
  #++ 1 Description tab ----
  source("tabs/descriptionTab1.R", local = TRUE)
  
  #-- Load SUA frozen ----
  
  observeEvent(input$tabs, {
    req(input$btn_start_year != '')
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    
    if(input$tabs == "SUA compare"){
      
      # ----
      # if(nrow(frozen_data$SUA) == 0){
      #   
      #   KeySUA <- DatasetKey(domain = "FisheriesCommodities", 
      #                        dataset = "fi_sua_balanced_control", 
      #                        dimensions = list(geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
      #                                          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
      #                                                                            GetCodeList("FisheriesCommodities", 
      #                                                                                        "fi_sua_balanced_control",
      #                                                                                        "measuredElementSuaFbs" )[,code]),
      #                                          measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
      #                                                                             GetCodeList("FisheriesCommodities", 
      #                                                                                         "fi_sua_balanced_control",
      #                                                                                         "measuredItemFaostat_L2" )[,code]),
      #                                          timePointYears = Dimension(name = "timePointYears", keys =  sel_years )))
      #   
      #   withProgress(message = 'Frozen SUA data loading in progress',
      #                value = 0, {
      #                  Sys.sleep(0.25)
      #                  incProgress(0.25)
      #                  SUAfrozen <- GetData(KeySUA)
      #                  Sys.sleep(0.75)
      #                  incProgress(0.95)
      #                })
      #   
      #   frozen_data$SUA <- SUAfrozen
      #   
      # } else if(nrow(frozen_data$SUA) > 0 &
      #           unique(frozen_data$SUA$geographicAreaM49_fi) != input$btn_country |
      #           min(unique(frozen_data$SUA$timePointYears)) != input$btn_start_year |
      #           max(unique(frozen_data$SUA$timePointYears)) != input$btn_year){
      #   
      #   KeySUA <- DatasetKey(domain = "FisheriesCommodities", 
      #                        dataset = "fi_sua_balanced_control", 
      #                        dimensions = list(geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
      #                                          measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
      #                                                                            GetCodeList("FisheriesCommodities", 
      #                                                                                        "fi_sua_balanced_control",
      #                                                                                        "measuredElementSuaFbs" )[,code]),
      #                                          measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
      #                                                                             GetCodeList("FisheriesCommodities", 
      #                                                                                         "fi_sua_balanced_control",
      #                                                                                         "measuredItemFaostat_L2" )[,code]),
      #                                          timePointYears = Dimension(name = "timePointYears", keys =  sel_years )))
      #   
      #   withProgress(message = 'Frozen SUA data loading in progress',
      #                value = 0, {
      #                  Sys.sleep(0.25)
      #                  incProgress(0.25)
      #                  SUAfrozen <- GetData(KeySUA)
      #                  Sys.sleep(0.75)
      #                  incProgress(0.95)
      #                })
      #   
      #   frozen_data$SUA <- SUAfrozen}
      # ----

      # ----
      # if(nrow(live_data$SUAb) == 0){
      #   # SUA live 
      #   
      #   if(localrun){
      #     if(CheckDebug()){
      #       library(faoswsModules)
      #       SETTINGS = ReadSettings("sws.yml")
      #       R_SWS_SHARE_PATH = SETTINGS[["share"]]
      #       SetClientFiles(SETTINGS[["certdir"]])
      #       GetTestEnvironment(baseUrl = SETTINGS[["server"]],
      #                          token = tokenSuaB)
      #     }
      #   } else {
      #     R_SWS_SHARE_PATH = "Z:"
      #     SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
      #     GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
      #                        token = tokenSuaB)
      #   }
      #   
      #   KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", 
      #                           dataset = "fi_sua_balanced", 
      #                           dimensions = list(geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", 
      #                                                                              keys = sel_country),
      #                                             measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
      #                                                                               keys = GetCodeList("FisheriesCommodities", 
      #                                                                                                  "fi_sua_balanced",
      #                                                                                                  "measuredElementSuaFbs")[,code]),
      #                                             measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
      #                                                                                GetCodeList("FisheriesCommodities", 
      #                                                                                            "fi_sua_balanced",
      #                                                                                            "measuredItemFaostat_L2" )[,code]),
      #                                             timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
      #   
      #   withProgress(message = 'SUA balanced data loading in progress',
      #                value = 0, {
      #                  Sys.sleep(0.25)
      #                  incProgress(0.25)
      #                  SUAbal <- GetData(KeySUAbal)
      #                  Sys.sleep(0.75)
      #                  incProgress(0.95)
      #                })
      #   
      #   
      #   live_data$SUAb <- SUAbal
      #   
      #   
      #   
      #   
      # } else if(nrow(live_data$SUAb) > 0 &
      #           unique(frozen_data$SUA$geographicAreaM49_fi) != input$btn_country |
      #           min(unique(frozen_data$SUA$timePointYears)) != input$btn_start_year |
      #           max(unique(frozen_data$SUA$timePointYears)) != input$btn_year){
      #   if(localrun){
      #     if(CheckDebug()){
      #       library(faoswsModules)
      #       SETTINGS = ReadSettings("sws.yml")
      #       R_SWS_SHARE_PATH = SETTINGS[["share"]]
      #       SetClientFiles(SETTINGS[["certdir"]])
      #       GetTestEnvironment(baseUrl = SETTINGS[["server"]],
      #                          token = tokenSuaB)
      #     }
      #   } else {
      #     R_SWS_SHARE_PATH = "Z:"
      #     SetClientFiles("/srv/shiny-server/shinyFisheriesCommodities")
      #     GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
      #                        token = tokenSuaB)
      #   }
      #   
      #   KeySUAbal <- DatasetKey(domain = "FisheriesCommodities", 
      #                           dataset = "fi_sua_balanced", 
      #                           dimensions = list(geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", 
      #                                                                              keys = sel_country),
      #                                             measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
      #                                                                               keys = GetCodeList("FisheriesCommodities", 
      #                                                                                                  "fi_sua_balanced",
      #                                                                                                  "measuredElementSuaFbs")[,code]),
      #                                             measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
      #                                                                                GetCodeList("FisheriesCommodities", 
      #                                                                                            "fi_sua_balanced",
      #                                                                                            "measuredItemFaostat_L2" )[,code]),
      #                                             timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
      #   
      #   withProgress(message = 'SUA balanced data loading in progress',
      #                value = 0, {
      #                  Sys.sleep(0.25)
      #                  incProgress(0.25)
      #                  SUAbal <- GetData(KeySUAbal)
      #                  Sys.sleep(0.75)
      #                  incProgress(0.95)
      #                })
      #   
      #   
      #   live_data$SUAb <- SUAbal
      #   
      # }
      # ----
      
      frozenB <- reloadData(data = frozen_data$SUA, 
                            keycountry = sel_country, 
                            minyear = input$btn_start_year, 
                            maxyear = input$btn_year,
                            keydomain = domainComm, 
                            keydataset = datasetSUABfrozen)
      
      if(!is.null(frozenB)){
        frozen_data$SUA <- frozenB
      }
      
      liveB <- reloadDataToken(data = live_data$SUAb, 
                               keycountry = sel_country, 
                               minyear = input$btn_start_year, 
                               maxyear = input$btn_year,
                               keydomain = domainComm, 
                               keydataset = datasetSUABlive,
                               keytoken = tokenSuaB)
      
      if(!is.null(liveB)){
        live_data$SUAb <- liveB
      }
      
    }
  })
  
  observeEvent(input$tabs, {
    
    req(input$btn_start_year)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    
    if(input$tabs == "SUA imbalances"){  
      liveU <- reloadDataToken(data = live_data$SUAu, 
                               keycountry = sel_country, 
                               minyear = input$btn_start_year, 
                               maxyear = input$btn_year,
                               keydomain = domainComm, 
                               keydataset = datasetSUAUlive,
                               keytoken = tokenSuaU)
      
      if(!is.null(liveU)){
        live_data$SUAu <- liveU
      }
    }
  })
  
  
  #-- FBS group button ----
  
  output$btn_group_fbs <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country)
    
    fbs_group_input <- merge(data.table(code = as.character(c(seq(10, 90, by = 10), 99))), 
                             groups_input , by = 'code')
    # selectInput
    checkboxGroupInput(inputId = "btn_group_fbs",
                       label = 'FBS group',
                       choices = c('All', fbs_group_input$label),
                       selected = NULL)#fbs_group_input$label)
    
  })
  
  #-- Element group button ----  
  output$btn_element_group <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country) #, input$btn_group_fbs)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(sel_country != "-") {
      radioButtons(inputId = "btn_element_group",
                   label = 'Element group',
                   inline = TRUE,
                   choices = elementGroups,
                   selected = NULL)
    }
    
  })
  
  #-- SUA element button ---- 
  
  output$btn_element_sua <- renderUI({
    # Country and year buttons required
    req(input$btn_country, input$btn_element_group)
    
    if(input$btn_element_group == 'All'){
      
      chosen <- sua_element_input$label
      
    } else if(input$btn_element_group == 'Primary'){
      
      chosen <- sua_element_input[code %in% primaryEl, ]$label
      
    } else if(input$btn_element_group == 'Nutrients') {
      
      chosen <- sua_element_input[code %in% NutrientsEl, ]$label
      
    } else if(input$btn_element_group == 'SUA'){
      
      chosen <- sua_element_input[code %in% SUAel, ]$label
      
    } else {
      chosen <- NULL
    }
    
    checkboxGroupInput(inputId = "btn_element_sua",
                       label = 'SUA element',
                       choices = sua_element_input$label,
                       selected = chosen)
    
  })
  
  #++ 2 Comparing SUA tab ----
  source("tabs/comparingSuaTab2.R", local = TRUE)
  
  #++ 2bis Imbalance tab ----
  source("tabs/imbalanceTab2.R", local = TRUE)
  
  #-- FBS group button Tab3 ----
  
  output$btn_group_fbs_tab3 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country)
    
    fbs_group_input <- merge(data.table(code = as.character(c(seq(10, 90, by = 10), 99))), groups_input , by = 'code')
    
    selectInput(inputId = "btn_group_fbs_tab3",
                label = 'FBS group',
                choices = c('',fbs_group_input$label),
                selected = input$btn_group_fbs)
    
  })
  
  #-- ICS product button Tab3 ----
  
  output$btn_ics_prod_tab3 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_group_fbs_tab3)
    # code of FBS group chosen
    group_sel <- groups_input[ label == input$btn_group_fbs_tab3]$code
    # ICS product in the chosen FBS group
    ICSinput_code <- l2l1[code_l1 == group_sel ]$code_l2
    ICSinput <- groups_input[ code %in% ICSinput_code]$label
    
    checkboxGroupInput(inputId = "btn_ics_prod_tab3",
                       label = 'ICS product',
                       choices = ICSinput,
                       selected = ICSinput)
    
  })
  
  #++ 3 FBS group by ICS product tab ----
  source("tabs/productTab3.R", local = TRUE)
  
  #-- FBS group button Tab4 ----
  
  output$btn_group_fbs_tab4 <- renderUI({
    
    # Country and year buttons required
    req( input$btn_country, input$btn_year, input$btn_start_year)
    
    fbs_group_input <- merge( data.table(code = as.character(c(seq(10, 90, by = 10), 99))), groups_input , by = 'code')
    
    selectInput(inputId = "btn_group_fbs_tab4",
                label = 'FBS group',
                choices = c('',fbs_group_input$label),
                selected = input$btn_group_fbs)
    
  })
  
  #-- ICS product button Tab4 ----
  
  output$btn_ics_prod_tab4 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_group_fbs_tab4, input$btn_year, input$btn_start_year)
    
    # code of FBS group chosen
    group_sel <- groups_input[ label == input$btn_group_fbs_tab4]$code
    # ICS product in the chosen FBS group
    ICSinput_code <- l2l1[code_l1 == group_sel ]$code_l2
    ICSinput <- groups_input[ code %in% ICSinput_code]$label
    
    selectInput(inputId = "btn_ics_prod_tab4",
                label = 'ICS product',
                choices = c('',ICSinput))
    
  })
  
  #-- If SUA not loaded yet ----
  observeEvent(input$btn_ics_prod_tab4, {
    req(input$btn_start_year)
    
    sel_country <- country_input[country_input$label == input$btn_country, code]
    sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
    
    frozenB <- reloadData(data = frozen_data$SUA, 
                          keycountry = sel_country, 
                          minyear = input$btn_start_year, 
                          maxyear = input$btn_year,
                          keydomain = domainComm, 
                          keydataset = datasetSUABfrozen)
    
    if(!is.null(frozenB)){
      frozen_data$SUA <- frozenB
    }
    
  })
  
  #-- SUA element group Tab4 ---- 
  
  output$btn_element_group_tab4 <- renderUI({
    
    # Country and year buttons required
    # req(input$btn_ics_prod_tab4)
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(sel_country != "-") {
      radioButtons(inputId = "btn_element_group_tab4",
                   label = 'Element group',
                   inline = TRUE,
                   choices = elementGroups,
                   selected = 'All')
    }
    
  })
  
  #-- SUA element Tab4 ---- 
  
  output$btn_element_sua_tab4 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_element_group_tab4) #, input$btn_ics_prod_tab4)
    SUAfrozen <- frozen_data$SUA
    SUAelem_input <- sua_element_input[ code %in% unique(SUAfrozen$measuredElementSuaFbs), ]
    
    if(input$btn_element_group_tab4 == 'All'){
      
      chosen <- SUAelem_input$label
      
    } else if(input$btn_element_group_tab4 == 'Primary'){
      
      chosen <- SUAelem_input[code %in% primaryEl, ]$label
      
    } else if(input$btn_element_group_tab4 == 'Nutrients') {
      
      chosen <- SUAelem_input[code %in% NutrientsEl, ]$label
      
    } else if(input$btn_element_group_tab4 == 'SUA'){
      
      chosen <- SUAelem_input[code %in% SUAel, ]$label
      
    } else {
      chosen <- NULL
    }
    
    checkboxGroupInput(inputId = "btn_element_sua_tab4",
                       label = 'SUA element',
                       choices = SUAelem_input$label,
                       selected = chosen)
    
  })
  
  #++ 4 SUA element Tab4 ----
  source("tabs/elementTab4.R", local = TRUE)
  # ############################################################################################
  #-- Place for updated datasets ----
  updated_data <- reactiveValues(SUAunbal = data.table(),
                                 SUAbal = data.table(),
                                 FBSfias = data.table(),
                                 FBSfaostat = data.table())
  # live_data <- reactiveValues(FBS = data.table(),
  #                             SUAb = data.table(),
  #                             SUAu = data.table())
  
  
  updated_mappings <- reactiveValues(GP = data.table(),
                                     CDB = data.table())
  
  updated_table <- reactiveValues(NegAv = data.table(),
                                  FPproblems = data.table())
  
  #-- FBS group button Tab5 ----
  
  output$btn_group_fbs_tab5 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    fbs_group_input <- merge(data.table(code = as.character(c(seq(10, 90, by = 10), 99))), groups_input , by = 'code')
    
    selectInput(inputId = "btn_group_fbs_tab5",
                label = 'FBS group',
                choices = c('',fbs_group_input$label),
                selected = input$btn_group_fbs,
                multiple = TRUE)
    
  })
  
  InitialDatasets <- reactiveValues(GP = data.table(),
                                    CDB = data.table())
  
  #++ 5 Global Production Tab5 ----
  source("tabs/gpTab5.R", local = TRUE)
  
  #-- FBS group button Tab6 ----
  
  output$btn_group_fbs_tab6 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    fbs_group_input <- merge(data.table(code = as.character(c(seq(10, 90, by = 10), 99))), groups_input , by = 'code')
    
    selectInput(inputId = "btn_group_fbs_tab6",
                label = 'FBS group',
                choices = c('',fbs_group_input$label),
                selected = input$btn_group_fbs,
                multiple = TRUE)
    
  })
  
  #-- ICS product button Tab6 ----
  
  output$btn_ics_prod_tab6 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year,
        input$btn_group_fbs_tab6)
    # code of FBS group chosen
    group_sel <- groups_input[ label %in% input$btn_group_fbs_tab6]$code
    # ICS product in the chosen FBS group
    ICSinput_code <- l2l1[code_l1 %in% group_sel ]$code_l2
    ICSinput <- groups_input[ code %in% ICSinput_code]$label
    
    selectInput(inputId = "btn_ics_prod_tab6",
                label = 'ICS product',
                choices = c('', ICSinput),
                multiple = TRUE)
    
  })
  
  #-- SUA element Tab6 ---- 
  
  output$btn_element_cdb_tab6 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year) #, input$btn_ics_prod_tab6)
    SUAfrozen <- frozen_data$SUA
    SUAelem_input <- sua_element_input[ code %in% c('5510', '5610', '5910', '5912'), ]
    # NOTE one of '5951', '5912' has to be changed!!!
    
    checkboxGroupInput(inputId = "btn_element_cdb_tab6",
                       label = 'SUA element',
                       choices = SUAelem_input$label,
                       selected = SUAelem_input$label)
    
  })
  
  ##++ 6 Commodity Tab6  ----
  source("tabs/cdbTab6.R", local = TRUE)
  
  source("tabs/linktable.R", local = TRUE)
  
  source("tabs/extrRates.R", local = TRUE)
  #-- Dataset choice Tab7 ---- 
  
  # output$btn_dataset_tab7 <- renderUI({
  #   
  #   # Country and year buttons required
  #   req(input$btn_country, input$btn_year, input$btn_start_year)
  #   
  #   checkboxGroupInput(inputId = "btn_dataset_tab7",
  #                      label = 'SUA element',
  #                      choices = c('fi_sua_balanced', 'fi_fbs_fias', 'fi_fbs_faostat'),
  #                      selected = 'fi_sua_balanced')
  #   
  # })
  
  #-- FBS group button Tab7 ----
  
  output$btn_group_fbs_tab7 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year)
    
    fbs_group_input <- merge(data.table(code = as.character(c(seq(10, 90, by = 10), 99))), groups_input , by = 'code')
    
    selectInput(inputId = "btn_group_fbs_tab7",
                label = 'FBS group',
                choices = fbs_group_input$label,
                selected = NULL, #input$btn_group_fbs,
                multiple = TRUE)
    
  })
  
  #-- ICS product button Tab7 ----
  
  output$btn_ics_prod_tab7 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_country, input$btn_year, input$btn_start_year) #, input$btn_group_fbs_tab7)
    # code of FBS group chosen
    # if(!is.null(input$btn_group_fbs_tab7)){
    group_sel <- groups_input[ label %in% input$btn_group_fbs_tab7]$code
    # ICS product in the chosen FBS group
    ICSinput_code <- l2l1[code_l1 %in% group_sel ]$code_l2
    ICSinput <- groups_input[ code %in% ICSinput_code]$label
    
    selectInput(inputId = "btn_ics_prod_tab7",
                label = 'ICS product',
                choices = c('All', ICSinput),
                selected = NULL,
                multiple = TRUE)
    # }
    
  })
  
  # #-- Load SUA balanced ----
  # 
  # live_data <- reactiveValues(SUA = data.table())
  # 
  # observeEvent(input$btn_ics_prod_tab7, {
  # 
  # sel_country <- country_input[country_input$label == input$btn_country, code]
  # sel_years <- as.character(as.numeric(input$btn_start_year):as.numeric(input$btn_year))
  # sel_ics_prod <- as.character(groups_input[label %in% input$btn_ics_prod_tab7]$code)
  # 
  # KeySUA <- DatasetKey(domain = "FisheriesCommodities", dataset = "fi_sua_balanced", dimensions = list(
  #   geographicAreaM49_fi = Dimension(name = "geographicAreaM49_fi", keys = sel_country),
  #   measuredElementSuaFbs = Dimension(name = "measuredElementSuaFbs", 
  #                                     keys = GetCodeList("FisheriesCommodities", "fi_fbs_fias_control","measuredElementSuaFbs" )[,code]),
  #   measuredItemFaostat_L2 = Dimension(name = "measuredItemFaostat_L2", 
  #                                      keys = sel_ics_prod),
  #   timePointYears = Dimension(name = "timePointYears", keys = sel_years )))
  # 
  # withProgress(message = 'SUA balanced data loading in progress', 
  #              value = 0, {
  #                
  #                Sys.sleep(0.25)
  #                incProgress(0.25)
  #                SUA <- GetData(KeySUA)
  #                Sys.sleep(0.75)
  #                incProgress(0.95)
  #              })
  # 
  # live_data$SUA <- SUA
  # 
  # })
  
  #-- SUA element group Tab7 ----  
  
  output$btn_element_group_tab7 <- renderUI({
    
    # Country and year buttons required
    rep(input$btn_country) # req(input$btn_ics_prod_tab7)
    #if(!is.null(input$btn_group_fbs_tab7)){
    sel_country <- country_input[country_input$label == input$btn_country, code]
    
    if(sel_country != "-") {
      radioButtons(inputId = "btn_element_group_tab7",
                   label = 'Element group',
                   inline = TRUE,
                   choices = elementGroups)
    }
    # }
  })
  
  #-- SUA element Tab7 ---- 
  
  output$btn_sua_elem_tab7 <- renderUI({
    
    # Country and year buttons required
    req(input$btn_element_group_tab7)
    # if(!is.null(input$btn_group_fbs_tab7)){
    if(input$btn_element_group_tab7 == 'All'){
      
      chosen <- sua_element_input$label
      
    } else if(input$btn_element_group_tab7 == 'Primary'){
      
      chosen <- sua_element_input[code %in% primaryEl, ]$label
      
    } else if(input$btn_element_group_tab7 == 'Nutrients') {
      
      chosen <- sua_element_input[code %in% NutrientsEl, ]$label
      
    } else if(input$btn_element_group_tab7 == 'SUA'){
      
      chosen <- sua_element_input[code %in% SUAel, ]$label
      
    } else {
      chosen <- NULL
    }
    
    selectInput(inputId = "btn_sua_elem_tab7",
                label = 'SUA element',
                choices = sua_element_input$label,
                selected = chosen,
                multiple = TRUE)
    # }
  })
  
  
  #++ 7 Validation Tab6  ----
  source("tabs/validationTab7.R", local = TRUE)
  
  #++ 8 Consequences tab ----
  source("tabs/consequencesTab8.R", local = TRUE)
  
  
  output$xxx = renderTable(
    
    suaTab_reac()$sua2show
    
  )
  
})