library(dplyr)
library(shiny)
library(fabletools)
library(tsibbletalk)
library(ggplot2)
library(plotly)
library(patchwork)
library(forecast)
library(fable)
library(fabletools)
library(gt)
library(shinythemes)
library(shinyWidgets)
library(fresh)
library(gt)
library(data.table)

ui <- fluidPage(
  
  titlePanel(h1("Forecast Dengue Incidence in Sri Lanka", align = "center")),
  theme = shinytheme("cyborg"), 
  sidebarPanel( 
    fileInput("i_file", 
              "Upload your CSV file",
              # A character vector of MIME types; gives the browser a hint of what kind of files the server is expecting.
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
    br(),
    column(12, dateRangeInput('dateRange',
                              label = 'Filter Historical Data by Date',
                              start = "2006-12-29",
                              end = "2020-12-18",
                              min = "2006-12-29",
                              max = "2020-12-18"),),
    tags$hr(),
    radioButtons("structure", h5("Hierarchical structure:"),
                 list( "Spatial Hierarchy" = "SH",
                       "Temporal hierarchy" = "TH"
                 )),
    br(),
    radioButtons("Method", h5("Select the forecasting method:"),
                 list( "Base forecasting" = "BF",
                       "Hierarchical forecasting" = "HF"
                 )),
    br(),
    selectInput("period", h5("Select forecast period:"), 
                choices = c("1 year", "2 years", "3 years", "4 years", "5 years")),
    br(),
    downloadButton("downloadData", "Download"),
    h6("Click download button and wait until forecasts are generated.")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Description",
               
               helpText(h6("This dengue incidence forecasting app provides an overview of the dengue epidemic 
                           in Sri Lanka from existing data. This app is built with R software.")),
               tags$h4("Code"),
               helpText(h6("The code of this app is available on, ",
                           tags$a(href="https://github.com/SamudraMadushani/DengueSriLankaApp.git", 
                                  "Github"),)),
               helpText(h4("Data")),
               helpText(h6("Upload the csv file with column names EndDate, Dengue, Districts and Province.")),
               helpText(h6("'EndDate' column represents the end date of the week.")),
               helpText(h6("'Dengue' column represents the district-wise dengue counts of each week.")),
               helpText(h6("'Districts' column represents districts that correspond to dengue counts.")),
               helpText(h6("'Province' column represents province that corresponds to dengue counts.")),
               helpText(h4("Outputs")),
               helpText(h6("Firstly, this app provides the time series plots 
                           corresponding to series associated with the spatial or 
                           temporal hierarchical structure of Sri Lanka. 
                           You can obtain the individual series forecasts from
                           ETS, NAIVE, SNAIVE, and average approaches. 
                           These approaches are called base forecasts. Furthermore, 
                           you can obtain the hierarchical forecasts based on 
                           spatial or temporal hierarchical structure in order to 
                           obtain aligned decisions for the given forecast period 
                           and selected historical data are used for the modeling process."))),
      tabPanel("Data",
               column(8,
                      dataTableOutput('my_table')
               )),
      tabPanel("Visualization",plotlyOutput("summary1")),
      tabPanel("Forecasts",
               column(8,
                      gt_output(outputId = "table")))),
    
  )
  
)
server <- function(input, output, session) {
  DengueTEST <- reactive({
    inFile <- input$i_file
    
    if (is.null(inFile)){return(NULL)}
    df <- read.csv(inFile$datapath)
    Date<-df$EndDate
    Province<-df$Province
    Districts<-df$Districts
    Counts<-as.integer(df$Dengue)
    df<-data.frame(Date,Counts,Districts,Province)
    return(df)
  })
  
  output$my_table  <- renderDataTable({
    # Filter the data
    DengueTEST<-DengueTEST() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
    DengueTEST
  })
  
  output$summary1 <- renderPlotly({
    if(input$structure=="SH"){
      DengueTEST<-DengueTEST() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
      
      DengueTEST$Week<-tsibble::yearweek(DengueTEST$Date)
      
      DataDengueTEST<- DengueTEST%>% 
        as_tsibble(key=c(Province,Districts),index=Week)
      
      ProvinceDistricts_shared <- DataDengueTEST%>%
        as_shared_tsibble(spec = (Province/Districts))
      
      p01<- plotly_key_tree(ProvinceDistricts_shared, height = 700, width = 600)
      p1n <- ProvinceDistricts_shared %>%
        ggplot(aes(x = Date, y = Counts)) +
        geom_line(aes(group = Districts), alpha = 0.5,color='darkblue')
      
      DIS_DataDengueTEST<-DataDengueTEST %>%fabletools::aggregate_key(Province/Districts,Counts=sum(Counts)) #ARRANGE TEST SET
      SL<-DIS_DataDengueTEST%>%
        dplyr::filter(is_aggregated(Province) & is_aggregated(Districts)) %>%
        autoplot(Counts,col="#e7298a") + labs(y = "Counts",x="Week")
      
      DIS_AGG_SRS<-DIS_DataDengueTEST %>%
        dplyr::filter(is_aggregated(Districts)& !is_aggregated(Province)) %>%
        autoplot(Counts) +
        labs(y = "Counts",x="Week") +
        theme(legend.position = "none")
      
      subplot(p01,
              subplot(
                ggplotly(p1n, tooltip = "Districts", width = 900),
                ggplotly(DIS_AGG_SRS, width = 900),
                ggplotly(SL, width = 900),
                #p2nN,
                nrows = 3),
              widths = c(.5, .5)) %>%
        highlight(dynamic = TRUE)
    }
    else{
      DengueTEST<-DengueTEST() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
      
      DengueTEST$Week<-tsibble::yearweek(DengueTEST$Date)
      
      DataDengueTEST<- DengueTEST%>% 
        as_tsibble(key=c(Province,Districts),index=Week)
      
      Weekly_Dengue<-DataDengueTEST%>%
        tsibble::index_by(WD=~yearweek(.))%>%
        dplyr::summarise(Counts=sum(Counts))
      #Weekly_Dengue<-Weekly_Dengue%>%dplyr::filter(WD!=53)
      
      Dengue_total_data<-c(Weekly_Dengue$Counts)
      
      Dengue_total_data<- ts(Dengue_total_data, freq=52, start=c(lubridate::year(DengueTEST$Date[1]),lubridate::week(DengueTEST$Date[1])))
      aggts_total = thief::tsaggregates(Dengue_total_data)
      W<-autoplot(aggts_total[[1]],col="#e7298a") 
      BW<-autoplot(aggts_total[[2]],col="#1b9e77") 
      M<-autoplot(aggts_total[[3]],col="#d95f02") 
      Q<-autoplot(aggts_total[[4]],col="#7570b3") 
      SA<-autoplot(aggts_total[[5]],col="#66a61e") 
      A<-autoplot(aggts_total[[6]],col="#66a61e") 
      #TEM<-(W/BW/M/Q/Y)
      #TEM<-(W|BW|M)/(Q|Y)
      a <- list(text = "Weekly",showarrow = FALSE)
      b <- list(text = "Bi-weekly",showarrow = FALSE)
      c <- list(text = "Monthly",showarrow = FALSE)
      d <- list(text = "Quarterly",showarrow = FALSE)
      e <- list(text = "Semi-annual",showarrow = FALSE)
      f <- list(text = "Annual",showarrow = FALSE)
      
      subplot(ggplotly(W, width = 900,height = 600)%>%layout(annotations = a),
              ggplotly(BW, width = 900,height = 600)%>%layout(annotations = b),
              ggplotly(M, width = 900,height = 600)%>%layout(annotations = c),
              ggplotly(Q, width = 900,height = 600)%>%layout(annotations = d),
              ggplotly(SA, width = 900,height = 600)%>%layout(annotations = e),
              ggplotly(A, width = 900,height = 600)%>%layout(annotations = f),
              nrows = 3)
    }
  })
  datasetInput <- reactive({
    switch(
      "1 year" = 1,
      "2 years" = 2,
      "3 years" = 3,
      "4 years" = 4,
      "5 years" = 5)
  })
  
  Fvalues <- reactive({
    
    if(input$structure=="SH"){
      
      DengueTEST<-DengueTEST() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
      
      DengueTEST$Week<-tsibble::yearweek(DengueTEST$Date)
      
      DataDengueTEST<- DengueTEST%>% 
        as_tsibble(key=c(Province,Districts),index=Week)
      
      if(input$Method=="BF"){
        fit_TRAIN <- DataDengueTEST%>%
          model(snaive=SNAIVE(Counts),naive=NAIVE(Counts),ets=ETS(Counts),avg = MEAN(Counts))
        Fvalues<-fit_TRAIN %>% forecast(h=input$period)
        Fvalues<-data.frame(Fvalues)
        Fvalues$.mean<-round(Fvalues$.mean,0)
        Fvalues<-Fvalues[,c(1,2,3,4,6)]
      }
      else{
        fit_TRAIN <- DataDengueTEST%>%
          model(snaive=SNAIVE(Counts),naive=NAIVE(Counts),ets=ETS(Counts),avg = MEAN(Counts))%>%
          fabletools::reconcile(
            bu_ets = bottom_up(ets),
            #top_ets=top_down(ets),
            ols_ets=min_trace(ets,method="ols"),
            #MinT_ets=min_trace(ets,method="mint_shrink"),
            wls_var_ets = min_trace(ets,method="wls_var"),
            wls_struct_ets=min_trace(ets,method="wls_struct")
            #MinT_cov_ets=min_trace(ets,method="mint_cov")
          )
        # Forecast value calculation
        Fvalues<-fit_TRAIN %>% forecast(h=input$period)
        Fvalues<-data.frame(Fvalues)
        Fvalues = data.table(Fvalues)
        Fvalues$.mean<-round(Fvalues$.mean,0)
        Fvalues<-Fvalues[,c(1,2,3,4,6)]
      }  
    }
    else{
      
      DengueTEST<-DengueTEST() %>% filter(Date >= input$dateRange[1] & Date <= input$dateRange[2])
      
      DengueTEST$Week<-tsibble::yearweek(DengueTEST$Date)
      
      DataDengueTEST<- DengueTEST%>% 
        as_tsibble(key=c(Province,Districts),index=Week)
      
      Weekly_Dengue<-DataDengueTEST%>%
        tsibble::index_by(WD=~yearweek(.))%>%
        dplyr::summarise(Counts=sum(Counts))
      #Weekly_Dengue<-Weekly_Dengue%>%dplyr::filter(WD!=53)
      
      Dengue_total_data<-c(Weekly_Dengue$Counts)
      
      Dengue_total_data<- ts(Dengue_total_data, freq=52, start=c(lubridate::year(DengueTEST$Date[1]),lubridate::week(DengueTEST$Date[1])))
      aggts_total = thief::tsaggregates(Dengue_total_data)
      base <- list()
      if(input$Method=="BF"){
        BASE_TemporalHierachicalForecasting<-function(Dengue_total_data,fcastsFUN)
        {
          aggts_total = thief::tsaggregates(Dengue_total_data)
          base1 <- list()
          base2 <- list()
          base3 <- list()
          base4 <- list()
          base5 <- list()
          base6 <- list()
          if(input$period=="1 year"){
            base1<- forecast(fcastsFUN(aggts_total[[1]]), h=1*frequency(aggts_total[[1]]))
            base2<- forecast(fcastsFUN(aggts_total[[2]]), h=1*frequency(aggts_total[[2]]))
            base3<- forecast(fcastsFUN(aggts_total[[3]]), h=1*frequency(aggts_total[[3]]))
            base4<- forecast(fcastsFUN(aggts_total[[4]]), h=1*frequency(aggts_total[[4]]))
            base5<- forecast(fcastsFUN(aggts_total[[5]]), h=1*frequency(aggts_total[[5]]))
            base6<- forecast(fcastsFUN(aggts_total[[6]]), h=1*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:length(m1)){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            
            name2 <- c("BiWeek_1")
            for(i in 2:length(m2)){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            
            name3 <- c("Month_1")
            for(i in 2:length(m3)){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            
            name4 <- c("Quarter_1")
            for(i in 2:length(m4)){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            
            name5 <- c("SemiAnnual_1")
            for(i in 2:length(m5)){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            
            name6<-"Annual_1"
          }
          else if(input$period=="2 years"){
            base1<- forecast(fcastsFUN(aggts_total[[1]]), h=2*frequency(aggts_total[[1]]))
            base2<- forecast(fcastsFUN(aggts_total[[2]]), h=2*frequency(aggts_total[[2]]))
            base3<- forecast(fcastsFUN(aggts_total[[3]]), h=2*frequency(aggts_total[[3]]))
            base4<- forecast(fcastsFUN(aggts_total[[4]]), h=2*frequency(aggts_total[[4]]))
            base5<- forecast(fcastsFUN(aggts_total[[5]]), h=2*frequency(aggts_total[[5]]))
            base6<- forecast(fcastsFUN(aggts_total[[6]]), h=2*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,2)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,2)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,2)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,2)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,2)
            
            name6<-"Annual_1"
            name6<-rep(name6,2)
          }
          else if(input$period=="3 years"){
            base1<- forecast(fcastsFUN(aggts_total[[1]]), h=3*frequency(aggts_total[[1]]))
            base2<- forecast(fcastsFUN(aggts_total[[2]]), h=3*frequency(aggts_total[[2]]))
            base3<- forecast(fcastsFUN(aggts_total[[3]]), h=3*frequency(aggts_total[[3]]))
            base4<- forecast(fcastsFUN(aggts_total[[4]]), h=3*frequency(aggts_total[[4]]))
            base5<- forecast(fcastsFUN(aggts_total[[5]]), h=3*frequency(aggts_total[[5]]))
            base6<- forecast(fcastsFUN(aggts_total[[6]]), h=3*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,3)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,3)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,3)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,3)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,3)
            
            name6<-"Annual_1"
            name6<-rep(name6,3)
          }
          else if(input$period=="4 years"){
            base1<- forecast(fcastsFUN(aggts_total[[1]]), h=4*frequency(aggts_total[[1]]))
            base2<- forecast(fcastsFUN(aggts_total[[2]]), h=4*frequency(aggts_total[[2]]))
            base3<- forecast(fcastsFUN(aggts_total[[3]]), h=4*frequency(aggts_total[[3]]))
            base4<- forecast(fcastsFUN(aggts_total[[4]]), h=4*frequency(aggts_total[[4]]))
            base5<- forecast(fcastsFUN(aggts_total[[5]]), h=4*frequency(aggts_total[[5]]))
            base6<- forecast(fcastsFUN(aggts_total[[6]]), h=4*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,4)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,4)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,4)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,4)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,4)
            
            name6<-"Annual_1"
            name6<-rep(name6,4)
          }
          else if(input$period=="5 years"){
            base1<- forecast(fcastsFUN(aggts_total[[1]]), h=5*frequency(aggts_total[[1]]))
            base2<- forecast(fcastsFUN(aggts_total[[2]]), h=5*frequency(aggts_total[[2]]))
            base3<- forecast(fcastsFUN(aggts_total[[3]]), h=5*frequency(aggts_total[[3]]))
            base4<- forecast(fcastsFUN(aggts_total[[4]]), h=5*frequency(aggts_total[[4]]))
            base5<- forecast(fcastsFUN(aggts_total[[5]]), h=5*frequency(aggts_total[[5]]))
            base6<- forecast(fcastsFUN(aggts_total[[6]]), h=5*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,5)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,5)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,5)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,5)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,5)
            
            name6<-"Annual_1"
            name6<-rep(name6,5)
          }
          #Weekly results
          
          l1<-as.double(round(base1$lower[,1],2))
          u1<-as.double(round(base1$upper[,1],2))
          mth1<-c(rep(base1$method,length(m1)))
          # Bi-Weekly results
          
          l2<-as.double(round(base2$lower[,1],2))
          u2<-as.double(round(base2$upper[,1],2))
          mth2<-c(rep(base1$method,length(m2)))
          # Monthly results
          
          l3<-as.double(round(base3$lower[,1],2))
          u3<-as.double(round(base3$upper[,1],2))
          mth3<-c(rep(base1$method,length(m3)))
          #Quarterly results
          
          l4<-as.double(round(base4$lower[,1],2))
          u4<-as.double(round(base4$upper[,1],2))
          mth4<-c(rep(base1$method,length(m4)))
          # SemiAnnual results
          
          l5<-as.double(round(base5$lower[,1],2))
          u5<-as.double(round(base5$upper[,1],2))
          mth5<-c(rep(base1$method,length(m5)))
          # Annual results
          
          l6<-as.double(round(base6$lower[,1],2))
          u6<-as.double(round(base6$upper[,1],2))
          mth6<-c(rep(base1$method,length(m6)))
          # data set
          nameA<-c(name1,name2,name3,name4,name5,name6)
          m<-c(m1,m2,m3,m4,m5,m6)
          l<-c(l1,l2,l3,l4,l5,l6)
          u<-c(u1,u2,u3,u4,u5,u6)
          mth<-c(mth1,mth2,mth3,mth4,mth5,mth6)
          
          thf<-data.frame(nameA,mth,m,l,u)
          colnames(thf)<-c("Time","Method","Point forecast","Lo 80","Hi 80")
          return(thf)
        }
        
        AAetsB<-BASE_TemporalHierachicalForecasting(Dengue_total_data,ets)
        
        BASE_TemporalHierachicalForecastingNOmodel<-function(Dengue_total_data,fcastsFUN)
        {
          aggts_total = thief::tsaggregates(Dengue_total_data)
          base1 <- list()
          base2 <- list()
          base3 <- list()
          base4 <- list()
          base5 <- list()
          base6 <- list()
          if(input$period=="1 year"){
            base1 <- fcastsFUN(aggts_total[[1]], h=1*frequency(aggts_total[[1]]))
            base2 <- fcastsFUN(aggts_total[[2]], h=1*frequency(aggts_total[[2]]))
            base3 <- fcastsFUN(aggts_total[[3]], h=1*frequency(aggts_total[[3]]))
            base4 <- fcastsFUN(aggts_total[[4]], h=1*frequency(aggts_total[[4]]))
            base5 <- fcastsFUN(aggts_total[[5]], h=1*frequency(aggts_total[[5]]))
            base6 <- fcastsFUN(aggts_total[[6]], h=1*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:length(m1)){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            
            name2 <- c("BiWeek_1")
            for(i in 2:length(m2)){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            
            name3 <- c("Month_1")
            for(i in 2:length(m3)){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            
            name4 <- c("Quarter_1")
            for(i in 2:length(m4)){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            
            name5 <- c("SemiAnnual_1")
            for(i in 2:length(m5)){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            
            name6<-"Annual_1"
          }
          else if(input$period=="2 years"){
            base1 <- fcastsFUN(aggts_total[[1]], h=2*frequency(aggts_total[[1]]))
            base2 <- fcastsFUN(aggts_total[[2]], h=2*frequency(aggts_total[[2]]))
            base3 <- fcastsFUN(aggts_total[[3]], h=2*frequency(aggts_total[[3]]))
            base4 <- fcastsFUN(aggts_total[[4]], h=2*frequency(aggts_total[[4]]))
            base5 <- fcastsFUN(aggts_total[[5]], h=2*frequency(aggts_total[[5]]))
            base6 <- fcastsFUN(aggts_total[[6]], h=2*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,2)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,2)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,2)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,2)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,2)
            
            name6<-"Annual_1"
            name6<-rep(name6,2)
          }
          else if(input$period=="3 years"){
            base1 <- fcastsFUN(aggts_total[[1]], h=3*frequency(aggts_total[[1]]))
            base2 <- fcastsFUN(aggts_total[[2]], h=3*frequency(aggts_total[[2]]))
            base3 <- fcastsFUN(aggts_total[[3]], h=3*frequency(aggts_total[[3]]))
            base4 <- fcastsFUN(aggts_total[[4]], h=3*frequency(aggts_total[[4]]))
            base5 <- fcastsFUN(aggts_total[[5]], h=3*frequency(aggts_total[[5]]))
            base6 <- fcastsFUN(aggts_total[[6]], h=3*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,3)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,3)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,3)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,3)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,3)
            
            name6<-"Annual_1"
            name6<-rep(name6,3)
          }
          else if(input$period=="4 years"){
            base1 <- fcastsFUN(aggts_total[[1]], h=4*frequency(aggts_total[[1]]))
            base2 <- fcastsFUN(aggts_total[[2]], h=4*frequency(aggts_total[[2]]))
            base3 <- fcastsFUN(aggts_total[[3]], h=4*frequency(aggts_total[[3]]))
            base4 <- fcastsFUN(aggts_total[[4]], h=4*frequency(aggts_total[[4]]))
            base5 <- fcastsFUN(aggts_total[[5]], h=4*frequency(aggts_total[[5]]))
            base6 <- fcastsFUN(aggts_total[[6]], h=4*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,4)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,4)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,4)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,4)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,4)
            
            name6<-"Annual_1"
            name6<-rep(name6,4)
          }
          else if(input$period=="5 years"){
            base1 <- fcastsFUN(aggts_total[[1]], h=5*frequency(aggts_total[[1]]))
            base2 <- fcastsFUN(aggts_total[[2]], h=5*frequency(aggts_total[[2]]))
            base3 <- fcastsFUN(aggts_total[[3]], h=5*frequency(aggts_total[[3]]))
            base4 <- fcastsFUN(aggts_total[[4]], h=5*frequency(aggts_total[[4]]))
            base5 <- fcastsFUN(aggts_total[[5]], h=5*frequency(aggts_total[[5]]))
            base6 <- fcastsFUN(aggts_total[[6]], h=5*frequency(aggts_total[[6]]))
            m1<-round(base1$mean,0)
            m2<-round(base2$mean,0)
            m3<-round(base3$mean,0)
            m4<-round(base4$mean,0)
            m5<-round(base5$mean,0)
            m6<-round(base6$mean,0)
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,5)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,5)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,5)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,5)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,5)
            
            name6<-"Annual_1"
            name6<-rep(name6,5)
          }
          #Weekly results
          
          l1<-as.double(round(base1$lower[,1],2))
          u1<-as.double(round(base1$upper[,1],2))
          mth1<-c(rep(base1$method,length(m1)))
          # Bi-Weekly results
          
          l2<-as.double(round(base2$lower[,1],2))
          u2<-as.double(round(base2$upper[,1],2))
          mth2<-c(rep(base1$method,length(m2)))
          # Monthly results
          
          l3<-as.double(round(base3$lower[,1],2))
          u3<-as.double(round(base3$upper[,1],2))
          mth3<-c(rep(base1$method,length(m3)))
          #Quarterly results
          
          l4<-as.double(round(base4$lower[,1],2))
          u4<-as.double(round(base4$upper[,1],2))
          mth4<-c(rep(base1$method,length(m4)))
          # SemiAnnual results
          
          l5<-as.double(round(base5$lower[,1],2))
          u5<-as.double(round(base5$upper[,1],2))
          mth5<-c(rep(base1$method,length(m5)))
          # Annual results
          
          l6<-as.double(round(base6$lower[,1],2))
          u6<-as.double(round(base6$upper[,1],2))
          mth6<-c(rep(base1$method,length(m6)))
          # data set
          nameA<-c(name1,name2,name3,name4,name5,name6)
          m<-c(m1,m2,m3,m4,m5,m6)
          l<-c(l1,l2,l3,l4,l5,l6)
          u<-c(u1,u2,u3,u4,u5,u6)
          mth<-c(mth1,mth2,mth3,mth4,mth5,mth6)
          
          thf<-data.frame(nameA,mth,m,l,u)
          colnames(thf)<-c("Time","Method","Point forecast","Lo 80","Hi 80")
          return(thf)
        }
        
        AAnaiveB<-BASE_TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::naive)
        AAsnaiveB<-BASE_TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::snaive)
        AAavgB<-BASE_TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::meanf)
        
        Fvalues<-data.frame(AAnaiveB,AAsnaiveB,AAavgB,AAetsB)
        Fvalues = data.table(Fvalues)
        
      }
      else{
        
        
        TemporalHierachicalForecasting<-function(Dengue_total_data,fcastsFUN)
        {
          aggts_total = thief::tsaggregates(Dengue_total_data)
          base <- list()
          
          for(i in seq_along(aggts_total)){
            if(input$period=="1 year"){
              base[[i]] <- forecast(fcastsFUN(aggts_total[[i]]), h=1*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              
              name6<-"Annual_1"
              
            }
            else if(input$period=="2 years"){
              base[[i]] <- forecast(fcastsFUN(aggts_total[[i]]), h=2*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              name1<-rep(name1,2)
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              name2<-rep(name2,2)
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              name3<-rep(name3,2)
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              name4<-rep(name4,2)
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              name5<-rep(name5,2)
              
              name6<-"Annual_1"
              name6<-rep(name6,2)
            }
            else if(input$period=="3 years"){
              base[[i]] <- forecast(fcastsFUN(aggts_total[[i]]), h=3*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              name1<-rep(name1,3)
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              name2<-rep(name2,3)
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              name3<-rep(name3,3)
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              name4<-rep(name4,3)
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              name5<-rep(name5,3)
              
              name6<-"Annual_1"
              name6<-rep(name6,3) 
            }
            else if(input$period=="4 years"){
              base[[i]] <- forecast(fcastsFUN(aggts_total[[i]]), h=4*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              name1<-rep(name1,4)
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              name2<-rep(name2,4)
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              name3<-rep(name3,4)
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              name4<-rep(name4,4)
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              name5<-rep(name5,4)
              
              name6<-"Annual_1"
              name6<-rep(name6,4) 
            }
            else if(input$period=="5 years"){
              base[[i]] <- forecast(fcastsFUN(aggts_total[[i]]), h=5*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              name1<-rep(name1,5)
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              name2<-rep(name2,5)
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              name3<-rep(name3,5)
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              name4<-rep(name4,5)
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              name5<-rep(name5,5)
              
              name6<-"Annual_1"
              name6<-rep(name6,5)
            }
          }
          reconciled <- thief::reconcilethief(base)
          nameA<-c(name1,name2,name3,name4,name5,name6)
          m1<-round(reconciled[[1]]$mean,0)
          m2<-round(reconciled[[2]]$mean,0)
          m3<-round(reconciled[[3]]$mean,0)
          m4<-round(reconciled[[4]]$mean,0)
          m5<-round(reconciled[[5]]$mean,0)
          m6<-round(reconciled[[6]]$mean,0)
          m<-c(m1,m2,m3,m4,m5,m6)
          l1<-round(reconciled[[1]]$lower[,1],2)
          l2<-round(reconciled[[2]]$lower[,1],2)
          l3<-round(reconciled[[3]]$lower[,1],2)
          l4<-round(reconciled[[4]]$lower[,1],2)
          l5<-round(reconciled[[5]]$lower[,1],2)
          l6<-round(reconciled[[6]]$lower[,1],2)
          l<-c(l1,l2,l3,l4,l5,l6)
          u1<-round(reconciled[[1]]$upper[,1],2)
          u2<-round(reconciled[[2]]$upper[,1],2)
          u3<-round(reconciled[[3]]$upper[,1],2)
          u4<-round(reconciled[[4]]$upper[,1],2)
          u5<-round(reconciled[[5]]$upper[,1],2)
          u6<-round(reconciled[[6]]$upper[,1],2)
          u<-c(u1,u2,u3,u4,u5,u6)
          mth1<-c(rep(reconciled[[1]]$method,length(m1)))
          mth2<-c(rep(reconciled[[2]]$method,length(m2)))
          mth3<-c(rep(reconciled[[3]]$method,length(m3)))
          mth4<-c(rep(reconciled[[4]]$method,length(m4)))
          mth5<-c(rep(reconciled[[5]]$method,length(m5)))
          mth6<-c(rep(reconciled[[6]]$method,length(m6)))
          mth<-c(mth1,mth2,mth3,mth4,mth5,mth6)
          thf<-data.frame(nameA,mth,m,l,u)
          colnames(thf)<-c("Time","Method","Point forecast","Lo 80","Hi 80")
          return(thf)
        }
        AAarima<-TemporalHierachicalForecasting(Dengue_total_data,forecast::auto.arima)
        AAets<-TemporalHierachicalForecasting(Dengue_total_data,forecast::ets)
        
        TemporalHierachicalForecastingNOmodel<-function(Dengue_total_data,fcastsFUN)
        {
          aggts_total = thief::tsaggregates(Dengue_total_data)
          base <- list()
          for(i in seq_along(aggts_total))
            if(input$period=="1 year"){
              base[[i]] <- fcastsFUN(aggts_total[[i]], h=1*frequency(aggts_total[[i]]))
              name1 <- c("Week_1")
              for(i in 2:52){
                names11<-paste("Week_",i)
                name1<-c(name1,names11)
              }
              
              name2 <- c("BiWeek_1")
              for(i in 2:26){
                names12<-paste("BiWeek_",i)
                name2<-c(name2,names12)
              }
              
              name3 <- c("Month_1")
              for(i in 2:13){
                names13<-paste("Month_",i)
                name3<-c(name3,names13)
              }
              
              name4 <- c("Quarter_1")
              for(i in 2:4){
                names14<-paste("Quarter_",i)
                name4<-c(name4,names14)
              }
              
              name5 <- c("SemiAnnual_1")
              for(i in 2){
                names15<-paste("SemiAnnual_",i)
                name5<-c(name5,names15)
              }
              
              name6<-"Annual_1"
            }
          else if(input$period=="2 years"){
            base[[i]] <- fcastsFUN(aggts_total[[i]], h=2*frequency(aggts_total[[i]]))
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,2)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,2)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,2)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,2)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,2)
            
            name6<-"Annual_1"
            name6<-rep(name6,2)
          }
          else if(input$period=="3 years"){
            base[[i]] <- fcastsFUN(aggts_total[[i]], h=3*frequency(aggts_total[[i]]))
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,3)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,3)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,3)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,3)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,3)
            
            name6<-"Annual_1"
            name6<-rep(name6,3)
          }
          else if(input$period=="4 years"){
            base[[i]] <- fcastsFUN(aggts_total[[i]], h=4*frequency(aggts_total[[i]]))
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,4)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,4)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,4)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,4)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,4)
            
            name6<-"Annual_1"
            name6<-rep(name6,4)
          }
          else if(input$period=="5 years"){
            base[[i]] <- fcastsFUN(aggts_total[[i]], h=5*frequency(aggts_total[[i]]))
            name1 <- c("Week_1")
            for(i in 2:52){
              names11<-paste("Week_",i)
              name1<-c(name1,names11)
            }
            name1<-rep(name1,5)
            
            name2 <- c("BiWeek_1")
            for(i in 2:26){
              names12<-paste("BiWeek_",i)
              name2<-c(name2,names12)
            }
            name2<-rep(name2,5)
            
            name3 <- c("Month_1")
            for(i in 2:13){
              names13<-paste("Month_",i)
              name3<-c(name3,names13)
            }
            name3<-rep(name3,5)
            
            name4 <- c("Quarter_1")
            for(i in 2:4){
              names14<-paste("Quarter_",i)
              name4<-c(name4,names14)
            }
            name4<-rep(name4,5)
            
            name5 <- c("SemiAnnual_1")
            for(i in 2){
              names15<-paste("SemiAnnual_",i)
              name5<-c(name5,names15)
            }
            name5<-rep(name5,5)
            
            name6<-"Annual_1"
            name6<-rep(name6,5)
          }
          reconciled <- thief::reconcilethief(base)
          nameA<-c(name1,name2,name3,name4,name5,name6)
          m1<-round(reconciled[[1]]$mean,0)
          m2<-round(reconciled[[2]]$mean,0)
          m3<-round(reconciled[[3]]$mean,0)
          m4<-round(reconciled[[4]]$mean,0)
          m5<-round(reconciled[[5]]$mean,0)
          m6<-round(reconciled[[6]]$mean,0)
          m<-c(m1,m2,m3,m4,m5,m6)
          l1<-round(reconciled[[1]]$lower[,1],2)
          l2<-round(reconciled[[2]]$lower[,1],2)
          l3<-round(reconciled[[3]]$lower[,1],2)
          l4<-round(reconciled[[4]]$lower[,1],2)
          l5<-round(reconciled[[5]]$lower[,1],2)
          l6<-round(reconciled[[6]]$lower[,1],2)
          l<-c(l1,l2,l3,l4,l5,l6)
          u1<-round(reconciled[[1]]$upper[,1],2)
          u2<-round(reconciled[[2]]$upper[,1],2)
          u3<-round(reconciled[[3]]$upper[,1],2)
          u4<-round(reconciled[[4]]$upper[,1],2)
          u5<-round(reconciled[[5]]$upper[,1],2)
          u6<-round(reconciled[[6]]$upper[,1],2)
          u<-c(u1,u2,u3,u4,u5,u6)
          mth1<-c(rep(reconciled[[1]]$method,length(m1)))
          mth2<-c(rep(reconciled[[2]]$method,length(m2)))
          mth3<-c(rep(reconciled[[3]]$method,length(m3)))
          mth4<-c(rep(reconciled[[4]]$method,length(m4)))
          mth5<-c(rep(reconciled[[5]]$method,length(m5)))
          mth6<-c(rep(reconciled[[6]]$method,length(m6)))
          mth<-c(mth1,mth2,mth3,mth4,mth5,mth6)
          thf<-data.frame(nameA,mth,m,l,u)
          colnames(thf)<-c("Time","Method","Point forecast","Lo 80","Hi 80")
          return(thf)
        }
        
        AAnaive<-TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::naive)
        AAsnaive<-TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::snaive)
        AAavg<-TemporalHierachicalForecastingNOmodel(Dengue_total_data,forecast::meanf)
        Fvalues<-data.frame(AAnaive,AAsnaive,AAavg,AAarima,AAets)
        Fvalues = data.table(Fvalues)
        
      }
      
    }
  })
  
  output$table <-
    render_gt(
      expr = Fvalues(),
      height = px(1000),
      width = px(1000)
    )
  
  
  output$downloadData <- downloadHandler(
    filename = function(){"Fvalues.csv"}, 
    content = function(fname){
      write.csv(Fvalues(), fname)
    }
  )
}

shinyApp(ui = ui, server = server)

