# server.R
library(quantmod)
library(dygraphs)
library(magrittr)
library(pracma)
#library(qmao)
source("tradingstrategy.R")
require(xts)
require(jsonlite)
library(TTR)
library(zoo)
source("tradedynamic.R")

getQuote_ <- function(ticks) {
qRoot <- "https://query1.finance.yahoo.com/v7/finance/quote?fields=symbol,longName,regularMarketTime,regularMarketPreviousClose,regularMarketChange,regularMarketOpen,regularMarketDayHigh,regularMarketDayLow,regularMarketVolume&formatted=false&symbols="
z <- fromJSON(paste(qRoot, paste(ticks, collapse=","), sep=""))
z <- z$quoteResponse$result[,c("symbol", "regularMarketTime", "regularMarketPreviousClose", "regularMarketChange", "regularMarketOpen", "regularMarketDayHigh", "regularMarketDayLow", "regularMarketVolume")]
row.names(z) <- z$symbol
z$symbol <- NULL
names(z) <- c("Time", "Last", "Change", "Open", "High", "Low", "Volume")
z$Time <- as.POSIXct(z$Time, origin = '1970-01-01 00:00:00')
return(z)
}

shinyServer(function(input, output) { 
  tini <- reactiveValues(data = 90)
  GameD <- reactiveValues(da = NULL,da2 = NULL)
  analsign <- reactiveValues(d = NULL)
  winsz <- reactiveValues(data = 20,fastsz = 26,slowsz = 12, bbnd = 14, nsig = 9)
  mode <- reactiveValues(data = NULL)
  observeEvent(input$normal,{
  mode$data <- NULL
  })
  observe({if(is.null(GameD$da)){ 
  GameD$da <- input$dateG[1]}
  })
  observeEvent(input$dateG[1],{
  GameD$da <- input$dateG[1]
  })
  #observe({if(is.null(GameD$da2)){
  #GameD$da2 <- input$dateG[2]}
  #})
  #observeEvent(input$dateG[2],{
  #GameD$da2 <- input$dateG[2]
  #})
   observe({if(input$checkG== TRUE){
   analsign$d = 1}else{analsign$d = 2}
  })
  observeEvent(input$game,{
  mode$data <- 1
  })
# Symbol - Current
  dataInput <- reactive({if(is.null(mode$data)){  
      getSymbols(input$symb,src = "yahoo", 
	  from =  as.Date(Sys.Date()) - tini$data-90,
          to = as.Date(Sys.Date()),
          auto.assign = FALSE)}else{
      getSymbols(input$symb,src = "yahoo", 
	  from =  input$dateG[1]-90,
          to = input$dateG[2],
          auto.assign = FALSE)}
  })
  dataInputc <- reactive({getSymbols(input$symb,src = "yahoo", 
	  from =  as.Date(Sys.Date()) - tini$data,
          to = as.Date(Sys.Date()),
          auto.assign = FALSE)
  })
  dataInput6 <- reactive({
      getSymbols(input$symb,src = "yahoo", 
	  from =  GameD$da-7,
          to = GameD$da,
          auto.assign = FALSE)
  })
# Symbol - Historical
  dataInput5 <- reactive({ 
      getSymbols(input$symb,src = "yahoo", 
	  from =  input$dates[1],
          to = input$dates[2]+1,
          auto.assign = FALSE)
  })
# Selector
  wselectbox <- reactive({
    switch(input$trade,
           "Simple" = 1,
           "Trading Indicators" = 2,
           "Rscript" = 3)
  })
# Selector
  selboxindG <- reactive({
    switch(input$indicatorG,
           "RSI" = 1,
           "CCI" = 2,
           "Bbands" = 3,"SMA"=4,"SMI"=5,"SAR"=6)
  })
 gameQ <- reactiveValues(data = NULL,data2=NULL)
 observeEvent(input$playG,{
  gameQ$data <- 1
  gameQ$data2 <- NULL
  })
 observeEvent(input$analyzeG,{
  optimalret()
  gameQ$data2 <- 1
  gameQ$data <- NULL
  })
  observe({if(!is.null(mode$data) & !is.null(gameQ$data)){showElement("tgq1");showElement("tgq2");showElement("tgq3");showElement("txgame1");showElement("txgame2");showElement("txgame3");}else{hideElement("tgq1");hideElement("tgq2");hideElement("tgq3");hideElement("txgame1");hideElement("txgame2");hideElement("txgame3");}})
  observe({if(!is.null(mode$data) & !is.null(gameQ$data2)){showElement("taq1");showElement("taq2");showElement("taq3");showElement("txanal1");showElement("txanal2");showElement("txanal3");showElement("ReportA");}else{hideElement("taq1");hideElement("taq2");hideElement("taq3");hideElement("txanal1");hideElement("txanal2");hideElement("txanal3");hideElement("ReportA");}})
  observe({if(is.null(mode$data)) {showElement("gt1");showElement("gt2");showElement("gt3");showElement("gt4");showElement("gt5"); 
           showElement("dates");showElement("trade");hideElement("dateG");hideElement("analyzeG");hideElement("playG");hideElement("indicatorG");hideElement("strategyG");showElement("tt1");showElement("tt2");showElement("tt3");showElement("tt4");showElement("table2");hideElement("nextb");hideElement("checkGroup");hideElement("checkG");}else{showElement("analyzeG");showElement("playG");showElement("dateG");showElement("indicatorG");showElement("strategyG");hideElement("dates");hideElement("trade");
hideElement("gt1");hideElement("gt2");hideElement("gt3");hideElement("gt4");hideElement("gt5");hide("tt1");hide("tt2");hide("tt3");hide("tt4");hideElement("table2");showElement("nextb");showElement("checkGroup");showElement("checkG");}
  })
# CODE RELATED TO SIMPLE
  observe({if((wselectbox() == 1 & is.null(mode$data)) | (wselectbox() == 3 & w$data != 1 & is.null(mode$data))) { 
           showElement("text15");showElement("text16");showElement("text17");showElement("text18");showElement("text19");showElement("text20");showElement("text21");showElement("text21j");showElement("textn");showElement("textn2");showElement("textn3");showElement("textn33");
           }else{
           hideElement("text15");hideElement("text16");hideElement("text17");hideElement("text18");hideElement("text19");hideElement("text20");hideElement("text21");hideElement("text21j");hideElement("textn");hideElement("textn2");hideElement("textn3");hideElement("textn33");}
   })
  resulttrade20 <- reactive({
    simpletrade(dataInputc()[,1],dataInputc()[,4],dataInput2(),v$data,input$symb,2)
  })
  resulttrade2 <- reactive({
	 simpletrade(dataInput5()[,1],dataInput5()[,4],dataInput2(),v$data,input$symb,1)
   })
#  resultsell2 <- reactive({
#	 simpletrade2(dataInput()[,1],dataInput()[,4])
 # })
#  output$text15 <- renderText({
#	paste("Close Price < Open Price ===> Buy")
#  })
#  output$text16 <- renderText({
#        paste("Close Price > Open Price ===> Sell")
#  })
#  output$textn4 <- renderText({
#	paste("-----------------------------")
#  })
#  output$textn5 <- renderText({
#	paste("Decision is ",resulttrade2()[4],"if the condition satisfied")
#  })
#  output$textn6 <- renderText({
#	paste(resulttrade2()[12])
#  })
# output$table10 <- renderTable({
#        resulttrade2()[13]
#  })
  output$textn2 <- renderText({
	paste("__________________________________")
  })
#  output$textn <- renderText({
#	paste("Back testing summary:")
#  })
  output$textn3 <- renderText({
	paste("Date:    ",input$dates[1],"   to    ",input$dates[2])
  })
  output$textn33 <- renderText({
	paste("Stock:  ",input$symb)
  })
  output$text17 <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste("# of buy = ",resulttrade2()[[4]])}else{paste("# of buy = ",length(bu1()[,1]))}
  })
  output$text18 <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste(" # of sell = ",resulttrade2()[[5]])}else{paste(" # of sell = ",length(se1()[,1]))}
  })
  output$text19 <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste("total Buy =", round(resulttrade2()[[6]],2))}else{paste("total Buy =", round(sum(bu1()[,2]),2))}
  })
  output$text20 <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste("total Sell = ",round(resulttrade2()[[7]],2))}else{paste("total Sell =", round(sum(se1()[,2]),2))}
  })
  output$text21 <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste("How many stocks are unrealized? ",resulttrade2()[[4]]-resulttrade2()[[5]])}else{paste("How many stocks are unrealized? ",length(bu1()[,1])-length(se1()[,1]))}
  })
  output$text21j <- renderText({if(w$data == 1 | wselectbox() == 1){
	paste("What is the earning? ", round(dataInput5()[length(dataInput5()[,4]),4] * (resulttrade2()[[4]]-resulttrade2()[[5]])+resulttrade2()[[7]]-resulttrade2()[[6]],2))}else{paste("What is the earning? ", round(dataInput5()[length(dataInput5()[,4]),4] * (length(bu1()[,1])-length(se1()[,1]))+sum(se1()[,2])-sum(bu1()[,2]),2))}
  })
# CODE RELATED TO INDICATOR
  observe({if(wselectbox() == 2 & is.null(mode$data)) { 
           showElement("RSI");showElement("RSI1");showElement("RSI0");showElement("RSI10");
           showElement("CCI");showElement("CCI1");showElement("CCI0");showElement("CCI10");
           showElement("BB");showElement("BB1");showElement("BB0");showElement("BB10");
           showElement("SMA");showElement("SMA1");showElement("SMA0");showElement("SMA10");
           showElement("SMI");showElement("SMI1");showElement("SMI0");showElement("SMI10");
           showElement("SAR");showElement("SAR1");showElement("SAR0");showElement("SAR10");
           showElement("rsiv1");showElement("rsiv10");showElement("rsiv11");showElement("rsiv110");
	   showElement("rsiv2");showElement("rsiv20");showElement("rsiv12");showElement("rsiv120");
 	   showElement("rsiv3");showElement("rsiv30");showElement("rsiv13");showElement("rsiv130");
           showElement("rsivn1");showElement("rsivn10");showElement("rsivn11");showElement("rsivn110");
	   showElement("rsivn2");showElement("rsivn20");showElement("rsivn12");showElement("rsivn120");
 	   showElement("rsivn3");showElement("rsivn30");showElement("rsivn13");showElement("rsivn130");
	   showElement("LAGn1");showElement("LAGn2");showElement("LAGn3");showElement("LAGn4");showElement("LAGn5");showElement("LAGn6");showElement("LAGn7");showElement("LAGn8");showElement("LAGn9");showElement("LAGn10");showElement("LAGn11");showElement("LAGn12");
           showElement("And3");showElement("And4");showElement("And5");showElement("And13");showElement("And14");showElement("And15");
           showElement("And1");showElement("And11");
	   showElement("LAG1");showElement("LAG2");showElement("LAG3");showElement("LAG4");showElement("LAG5");showElement("LAG6");showElement("LAG7");showElement("LAG8");showElement("LAG9");showElement("LAG10");showElement("LAG11");showElement("LAG12");
           showElement("whenb");showElement("whens");
           showElement("And2");showElement("And12");
           showElement("Buy")
           showElement("Sell")
           }else{
           hideElement("RSI");hideElement("RSI1");hideElement("RSI0");hideElement("RSI10");
           hideElement("CCI");hideElement("CCI1");hideElement("CCI0");hideElement("CCI10");
           hideElement("BB");hideElement("BB1");hideElement("BB0");hideElement("BB10");
           hideElement("SMA");hideElement("SMA1");hideElement("SMA0");hideElement("SMA10");
           hideElement("SMI");hideElement("SMI1");hideElement("SMI0");hideElement("SMI10");
           hideElement("SAR");hideElement("SAR1");hideElement("SAR0");hideElement("SAR10");
           hideElement("rsiv1");hideElement("rsiv10");hideElement("rsiv11");hideElement("rsiv110");
	   hideElement("rsiv2");hideElement("rsiv20");hideElement("rsiv12");hideElement("rsiv120");
 	   hideElement("rsiv3");hideElement("rsiv30");hideElement("rsiv13");hideElement("rsiv130");
           hideElement("rsivn1");hideElement("rsivn10");hideElement("rsivn11");hideElement("rsivn110");
	   hideElement("rsivn2");hideElement("rsivn20");hideElement("rsivn12");hideElement("rsivn120");
 	   hideElement("rsivn3");hideElement("rsivn30");hideElement("rsivn13");hideElement("rsivn130");
	   hideElement("LAGn1");hideElement("LAGn2");hideElement("LAGn3");hideElement("LAGn4");hideElement("LAGn5");hideElement("LAGn6");hideElement("LAGn7");hideElement("LAGn8");hideElement("LAGn9");hideElement("LAGn10");hideElement("LAGn11");hideElement("LAGn12");
           hideElement("And3");hideElement("And4");hideElement("And5");hideElement("And13");hideElement("And14");hideElement("And15");
           hideElement("And1");hideElement("And11");
hideElement("LAG1");hideElement("LAG2");hideElement("LAG3");hideElement("LAG4");hideElement("LAG5");hideElement("LAG6");hideElement("LAG7");hideElement("LAG8");hideElement("LAG9");hideElement("LAG10");hideElement("LAG11");hideElement("LAG12");hideElement("whenb");hideElement("whens");
           hideElement("And2");hideElement("And12");
           hideElement("Buy")
           hideElement("Sell")
           }
   })
  cs1 <- reactive({
    switch(input$RSI,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs2 <- reactive({
    switch(input$CCI,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs3 <- reactive({
    switch(input$BB,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn1 <- reactive({
    switch(input$SMA,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn2 <- reactive({
    switch(input$SMI,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn3 <- reactive({
    switch(input$SAR,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs4 <- reactive({
    switch(input$RSI0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs5 <- reactive({
    switch(input$CCI0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs6 <- reactive({
    switch(input$BB0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn4 <- reactive({
    switch(input$SMA0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn5 <- reactive({
    switch(input$SMI0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn6 <- reactive({
    switch(input$SAR0,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs7 <- reactive({
    switch(input$RSI1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs8 <- reactive({
    switch(input$CCI1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs9 <- reactive({
    switch(input$BB1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn7 <- reactive({
    switch(input$SMA1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn8 <- reactive({
    switch(input$SMI1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn9 <- reactive({
    switch(input$SAR1,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs10 <- reactive({
    switch(input$RSI10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs11 <- reactive({
    switch(input$CCI10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cs12 <- reactive({
    switch(input$BB10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn10 <- reactive({
    switch(input$SMA10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn11 <- reactive({
    switch(input$SMI10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  csn12 <- reactive({
    switch(input$SAR10,
           "<" = 1,"=" = 2,">" = 3,"<=" = 4,">=" = 5)
  })
  cond1 <- reactive({
    switch(input$And1,
           "AND" = 1,"OR" = 2)
  })
  cond2 <- reactive({
    switch(input$And2,
           "AND" = 1,"OR" = 2)
  })
  condn1 <- reactive({
    switch(input$And3,
           "AND" = 1,"OR" = 2)
  })
  condn2 <- reactive({
    switch(input$And4,
           "AND" = 1,"OR" = 2)
  })
  condn3 <- reactive({
    switch(input$And5,
           "AND" = 1,"OR" = 2)
  })
  cond3 <- reactive({
    switch(input$And11,
           "AND" = 1,"OR" = 2)
  })
  cond4 <- reactive({
    switch(input$And12,
           "AND" = 1,"OR" = 2)
  })
  condn4 <- reactive({
    switch(input$And13,
           "AND" = 1,"OR" = 2)
  })
  condn5 <- reactive({
    switch(input$And14,
           "AND" = 1,"OR" = 2)
  })
  condn6 <- reactive({
    switch(input$And15,
           "AND" = 1,"OR" = 2)
  })
 # observe({if(autoind()[1] == 1) {showElement("textp1")}else{hideElement("textp1")}
 #  }) 
  indicator <- reactiveValues(inp1 = NULL,inp2 = NULL,inp3 = NULL,inp4=NULL,inp5=NULL,inp6=NULL,inp7 = NULL,inp8 = NULL,inp9 = NULL,inp10=NULL,inp11=NULL,inp12=NULL)
  observeEvent(input$Buy,{
        indicator$inp1 <- input$rsiv1
        indicator$inp2 <- input$rsiv2
        indicator$inp3 <- input$rsiv3
        indicator$inp4 <- input$rsiv10
        indicator$inp5 <- input$rsiv20
        indicator$inp6 <- input$rsiv30
        indicator$inp7 <- input$rsivn1
        indicator$inp8 <- input$rsivn2
        indicator$inp9 <- input$rsivn3
        indicator$inp10 <- input$rsivn10
        indicator$inp11 <- input$rsivn20
        indicator$inp12 <- input$rsivn30
	conf$data2 <- NULL
  })
  indicator2 <- reactiveValues(inp1 = NULL,inp2 = NULL,inp3 = NULL,inp4=NULL,inp5=NULL,inp6=NULL,inp7 = NULL,inp8 = NULL,inp9 = NULL,inp10=NULL,inp11=NULL,inp12=NULL)
  observeEvent(input$Sell,{
        indicator2$inp1 <- input$rsiv11
        indicator2$inp2 <- input$rsiv12
        indicator2$inp3 <- input$rsiv13
        indicator2$inp4 <- input$rsiv110
        indicator2$inp5 <- input$rsiv120
        indicator2$inp6 <- input$rsiv130
        indicator2$inp7 <- input$rsivn11
        indicator2$inp8 <- input$rsivn12
        indicator2$inp9 <- input$rsivn13
        indicator2$inp10 <- input$rsivn110
        indicator2$inp11 <- input$rsivn120
        indicator2$inp12 <- input$rsivn130
	conf$data <- NULL
	conf$data2 <- 1
  })
  observe({if((is.null(indicator2$inp1) & is.null(indicator2$inp2) & is.null(indicator2$inp3) & is.null(indicator2$inp4) & is.null(indicator2$inp5) & is.null(indicator2$inp6) & is.null(indicator2$inp7) & is.null(indicator2$inp8) & is.null(indicator2$inp9) & is.null(indicator2$inp10) & is.null(indicator2$inp11) & is.null(indicator2$inp12) & !is.null(mode$data)) | wselectbox() != 2 | !is.null(mode$data)){
  hideElement("text9");hideElement("text9m");hideElement("text10");hideElement("text11");hideElement("text12");hideElement("text13");hideElement("text14");hideElement("text14p");hideElement("te1");hideElement("te2");hideElement("te4n");hideElement("te5n");hideElement("te3");hideElement("te4");hideElement("te5");hideElement("te6");hideElement("tek1");hideElement("tek2");hideElement("tem11");hideElement("tem12");hideElement("tem21");hideElement("tem22");hideElement("tem31");hideElement("tem32");}else{showElement("text9");showElement("text9m");showElement("text10");showElement("text11");showElement("text12");showElement("text13");showElement("text14");showElement("text14p");showElement("te1");showElement("te2");showElement("te3");showElement("te4");showElement("te5");showElement("te6");showElement("te4n");showElement("te5n");showElement("tek1");showElement("tek2");showElement("tem11");showElement("tem12");showElement("tem21");showElement("tem22");showElement("tem31");showElement("tem32");}
  })
  conf <- reactiveValues(data = NULL,data2 = NULL)
  observeEvent(input$Report,{
  conf$data <- 1
  })
  observe({if((is.null(indicator2$inp1) & is.null(indicator2$inp2) & is.null(indicator2$inp3) & is.null(indicator2$inp4) & is.null(indicator2$inp5) & is.null(indicator2$inp6) & is.null(indicator2$inp7) & is.null(indicator2$inp8) & is.null(indicator2$inp9) & is.null(indicator2$inp10) & is.null(indicator2$inp11) & is.null(indicator2$inp12) & !is.null(mode$data)) | wselectbox() != 2 | !is.null(mode$data)){hideElement("Report");
  }else{showElement("Report");}
  })
  resultbuy <- eventReactive(input$Buy,{
         if((is.null(indicator$inp1) & is.null(indicator$inp2) & is.null(indicator$inp3) & is.null(indicator$inp4) & is.null(indicator$inp5) & is.null(indicator$inp6) & is.null(indicator$inp7) & is.null(indicator$inp8) & is.null(indicator$inp9) & is.null(indicator$inp10) & is.null(indicator$inp11) & is.null(indicator$inp12))){
         0
	 }else{
	 tradeBuy(dataInput5()[,4],RSindex5(),CCindex5(),BBandsc5()[,2],sma5(),smi5()[,1],sar5(),indicator$inp1,indicator$inp2,indicator$inp3,indicator$inp4,indicator$inp5,indicator$inp6,indicator$inp7,indicator$inp8,indicator$inp9,indicator$inp10,indicator$inp11,indicator$inp12,cs1(),cs2(),cs3(),cs4(),cs5(),cs6(),csn1(),csn2(),csn3(),csn4(),csn5(),csn6(),cond1(),cond2(),condn1(),condn2(),condn3(),0,0)}
  })
  resultsell <- eventReactive(input$Sell,{
         if((is.null(indicator2$inp1) & is.null(indicator2$inp2) & is.null(indicator2$inp3) & is.null(indicator2$inp4) & is.null(indicator2$inp5) & is.null(indicator2$inp6) & is.null(indicator2$inp7) & is.null(indicator2$inp8) & is.null(indicator2$inp9) & is.null(indicator2$inp10) & is.null(indicator2$inp11) & is.null(indicator2$inp12))){
         0
	 }else{
	 tradeBuy(dataInput5()[,4],RSindex5(),CCindex5(),BBandsc5()[,2],sma5(),smi5()[,1],sar5(),indicator2$inp1,indicator2$inp2,indicator2$inp3,indicator2$inp4,indicator2$inp5,indicator2$inp6,indicator2$inp7,indicator2$inp8,indicator2$inp9,indicator2$inp10,indicator2$inp11,indicator2$inp12,cs7(),cs8(),cs9(),cs10(),cs11(),cs12(),csn7(),csn8(),csn9(),csn10(),csn11(),csn12(),cond3(),cond4(),condn4(),condn5(),condn6(),1,resultbuy()[,2])}
  })
  autoind <- eventReactive(input$Sell,{
         if(is.null(indicator2$inp1)){
         0
	 }else{
		autotradeindicator(resultsell()[,1],resultbuy()[,1],1,dataInput()[,4])}
  })
  output$whenb <- renderText({
	paste("When to Buy")
  })  
  output$whens <- renderText({
	paste("When to Sell")
  })
  output$te1 <- renderText({if(!is.null(conf$data2)){
        paste("Are you sure with this strategy:")}else{paste("")}
  })
  output$tek1 <- renderText({if(!is.null(conf$data2)){
        paste("Buy:")}else{paste("")}
  })
  output$tek2 <- renderText({if(!is.null(conf$data2)){
        paste("Sell:")}else{paste("")}
  })
  output$te2 <- renderText({if(!is.null(conf$data2)){
        paste("RSI:T-1",input$RSI,if(indicator$inp1==""){"not filled"}else{indicator$inp1},"-T",input$RSI0,if(indicator$inp4==""){"not filled"}else{indicator$inp4})}else{paste("")}
  })
  output$te3 <- renderText({if(!is.null(conf$data2)){
        paste("RSI:T-1",input$RSI1,if(indicator2$inp1==""){"not filled"}else{indicator2$inp1},"-T",input$RSI10,if(indicator2$inp4==""){"not filled"}else{indicator2$inp4})}else{paste("")}
  })
  output$te4 <- renderText({if(!is.null(conf$data2)){
        paste("CCI:T-1",input$CCI,if(indicator$inp2==""){"not filled"}else{indicator$inp2},"-T",input$CCI0,if(indicator$inp5==""){"not filled"}else{indicator$inp5})}else{paste("")}
  })
  output$te4n <- renderText({if(!is.null(conf$data2)){
        paste("BB:T-1",input$BB,if(indicator$inp3==""){"not filled"}else{indicator$inp3},"-T",input$BB0,if(indicator$inp6==""){"not filled"}else{indicator$inp6})}else{paste("")}
  })
  output$te5 <- renderText({if(!is.null(conf$data2)){
        paste("CCI:T-1",input$CCI1,if(indicator2$inp2==""){"not filled"}else{indicator2$inp2},"-T",input$CCI10,if(indicator2$inp5==""){"not filled"}else{indicator2$inp5})}else{paste("")}
  })
  output$te5n <- renderText({if(!is.null(conf$data2)){
        paste("BB:T-1",input$BB1,if(indicator2$inp3==""){"not filled"}else{indicator2$inp3},"-T",input$BB10,if(indicator2$inp6==""){"not filled"}else{indicator2$inp6})}else{paste("")}
  })
  output$tem11 <- renderText({if(!is.null(conf$data2)){
        paste("SMA:T-1",input$SMA,if(indicator$inp7==""){"not filled"}else{indicator$inp7},"-T",input$SMA0,if(indicator$inp10==""){"not filled"}else{indicator$inp10})}else{paste("")}
  })
  output$tem12 <- renderText({if(!is.null(conf$data2)){
        paste("SMA:T-1",input$SMA1,if(indicator2$inp7==""){"not filled"}else{indicator2$inp7},"-T",input$SMA10,if(indicator2$inp10==""){"not filled"}else{indicator2$inp10})}else{paste("")}
  })
  output$tem21 <- renderText({if(!is.null(conf$data2)){
        paste("SMI:T-1",input$SMI,if(indicator$inp8==""){"not filled"}else{indicator$inp8},"-T",input$SMI0,if(indicator$inp11==""){"not filled"}else{indicator$inp11})}else{paste("")}
  })
  output$tem22 <- renderText({if(!is.null(conf$data2)){
        paste("SMI:T-1",input$SMI1,if(indicator2$inp8==""){"not filled"}else{indicator2$inp8},"-T",input$SMI10,if(indicator2$inp11==""){"not filled"}else{indicator2$inp11})}else{paste("")}
  })
  output$tem31 <- renderText({if(!is.null(conf$data2)){
        paste("SAR:T-1",input$SAR,if(indicator$inp9==""){"not filled"}else{indicator$inp9},"-T",input$SAR0,if(indicator$inp12==""){"not filled"}else{indicator$inp12})}else{paste("")}
  })
  output$tem32 <- renderText({if(!is.null(conf$data2)){
        paste("SAR:T-1",input$SAR1,if(indicator2$inp9==""){"not filled"}else{indicator2$inp9},"-T",input$SAR10,if(indicator2$inp12==""){"not filled"}else{indicator2$inp12})}else{paste("")}
  })
  output$te6 <- renderText({if(!is.null(conf$data2)){
        paste("___________________________________")}else{paste("")}
  })
  output$text9 <- renderText({if(!is.null(conf$data)){
	paste("Date:    ",input$dates[1],"   to    ",input$dates[2])}
  })
  output$text9m <- renderText({if(!is.null(conf$data)){
	paste("Stock:  ",input$symb)}else{paste("")}
  })
  output$text10 <- renderText({if(!is.null(conf$data)){
	paste("# Buy =", sum(resultbuy()[,2]))}
  })
  output$text11 <- renderText({if(!is.null(conf$data)){
	paste("# Sell =", sum(resultsell()[,2]))}
  })
  output$text12 <- renderText({if(!is.null(conf$data)){
	paste("total Buy =", round(sum(resultbuy()[resultbuy()[,2] == 1,4]),2))}
  })
  output$text13 <- renderText({if(!is.null(conf$data)){
	paste("total Sell =", round(sum(resultsell()[resultsell()[,2] == 1,4]),2))}
  })
  output$text14p <- renderText({if(!is.null(conf$data)){
	paste("How many stocks are unrealized? ", sum(resultbuy()[,2]) - sum(resultsell()[,2]))}
  })
  output$text14 <- renderText({if(!is.null(conf$data)){
	paste("what is earning? ", round(dataInput5()[length(dataInput5()[,4]),4] * (sum(resultbuy()[,2]) - sum(resultsell()[,2]))+sum(resultsell()[resultsell()[,2] == 1,4])-sum(resultbuy()[resultbuy()[,2] == 1,4]),2))}
  })
 # output$textp <- renderText({
#	paste("auto trade:", autoind()[2])
 # })
 # output$textp1 <- renderText({
#	paste("trade price:", autoind()[3])
#  })
  observeEvent(input$Report,{
	fprintf("\nReport of Back Testing the trading strategy:\n",file="indicatorreport.txt")
        fprintf("\nThis is the trading strategy:\n",file="indicatorreport.txt",append = TRUE)
        fprintf("\nBuy:",file="indicatorreport.txt",append = TRUE)
        fprintf("\nRSI:T-1 %s %s -T %s %s",input$RSI,if(indicator$inp1==""){"not filled"}else{indicator$inp1},input$RSI0,if(indicator$inp4==""){"not filled"}else{indicator$inp4},file="indicatorreport.txt",append = TRUE)
        fprintf("\nCCI:T-1 %s %s -T %s %s",input$CCI,if(indicator$inp2==""){"not filled"}else{indicator$inp2},input$CCI0,if(indicator$inp5==""){"not filled"}else{indicator$inp5},file="indicatorreport.txt",append = TRUE)
        fprintf("\n BB:T-1 %s %s -T %s %s",input$BB,if(indicator$inp3==""){"not filled"}else{indicator$inp3},input$BB0,if(indicator$inp6==""){"not filled"}else{indicator$inp6},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSMA:T-1 %s %s -T %s %s",input$SMA,if(indicator$inp7==""){"not filled"}else{indicator$inp7},input$SMA0,if(indicator$inp10==""){"not filled"}else{indicator$inp10},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSMI:T-1 %s %s -T %s %s",input$SMI,if(indicator$inp8==""){"not filled"}else{indicator$inp8},input$SMI0,if(indicator$inp11==""){"not filled"}else{indicator$inp11},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSAR:T-1 %s %s -T %s %s",input$SAR,if(indicator$inp9==""){"not filled"}else{indicator$inp9},input$SAR0,if(indicator$inp12==""){"not filled"}else{indicator$inp12},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSell:",file="indicatorreport.txt",append = TRUE)
        fprintf("\nRSI:T-1 %s %s -T %s %s",input$RSI1,if(indicator2$inp1==""){"not filled"}else{indicator2$inp1},input$RSI10,if(indicator2$inp4==""){"not filled"}else{indicator2$inp4},file="indicatorreport.txt",append = TRUE)
        fprintf("\nCCI:T-1 %s %s -T %s %s",input$CCI1,if(indicator2$inp2==""){"not filled"}else{indicator2$inp2},input$CCI10,if(indicator2$inp5==""){"not filled"}else{indicator2$inp5},file="indicatorreport.txt",append = TRUE)
        fprintf("\n BB:T-1 %s %s -T %s %s",input$BB1,if(indicator2$inp3==""){"not filled"}else{indicator2$inp3},input$BB10,if(indicator2$inp6==""){"not filled"}else{indicator2$inp6},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSMA:T-1 %s %s -T %s %s",input$SMA1,if(indicator2$inp7==""){"not filled"}else{indicator2$inp7},input$SMA10,if(indicator2$inp10==""){"not filled"}else{indicator2$inp10},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSMI:T-1 %s %s -T %s %s",input$SMI1,if(indicator2$inp8==""){"not filled"}else{indicator2$inp8},input$SMI10,if(indicator2$inp11==""){"not filled"}else{indicator2$inp11},file="indicatorreport.txt",append = TRUE)
        fprintf("\nSAR:T-1 %s %s -T %s %s",input$SAR1,if(indicator2$inp9==""){"not filled"}else{indicator2$inp9},input$SAR10,if(indicator2$inp12==""){"not filled"}else{indicator2$inp12},file="indicatorreport.txt",append = TRUE)
	fprintf("\n*************************************************************************************",file="indicatorreport.txt",append = TRUE)
	fprintf("\nBuy trades:\n",file="indicatorreport.txt",append = TRUE)
        fprintf("\n ____________________________________________________________________________________",file="indicatorreport.txt",append = TRUE)
	fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20s |","Date","Stock Name","Buy","How many shares","Transaction Price",file="indicatorreport.txt",append = TRUE)
	fprintf("\n ====================================================================================",file="indicatorreport.txt",append = TRUE)
	fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20.2f |",resultbuy()[resultbuy()[,2] == 1,3],input$symb,resultbuy()[resultbuy()[,2] == 1,1],"1",resultbuy()[resultbuy()[,2] == 1,4],file="indicatorreport.txt",append = TRUE)
	fprintf("\n*************************************************************************************\n",file="indicatorreport.txt",append = TRUE)
	fprintf("\nSell trades:\n",file="indicatorreport.txt",append = TRUE)
        fprintf("\n ____________________________________________________________________________________",file="indicatorreport.txt",append = TRUE)
	fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20s |","Date","Stock Name","Buy","How many shares","Transaction Price",file="indicatorreport.txt",append = TRUE)
	fprintf("\n ====================================================================================",file="indicatorreport.txt",append = TRUE)
	fprintf("\n| %10s |   %10s |  %10s |  %15s |  %20.2f |",resultsell()[resultsell()[,2] == 1,3],input$symb,resultsell()[resultsell()[,2] == 1,1],"1",resultsell()[resultsell()[,2] == 1,4],file="indicatorreport.txt",append = TRUE)
	fprintf("\n*************************************************************************************\n",file="indicatorreport.txt",append = TRUE)
        fprintf("\ntotal Buy = %6.2f",sum(resultbuy()[resultbuy()[,2] == 1,4]),file="indicatorreport.txt",append = TRUE)
        fprintf("\ntotal Sell = %6.2f",sum(resultsell()[resultsell()[,2] == 1,4]),file="indicatorreport.txt",append = TRUE)
        fprintf("\nrealized Earning = %6.2f",round(dataInput5()[length(dataInput5()[,4]),4] * (sum(resultbuy()[,2]) - sum(resultsell()[,2]))+sum(resultsell()[resultsell()[,2] == 1,4])-sum(resultbuy()[resultbuy()[,2] == 1,4]),2),file="indicatorreport.txt",append = TRUE)
  })
# CODE RELATED TO Rscript
  observe({if(wselectbox() == 3 & is.null(mode$data)) { showElement("file1") }else{hideElement("file1")}})

# output$table10 <- renderTable({
#      resulttrade2()[[9]]
#  })
# Code Related to Rscript
  w <- reactiveValues(data = 1)
  observeEvent(input$file1,{
        source(input$file1$datapath,chdir=TRUE)
        w$data = 2 
  })
bu1 <- eventReactive(input$file1,{
         if(w$data == 1){
         0
	 }else{
         calc(dataInput5(),1)}
  })
se1 <- eventReactive(input$file1,{
         if(w$data == 1){
         0
	 }else{
         calc(dataInput5(),2)}
  })
bu10 <- eventReactive(input$file1,{
         if(w$data == 1){
         0
	 }else{
         calc(dataInputc(),1)}
  })
se10 <- eventReactive(input$file1,{
         if(w$data == 1){
         0
	 }else{
         calc(dataInputc(),2)}
  })
to1 <- eventReactive(input$file1,{
         if(w$data == 1){
         0
	 }else{
         calc(dataInput5(),3)}
  })
# Code Related to first graph
  v <- reactiveValues(data = 1)
  observeEvent(input$gt1, {
           v$data <- 2
           tini$data <- 7
           winsz$data <- 2
           winsz$slowsz <- 3
           winsz$fastsz <- 1
           winsz$bbnd <- 2
           winsz$nsig <- 2
  })
  observeEvent(input$gt2, {
           v$data <- 1
           tini$data <- 7
           winsz$data <- 2
           winsz$slowsz <- 3
           winsz$fastsz <- 1
           winsz$bbnd <- 2
           winsz$nsig <- 2
  })
  observeEvent(input$gt3, {
           v$data <- 1
           tini$data <- 30
           winsz$data <- 5
           winsz$slowsz <- 10
           winsz$fastsz <- 5
           winsz$bbnd <- 5
           winsz$nsig <- 7    
  })
  observeEvent(input$game, {
           v$data <- 1
           tini$data <- 30
	   winsz$data <- 5
           winsz$slowsz <- 10
           winsz$fastsz <- 5
           winsz$bbnd <- 5
           winsz$nsig <- 7  
  })
  observeEvent(input$gt4, {
           v$data <- 1
           tini$data <- 90
           winsz$slowsz <- 26
           winsz$fastsz <- 12
           winsz$bbnd <- 14
           winsz$nsig <- 9
  })
  observeEvent(input$gt5, {
           v$data <- 1
           tini$data <- 365
           winsz$slowsz <- 26
           winsz$fastsz <- 12
           winsz$bbnd <- 14
           winsz$nsig <- 9
  })
 # shinyBATS()
  autoInvalidate <- reactiveTimer(60000)
  dataInput2 <- reactive({
	autoInvalidate()
        getQuote_(input$symb)   
  })
  CR <- reactive({
        autoInvalidate()
        tradeQuote(dataInput2()[1:3])
  })
  L24hour <- reactive({
        autoInvalidate()
        tradeTime(dataInput2()[1:3],input$symb)	
  })
  L24HOUR <- reactive({
        autoInvalidate()
        xts(L24hour()[,2], order.by=as.POSIXct(L24hour()[,1],origin = "1960-01-01"))
  })
  buydt <- reactive({if(w$data == 1){resulttrade20()[[1]]}else{bu10()}
  })
  selldt <- reactive({if(w$data == 1){resulttrade20()[[2]]}else{se10()}
  })
  t1 <- reactive({min(time(dataInputc()))
  })
  t2 <- reactive({max(time(dataInputc()))
  })
  p1 <- reactive({round(min(dataInputc()[,1]),0)
  })
  p2 <- reactive({round(max(dataInputc()[,1]),0)
  })
#  earning <- reactive({if(w$data==1){calculation(dataInput,3)}else{calc(dataInput,3)}
#  }) 
  buyd <- reactive({if(w$data == 1){xts(buydt()[,3], order.by=as.POSIXct(as.Date(buydt()[,1])))}else{xts(buydt()[,2], order.by=as.POSIXct(as.Date(buydt()[,1])))}})
  selld <- reactive({if(w$data == 1){xts(selldt()[,3], order.by=as.POSIXct(as.Date(selldt()[,1])))}else{xts(selldt()[,2], order.by=as.POSIXct(as.Date(selldt()[,1])))}})
  PC <- reactive({sort(rbind(buyd(),selld()))})
  RSindex0 <- reactive({RSI(dataInput()[,4],winsz$fastsz)})
  CCindex0 <- reactive({CCI(dataInput()[,2:4],n=winsz$data,c=0.015)})
  RSindex5 <- reactive({RSI(dataInput5()[,4],winsz$fastsz)})
  CCindex5 <- reactive({CCI(dataInput5()[,2:4],n=winsz$data,c=0.015)})
  BBandsc5 <- reactive({BBands(dataInput5()[,2:4], n = winsz$bbnd, sd = 2)})
  MACDc <- reactive({MACD(dataInput()[,4],nFast = winsz$fastsz, nSlow = winsz$slowsz, nSig = winsz$nsig, maType="SMA", percent = TRUE)})
  BBandsc0 <- reactive({BBands(dataInput()[,2:4], n = winsz$bbnd, sd = 2)})
  sma0 <- reactive({SMA(dataInput()[,4],round(winsz$fastsz+winsz$slowsz/2))})
  sma5 <- reactive({SMA(dataInput5()[,4],round(winsz$fastsz+winsz$slowsz/2))})
  sar0 <- reactive({SAR(dataInput()[,2:3])})
  sar5 <- reactive({SAR(dataInput5()[,2:3])})
  smi0 <- reactive({SMI(dataInput()[,2:4],nFast = winsz$fastsz, nSlow = winsz$slowsz, nSig = winsz$nsig, maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), bounded = TRUE)})
  smi5 <- reactive({SMI(dataInput5()[,2:4],nFast = winsz$fastsz, nSlow = winsz$slowsz, nSig = winsz$nsig, maType=list(list(SMA), list(EMA, wilder=TRUE), list(SMA)), bounded = TRUE)})
  RSindex <- reactive({if(is.null(mode$data)){
		same_time1(RSindex0(),dataInputc()[,4])}else{
		RSindex0()}
	})
  CCindex <- reactive({if(is.null(mode$data)){
		same_time1(CCindex0(),dataInputc()[,4])}else{
		CCindex0()}
	})	
  BBandsc <- reactive({if(is.null(mode$data)){
		same_time(BBandsc0()[,2],dataInputc()[,4],BBandsc0())}else{
		BBandsc0()}
	})   
  sma1 <- reactive({if(is.null(mode$data)){
		same_time1(sma0(),dataInputc()[,4])}else{
		sma0()}
	})
  smi1 <- reactive({if(is.null(mode$data)){
		same_time1(smi0()[,1],dataInputc()[,4])}else{
		smi0()}
	})
  sar1 <- reactive({if(is.null(mode$data)){
		same_time1(sar0(),dataInputc()[,4])}else{
		sar0()}
	})

  indimp_G <- reactive({if(selboxindG() == 1){RSindex()}else if(selboxindG() == 2){CCindex()}else if(selboxindG() == 3){BBandsc()[,2]}
else if(selboxindG() == 4){sma1()}else if(selboxindG() == 5){smi1()[,1]}else if(selboxindG() == 6){sar1()}})
  optimalret <- reactive({analysis_strategy(indimp_G(),dataInput()[,4],analsign$d)})
  op <- reactiveValues(a1 = NULL,a2 = NULL,a3 = NULL,a4=NULL,a5=NULL,a6=NULL,a7 = NULL,a8 = NULL,a9 = NULL,a10=NULL,a11=NULL,a12=NULL)
  observeEvent(input$ReportA,{
	op$a1 <- analysis_strategy(RSindex(),dataInput()[,4],1)
	op$a2 <- analysis_strategy(CCindex(),dataInput()[,4],1)
	op$a3 <- analysis_strategy(BBandsc()[,2],dataInput()[,4],1)
	op$a4 <- analysis_strategy(sma1(),dataInput()[,4],1)
	op$a5 <- analysis_strategy(smi1()[,1],dataInput()[,4],1)
	op$a6 <- analysis_strategy(sar1(),dataInput()[,4],1)
	op$a7 <- analysis_strategy(RSindex(),dataInput()[,4],2)
	op$a8 <- analysis_strategy(CCindex(),dataInput()[,4],2)
	op$a9 <- analysis_strategy(BBandsc()[,2],dataInput()[,4],2)
	op$a10 <- analysis_strategy(sma1(),dataInput()[,4],2)
	op$a11 <- analysis_strategy(smi1()[,1],dataInput()[,4],2)
	op$a12 <- analysis_strategy(sar1(),dataInput()[,4],2)
        write_report(input$symb,input$dateG[1],input$dateG[2],op$a1,op$a2,op$a3,op$a4,op$a5,op$a6,op$a7,op$a8,op$a9,op$a10,op$a11,op$a12,dataInput()[,4],RSindex(),CCindex(),BBandsc()[,2],sma1(),smi1()[,1],sar1())

  })
  observeEvent(input$analyzeG,{
       individual_report(input$symb,input$dateG[1],input$dateG[2],dataInput()[,4],optimalret(),analsign$d,selboxindG())
  })
  #weeklyprice <- reactive({weeklyprices(dataInput)})
  oparray <- reactiveValues(data = NULL,data2 = NULL,data3 = NULL,data4 = NULL)
  output$txgame1 <- renderText({if(!is.null(oparray$data)){
	paste(time(oparray$data[1]))}else{paste("")}
  })
  output$txgame2 <- renderText({if(!is.null(oparray$data) & length(oparray$data) != 1){
	paste(time(oparray$data[2]))}else{paste("")}
  })
  output$txgame3 <- renderText({if(!is.null(oparray$data)){
	paste(round(sum(oparray$data2) - sum(oparray$data),2))}else{paste("")}
  })
  output$txanal1 <- renderText({
	paste(optimalret()[[3]])
  })
  output$txanal2 <- renderText({
	paste(optimalret()[[2]])
  })
  output$txanal3 <- renderText({
	paste(round(optimalret()[[1]],2))
  })
  observeEvent(input$dateG[1], {
		oparray$data <- NULL;oparray$data2 <- NULL;oparray$data3 <- NULL;oparray$data4 <- NULL;
	})
  observeEvent(input$symb, {
		oparray$data <- NULL;oparray$data2 <- NULL;oparray$data3 <- NULL;oparray$data4 <- NULL;
	})
#  op <- reactive({operation(dataInput()[length(dataInput()[,4]),4],Input$checkGroup,operatearray$data,operatearray$data2)})
  observeEvent(input$nextb, {
                              if(input$checkGroup == 1){
                                   if(is.null(oparray$data)){
                                        oparray$data <- dataInput6()[length(dataInput6()[,4]),4]}else if(sum(time(oparray$data) == time(dataInput6()[length(dataInput6()[,4]),4])) == 0){
                                        oparray$data <- c(oparray$data,dataInput6()[length(dataInput6()[,4]),4])}
                              }else if(input$checkGroup == 2){
				if(!is.null(oparray$data)){
			      	   if(is.null(oparray$data2)){
                                        oparray$data2 <- dataInput6()[length(dataInput6()[,4]),4]}else if(sum(time(oparray$data2) == time(dataInput6()[length(dataInput6()[,4]),4])) == 0 & length(oparray$data) > length(oparray$data2)){
                                        oparray$data2 <- c(oparray$data2,dataInput6()[length(dataInput6()[,4]),4])}
			      }}
			      if(GameD$da < as.Date(Sys.Date())){
					GameD$da <- GameD$da + 1 	
			      }
			      if(!is.null(oparray$data)){
			      oparray$data3 <- same_size(oparray$data,dataInput6()[,4])
			      }
			      if(!is.null(oparray$data2)){
			      oparray$data4 <- same_size(oparray$data2,dataInput6()[,4])
		              }
			      print(paste("price",dataview()))
		              print(paste("buy",oparray$data3))
			      print(paste("sell",oparray$data4))
			      print(paste("last",dataInput6()[length(dataInput6()[,4]),4]))
          })
   dataview <- reactive({
		last_point(dataInput6()[,4])
	})   
   dataviewBB <- reactive({
		same_time(BBandsc()[,2],dataInput6()[,4],BBandsc())
	})   
   rsiforv <- reactive({
		same_time1(RSindex(),dataInput6()[,4])
	})	
   dataviewrsi <- reactive({
		last_point1(rsiforv(),dataInput6()[,4])
	}) 
   volforv <- reactive({
		same_time1(dataInput()[,5],dataInput6()[,4])
	})	
   dataviewvol <- reactive({
		last_point1(volforv(),dataInput6()[,4])
	}) 
   cciforv <- reactive({
		same_time1(CCindex(),dataInput6()[,4])
	})	
   dataviewcci <- reactive({
		last_point1(cciforv(),dataInput6()[,4])
	}) 
   smaforv <- reactive({
		same_time1(sma1(),dataInput6()[,4])
	})	
   dataviewsma <- reactive({
		last_point1(smaforv(),dataInput6()[,4])
	}) 
   smiforv <- reactive({
		same_time1(smi1()[,1],dataInput6()[,4])
	})	
   dataviewsmi <- reactive({
		last_point1(smiforv(),dataInput6()[,4])
	}) 
   sarforv <- reactive({
		same_time1(sar1(),dataInput6()[,4])
	})	
   dataviewsar <- reactive({
		last_point1(sarforv(),dataInput6()[,4])
	})   
   output$dygraph <- renderDygraph({if(is.null(mode$data) & v$data==1 & !isempty(buyd()) & !isempty(selld())){
				 dygraph(cbind(PC(),buyd(),selld(),BBandsc()[,1:3]), main = "Stock Prices",ylab="Prices")%>%
 		                 dySeries("PC..",label="Price",color = "magenta",strokeWidth = 2) %>%
                                 dySeries("buyd..",label="Buy",drawPoints= TRUE,strokeWidth= 0.0,color = "green") %>%
                                 dySeries("selld..",label="Sell",drawPoints= TRUE,strokeWidth= 0.0,color = "red") %>%
 				#dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 #dySeries(c("macd", "signal"), label = "MACD",color="#2ea") %>%
				 #dySeries(c("AAPL.Close", "AAPL.Close.1", "AAPL.Close.2"), label = "MACD",color="orange") %>%
                                 #dyOptions(axisLineWidth = 1.5, strokeWidth= 0.0,drawPoints= TRUE, drawGrid = FALSE)
 				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 3)
			      #   dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
				 }else if(is.null(mode$data) & v$data==1 & isempty(selld()) & !isempty(buyd())){
                                 dygraph(cbind(PC(),buyd(),BBandsc()[,1:3]), main = "Stock Prices",ylab="Prices")%>%
 		                 dySeries("..1",label="Price",color = "magenta",strokeWidth = 2) %>%
                                 dySeries("..2",label="Buy",drawPoints= TRUE,strokeWidth= 0.0,color = "green") %>%
                                # dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 #dySeries(c("macd", "signal"), label = "MACD",color="#2ea") %>%
				 #dySeries(c("AAPL.Close", "AAPL.Close.1", "AAPL.Close.2"), label = "MACD",color="orange") %>%
                                 #dyOptions(axisLineWidth = 1.5, strokeWidth= 0.0,drawPoints= TRUE, drawGrid = FALSE)
 				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 3)
			      #   dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
                                 }else if(is.null(mode$data) & v$data==1){dygraph(cbind(PC(),BBandsc()[,1:3]), main = "Stock Prices",ylab="Prices")%>%
 		                 dySeries("..1",label="Price",color = "magenta",strokeWidth = 2) %>%
 				# dyHighlight(highlightSeriesOpts = list(strokeWidth = 3))%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 #dySeries(c("macd", "signal"), label = "MACD",color="#2ea") %>%
				 #dySeries(c("AAPL.Close", "AAPL.Close.1", "AAPL.Close.2"), label = "MACD",color="orange") %>%
                                 #dyOptions(axisLineWidth = 1.5, strokeWidth= 0.0,drawPoints= TRUE, drawGrid = FALSE)
 				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 3)
			      #   dyOptions(colors = RColorBrewer::brewer.pal(3, "Set1"))
				 }else if(is.null(mode$data) & v$data==2){
 				 dygraph(L24HOUR(),ylab="Price")%>%
		                 dySeries("V1",label="Price",color = "darkgreen")
                                  }else if(!is.null(mode$data) & !is.null(oparray$data3) & !is.null(oparray$data4)){dygraph(cbind(dataview(),oparray$data3,oparray$data4,dataInput6()[length(dataInput6()[,4]),4],dataviewBB()[,1:3]), main ="Game Mode",ylab="Price")%>%
			         #dyAxis("x", valueRange = c(time(dataInput()[1,4]), NULL))%>%
  		                 dySeries(sprintf("%s.Close",input$symb),label="Price",color = "blue",strokeWidth = 2) %>%
                                 dySeries("..2",label="Buy",drawPoints= TRUE,strokeWidth= 0.0,color = "green") %>%
                                 dySeries("..3",label="Sell",drawPoints= TRUE,strokeWidth= 0.0,color = "red")%>%
                                 dySeries(sprintf("%s.Close.1",input$symb),label="current",drawPoints= TRUE,strokeWidth= 0.0,color = "#ffff66")%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)
 				  }else if(!is.null(mode$data) & !is.null(oparray$data3)){dygraph(cbind(dataview(),oparray$data3,dataInput6()[length(dataInput6()[,4]),4],dataviewBB()[,1:3]), main ="Game Mode",ylab="Price")%>%
  		                 dySeries(sprintf("%s.Close",input$symb),label="Price",color = "blue",strokeWidth = 2)%>%
  		                 dySeries("..2",label="Buy",drawPoints= TRUE,strokeWidth= 0.0,color = "green")%>%
  		                 dySeries(sprintf("%s.Close.1",input$symb),label="current",drawPoints= TRUE,strokeWidth= 0.0,color = "#ffff66")%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)
				  }else if(!is.null(mode$data)  & !is.null(oparray$data4)){dygraph(cbind(dataview(),oparray$data4,dataInput6()[length(dataInput6()[,4]),4],dataviewBB()[,1:3]), main ="Game Mode",ylab="Price")%>%
  		                 dySeries(sprintf("%s.Close",input$symb),label="Price",color = "blue",strokeWidth = 2)%>%
                                 dySeries("..2",label="Sell",drawPoints= TRUE,strokeWidth= 0.0,color = "red")%>%
                                 dySeries(sprintf("%s.Close.1",input$symb),label="current",drawPoints= TRUE,strokeWidth= 0.0,color = "#ffff66")%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)
				  }else if(!is.null(mode$data)){dygraph(cbind(dataview(),dataInput6()[length(dataInput6()[,4]),4],dataviewBB()[,1:3]), main ="Game Mode",ylab="Price")%>%
  		                 dySeries(sprintf("%s.Close",input$symb),label="Price",color = "blue",strokeWidth = 2)%>%
                                 dySeries(sprintf("%s.Close.1",input$symb),label="current",drawPoints= TRUE,strokeWidth= 0.0,color = "#ffff66")%>%
				 dySeries(c("dn", "mavg", "up"), label = "BBand",color="#2ea") %>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
				 })
 #}else{
 	# output$dygraph <- renderDygraph({dygraph(L24hour(),ylab="RSI")%>%
	#	                 dySeries("EMA",label="RSI",color = "darkgreen")
      	#		 })
 output$dygraph2 <- renderDygraph({if(is.null(mode$data)){dygraph(RSindex(),ylab="RSI")%>%
		                 dySeries("V1",label="RSI",color = "darkgreen")}else{
				 dygraph(dataviewrsi(),ylab="RSI")%>%
		                 dySeries("V1",label="RSI",color = "darkgreen",strokeWidth = 2)%>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
      			 })
 output$dygraph3 <- renderDygraph({if(is.null(mode$data)){dygraph(dataInputc()[,5],ylab="Volume")%>%
		                 dySeries(label="Volume",color = "green") %>%
				 dyOptions(stepPlot = TRUE,axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)}else{
				 dygraph(dataviewvol(),ylab="Volume")%>%
		                 dySeries(label="Volume",color = "green") %>%
				 dyOptions(stepPlot = TRUE,axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE)
				}
      			 })
 output$dygraph4 <- renderDygraph({if(is.null(mode$data)){dygraph(CCindex(),ylab="CCI")%>%
		                 dySeries("V1",label="CCI",color = "darkgreen")}else{
				 dygraph(dataviewcci(),ylab="CCI")%>%
		                 dySeries("V1",label="CCI",color = "darkgreen",strokeWidth = 2)%>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
      			 })
 output$dygraph5 <- renderDygraph({if(is.null(mode$data)){dygraph(sma1(),ylab="SMA")%>%
		                 dySeries("V1",label="SMA",color = "darkgreen")}else{
				 dygraph(dataviewsma(),ylab="SMA")%>%
		                 dySeries("V1",label="SMA",color = "darkgreen",strokeWidth = 2)%>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
      			 })
 output$dygraph6 <- renderDygraph({if(is.null(mode$data)){dygraph(smi1()[,1],ylab="SMI")%>%
		                 dySeries("V1",label="SMI",color = "darkgreen")}else{
				 dygraph(dataviewsmi(),ylab="SMI")%>%
		                 dySeries("V1",label="SMI",color = "darkgreen",strokeWidth = 2)%>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
      			 })
 output$dygraph7 <- renderDygraph({if(is.null(mode$data)){dygraph(sar1(),ylab="SAR")%>%
		                 dySeries("V1",label="SAR",color = "darkgreen")}else{
				 dygraph(dataviewsar(),ylab="SAR")%>%
		                 dySeries("V1",label="SAR",color = "darkgreen",strokeWidth = 2)%>%
				 dyOptions(connectSeparatedPoints=TRUE,axisLineWidth = 1.5, drawPoints= FALSE, drawGrid = TRUE,pointSize = 5)}
      			 })
#output$dygraph5 <- renderDygraph({dygraph(dataInput2(),ylab="CCI")%>%
#		                 dySeries("cci",label="CCI",color = "darkgreen")
#      			 })
 #output$plott <- renderPlot({
 #                                autoInvalidate()
#				 plot(c(as.POSIXct(dataInput2()[,1]-60),as.POSIXct(dataInput2()[,1])),CR(),xaxt = "n", type = "l", main="current price",
#  xlab="time", ylab="price")
#				 axis(1,c(as.POSIXct(dataInput2()[,1]-60),as.POSIXct(dataInput2()[,1])), format(c(as.POSIXct(dataInput2()[,1]-60),as.POSIXct(dataInput2()[,1])), '%Y / %m / %d - %H : %M : %S'), cex.axis = .7)	
#      			 })
#  output$table <- renderTable({if(!is.null(mode$data)){
     #sum(time(oparray$data) == time(dataInput()[length(dataInput()[,4]),4]))}
     #oparray$data
 #    optimalret()[[4]]}
#	data.frame(as.Date(time(dataviewrsi())),as.Date(time(dataview())))}
 # })
  output$table2 <- renderTable({
 	autoInvalidate()
  	data.frame(format(dataInput2()[1],'%Y-%m-%d %H:%M:%S'),dataInput2()[2:length(dataInput2())])
  })
#  output$value <- renderPrint({ input$checkGroup })

#  output$text3 <- renderText({if(w$data == 1){
#	paste("Net profit (sum of monthly earning) =", numl())}else{
#        paste("Net profit (sum of monthly earning) =", calc(dataInput,4))}
#  })


outputOptions(output, "table2", suspendWhenHidden = FALSE) 
})

