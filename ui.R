library(shiny)
library(shinyWidgets)
library(shinyjs)
library(dygraphs)
library(shinythemes)
shinyUI(fluidPage(
  useShinyjs(),
  theme = shinytheme("lumen"),
  #shinyWidgets::shinyWidgetsGallery(),
  titlePanel("Stock Price"),
  sidebarLayout(
    sidebarPanel(
	 hidden( dateRangeInput("dateG",'Date input: yyyy-mm-dd',                  
		  start  = as.character(Sys.Date()-90),
                  end    = as.character(Sys.Date()-1),
                  min    = "2001-01-01",
                  max    = as.character(Sys.Date()),
                  format = "mm/dd/yy",
                  separator = " - ")),
         hidden(selectInput("indicatorG", 
        label = "Indicator",
        choices = list("RSI","CCI","Bbands","SMA", "SMI","SAR"),
        selected = "SMA")),
	hidden( switchInput(inputId = "checkG",value = TRUE, onLabel = "Smaller",
  	offLabel = "Greater",offStatus="primary")),
      #helpText("Stock price"),
  #    submitButton("Submit"),
      #textInput("symb", "Symbol", "AAPL"),
      selectInput("symb", 
        label = "Choose a Stock",
        choices = list("AAPL", "CELG",
          "JPM", "XOM","ABX"),
        selected = "AAPL"), 
       hidden(selectInput("strategyG", label = "Strategy",
        choices = list("Simple", "Trading Indicators","Rscript"),selected = "Trading Indicators")),     
       selectInput("trade", 
        label = "Trading Strategy",
        choices = list("Simple", "Trading Indicators",
          "Rscript"),
        selected = "Simple"),
       dateRangeInput("dates", "Date range for backtesting:",
                  start  = "2016-01-01",
                  end    = as.character(Sys.Date()-1),
                  min    = "2001-01-01",
                  max    = as.character(Sys.Date()),
                  format = "mm/dd/yy",
                  separator = " - "),

      fluidRow(
      column(6, hidden(actionButton(inputId = "analyzeG",label = "Go Analyze",width = "100px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))),
      column(6, hidden(actionButton(inputId = "playG",label = "Go Play",width = "100px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                        ),
      fluidRow(
                        column(7,  hidden(tags$a("when you purchases the first one?",id="tgq1"))),
                        column(5,  hidden(textOutput("txgame1")))
                        ), 
      fluidRow(
                        column(7,  hidden(tags$a("when you purchases the second one?",id="tgq2"))),
                        column(5,  hidden(textOutput("txgame2")))
                        ), 
      fluidRow(
                        column(7,  hidden(tags$a("what you earning right now?",id="tgq3"))),
                        column(5,  hidden(textOutput("txgame3")))
                        ), 
      fluidRow(
                        column(7,  hidden(tags$a("best threshold for buying?",id="taq1"))),
                        column(5,  hidden(textOutput("txanal1")))
                        ), 
      fluidRow(
                        column(7,  hidden(tags$a("best threshold for selling?",id="taq2"))),
                        column(5,  hidden(textOutput("txanal2")))
                        ), 
      fluidRow(
                        column(7,  hidden(tags$a("what is the return?",id="taq3"))),
                        column(5,  hidden(textOutput("txanal3")))
                        ), 
      fluidRow(
      column(10,hidden(actionButton(inputId = "ReportA",label = "Generate Report",width = "280px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                        ),
      fluidRow(
			column(12,textOutput("textn2"))
			),
#      fluidRow(
#			column(12,textOutput("textn"))
#			),
      fluidRow(
			column(12,textOutput("textn3"))
			),
      fluidRow(
			column(12,textOutput("textn33"))
			),
     fluidRow(
			column(6,textOutput("text17")),
			column(6,textOutput("text18"))
			),
      fluidRow(
			column(6,textOutput("text19")),
			column(6,textOutput("text20"))
			),
      fluidRow(
			column(12,textOutput("text21"))
			),

      fluidRow(
			column(12,textOutput("text21j"))
			),
      hidden(textOutput("Whenb")),
      fluidRow(
                        column(4,  hidden(textInput("LAG1",label = "RSI",value="T-1"))),
			column(4,  hidden(selectInput("RSI", label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv1",label = "value",value="")))
                        ), 
      fluidRow(
                        column(4,  hidden(textInput("LAG2",label = "",value="T"))),
			column(4,  hidden(selectInput("RSI0", label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv10",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And1",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAG3",label = "CCI",value="T-1"))),	
                        column(4,  hidden(selectInput("CCI",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv2",label = "value",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAG4",label = "",value="T"))),	
                        column(4,  hidden(selectInput("CCI0",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv20",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And2",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAG5",label = "BB",value="T-1"))),
                        column(4,  hidden(selectInput("BB",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv3",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAG6",label = "",value="T"))),
                        column(4,  hidden(selectInput("BB0",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv30",label = "",value="")))
                        ),  
      fluidRow(
			column(5,  hidden(selectInput("And3",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),  
      fluidRow(
                        column(4,  hidden(textInput("LAGn1",label = "SMA",value="T-1"))),
                        column(4,  hidden(selectInput("SMA",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn1",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn2",label = "",value="T"))),
                        column(4,  hidden(selectInput("SMA0",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn10",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And4",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAGn3",label = "SMI",value="T-1"))),
                        column(4,  hidden(selectInput("SMI",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn2",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn4",label = "",value="T"))),
                        column(4,  hidden(selectInput("SMI0",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn20",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And5",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn5",label = "SAR",value="T-1"))),
                        column(4,  hidden(selectInput("SAR",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn3",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn6",label = "",value="T"))),
                        column(4,  hidden(selectInput("SAR0",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn30",label = "",value="")))
                        ),
      fluidRow(
     # column(7,""),
      column(1, hidden(actionButton(inputId = "Buy",label = "Buy",width = "220px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                        ),
       hidden(textOutput("Whens")),
      fluidRow(
                        column(4,  hidden(textInput("LAG7",label = "RSI",value="T-1"))),
			column(4,  hidden(selectInput("RSI1", label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv11",label = "value",value="")))
                        ), 
      fluidRow(
                        column(4,  hidden(textInput("LAG8",label = "",value="T"))),
			column(4,  hidden(selectInput("RSI10", label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv110",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And11",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAG9",label = "CCI",value="T-1"))),	
                        column(4,  hidden(selectInput("CCI1",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv12",label = "value",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAG10",label = "",value="T"))),	
                        column(4,  hidden(selectInput("CCI10",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv120",label = "",value="")))
                        ),

      fluidRow(
			column(5,  hidden(selectInput("And12",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAG11",label = "BB",value="T-1"))),
                        column(4,  hidden(selectInput("BB1",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv13",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAG12",label = "",value="T"))),
                        column(4,  hidden(selectInput("BB10",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsiv130",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And13",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),  
      fluidRow(
                        column(4,  hidden(textInput("LAGn7",label = "SMA",value="T-1"))),
                        column(4,  hidden(selectInput("SMA1",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn11",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn8",label = "",value="T"))),
                        column(4,  hidden(selectInput("SMA10",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn110",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And14",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),    
      fluidRow(
                        column(4,  hidden(textInput("LAGn9",label = "SMI",value="T-1"))),
                        column(4,  hidden(selectInput("SMI1",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn12",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn10",label = "",value="T"))),
                        column(4,  hidden(selectInput("SMI10",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn120",label = "",value="")))
                        ),
      fluidRow(
			column(5,  hidden(selectInput("And15",label = "OR/AND",choices = list("AND", "OR"),selected = "AND")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn11",label = "SAR",value="T-1"))),
                        column(4,  hidden(selectInput("SAR1",label = "sign",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn13",label = "val",value="")))
                        ),
      fluidRow(
                        column(4,  hidden(textInput("LAGn12",label = "",value="T"))),
                        column(4,  hidden(selectInput("SAR10",label = "",choices = list("<", "=",">","<=",">="),selected = "<"))),
                        column(3,  hidden(textInput("rsivn130",label = "",value="")))
                        ),

      fluidRow(
      column(1, hidden(actionButton(inputId = "Sell",label = "Sell",width = "220px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                        ),
 #     textOutput("textp"),
 #     textOutput("textp1"),
      fluidRow(
		        column(12,hidden(textOutput("te1")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tek1")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te2")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te4")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te4n")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tem11")))
		),

      fluidRow(
		        column(12,hidden(textOutput("tem21")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tem31")))
		),

      fluidRow(
		        column(12,hidden(textOutput("tek2")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te3")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te5")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te5n")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tem12")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tem22")))
		),
      fluidRow(
		        column(12,hidden(textOutput("tem32")))
		),
      fluidRow(
		        column(12,hidden(textOutput("te6")))
		),
      fluidRow(
		        column(12,hidden(textOutput("text9")))
		),
      fluidRow(
		        column(12,hidden(textOutput("text9m")))
		),
      fluidRow(
			column(6,hidden(textOutput("text10"))),
			column(6,hidden(textOutput("text11")))
			),
      fluidRow(
			column(6,hidden(textOutput("text12"))),
			column(6,hidden(textOutput("text13")))
			),
      fluidRow(
			column(12,hidden(textOutput("text14p")))
			),
      fluidRow(
			column(12,hidden(textOutput("text14")))
			),
      fluidRow(
      column(10,hidden(actionButton(inputId = "Report",label = "Report",width = "280px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                        ),
      hidden(fileInput("file1", "Input Trading strategy script"))
#      fluidRow(
#			column(12,textOutput("text15"))
#			),
#      fluidRow(
#			column(12,textOutput("text16"))
#			),
#      fluidRow(
#			column(12,textOutput("textn4"))
#			),
#      fluidRow(
#			column(12,textOutput("textn5"))
#			),
#      fluidRow(
#			column(12,tags$h4(tags$em(textOutput("textn6"))))
#			),
#      fluidRow(
#			column(12,tableOutput("table10"))
#			),
 #     h5(tableOutput('table10'),align="center")

    #  actionButton(inputId = "refresh",
    #  label = "Refresh", icon = icon("fa fa-refresh"))

       
      # selectInput("dataset", "Choose a dataset:", 
     #           choices = c("Prices", "Open price beginning of the week", "Close price at the end of the week", "Earning - monthly based")),
     # downloadButton('downloadData', 'Download')
      ),
    mainPanel(actionButton("normal", "Normal mode"),actionButton("game", "Game mode"),
h5(tableOutput('table2'),align="center"),h5(actionLink("gt1","1 day"),tags$a("  -  ",id="tt1"),actionLink("gt2","1 week"),tags$a("  -  ",id="tt2"),actionLink("gt3","1 month"),tags$a("  -  ",id="tt3"),actionLink("gt4","3 month"),tags$a("  -  ",id="tt4"),actionLink("gt5","1 year"),align="right"),dygraphOutput("dygraph",width="100%"),dygraphOutput("dygraph2",width="100%",height="100px"),dygraphOutput("dygraph3",width="100%",height="100px"),dygraphOutput("dygraph4",width="100%",height="100px"),dygraphOutput("dygraph5",width="100%",height="100px"),dygraphOutput("dygraph6",width="100%",height="100px"),dygraphOutput("dygraph7",width="100%",height="100px"),h3(hidden(radioButtons("checkGroup", label = h3("Make Decision:"),
    choices = list("you want to buy" = 1, "you want to sell" = 2, "you want to continue" = 3), 
    selected = 3)),style="color:blue"),h3(hidden(actionButton(inputId = "nextb",label = "Next",width = "180px",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))))
        #     ,h5(tableOutput('table'),align="center"))
    #          ,h4(textOutput("text3"), align="center",style = "color:blue"))
  )
))
