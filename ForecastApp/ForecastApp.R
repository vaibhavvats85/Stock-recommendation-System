ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# usage
packages <- c("lubridate", "PerformanceAnalytics", "dygraphs", "shinyBS", "ggplot2", "quantmod","TTR","Hmisc","forecast","graphics")
ipak(packages)


library(lubridate)
library(PerformanceAnalytics)
library(dygraphs)
library(shinyBS)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(quantmod)
library(TTR)
require(Hmisc)
library(forecast)
library(graphics)

Stock<-read.csv("./ALLNEW.csv")

Stock$Date<-dmy(Stock$Date)

var<-read.csv("./ERN.csv")

var1<-read.csv("./ALL data month.csv")

var2<-read.csv("./year.csv")

ui<-dashboardPage(
  dashboardHeader(title="Stock Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stock Chart and Forecasting",tabName="scf"),
      menuItem("Risk Analysis",tabName="ra"),
      menuItem("Market fluctuation",tabName="mf"),
      menuItem("Reccomendation",tabName="rec"),
      menuItem("Indicators",tabName="ind", menuSubItem('RSI', tabName = 'rsi'),menuSubItem('MACD', tabName = 'macd')
               )
      
      
    )
    
  ),
  dashboardBody(tabItems(
    tabItem(tabName = "scf",
    box(width=9,
      dygraphOutput('plot1',height = 600)
    ),
    box(width=3,
      selectInput('ycol', 'Stock attributes', names(Stock)[4:15],
                  selected = names(Stock)[[4]])
      ),
    box(width=3,
      selectInput('xcol', 'Company',unique(Stock[,1]),selected=unique(Stock[1,1]))
    ),
    box(width=3,title = "Trend chart",
        actionButton("go1","Past trend"), 
        bsModal("modalnew1","Past Trend (2014/04/22-2017/04/21)","go1",dygraphOutput("pt1"),size="large")
        ),
    box(width=3,title="forecasts",
        numericInput("months", label = "Months to Predict",
                                               value = 36, min = 12, max = 60, step = 12),
        
      actionButton("goButton", "Actual Forecasts"),
      p(" Click to display forecasted prices"),
      bsModal("modalnew2","Forecast","goButton",size = "large",
              verbatimTextOutput("abc")),
      actionButton("go", "Forecast chart"),
      p(" Click to display forecast plot "),
      bsModal("modalnew","Forecast","go",size = "large",
              dygraphOutput("pt")
    )
  )
  ),
  tabItem(tabName = "ra",
          fluidRow(
          box(
            selectInput('qwe', 'Company',unique(Stock[,1]),selected=unique(Stock[1,1]))
          ),
          box(
            selectInput('poi', 'Stock attributes', names(Stock)[4:10],
                        selected = names(Stock)[[4]])
          ),
          box(
            plotOutput("down",width=800,height = 400),width=10,height = 400
          ),
          actionButton("risk", "Risk Chart",icon("paper-plane"), 
                       style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          p(" Click to display Risks "),
          bsModal("riskchart","Risks","risk",size = "large",
                  plotOutput("down1"),height=200),
          
          valueBoxOutput("value3",width=3),
         valueBoxOutput("value4",width=3),
          valueBoxOutput("value5",width=3),
         valueBoxOutput("value6",width=3)
          
  )),
  tabItem(tabName = "mf",h1("Past market fluctuations"),
        fluidRow(
          box(
              selectInput('xyz', 'Company',unique(Stock[,1]),selected=unique(Stock[1,1]))),
             
              valueBoxOutput("value1",width = 3)
              ,
              valueBoxOutput("value2",width=3),
          box(
            plotOutput("ggplot"),width=12
          )
          
          )),
  tabItem(tabName = "rsi",h1("Relative Strength Indicator(Based on Close Price)"),
          fluidRow(
            box(
              selectInput('rsigrid', 'Company',unique(Stock[,1]),selected=unique(Stock[1,1])))
              ),
          box(width=12,
              plotOutput("rsigg")
          ),
            valueBoxOutput("rsivalue1"),
            valueBoxOutput("rsivalue2")
          ),
  tabItem(tabName = "macd",h2("Moving Average Convergence Divergence"),
          fluidRow(
            box(
              selectInput('macdgrid', 'Company',unique(Stock[,1]),selected=unique(Stock[1,1]))
            ),
            box(
              selectInput('macdgrid2', 'Stock attributes', names(Stock)[4:10],
                                      selected = names(Stock)[[4]])
                  
            ),
          plotOutput("macdgg")
          )),
  tabItem(tabName = "rec",h2("Recommendation-NASDAQ Factors"),
         fluidRow( box(
            selectInput('recgrid', 'Company',unique(var1[,1]),selected=unique(var1[1,1]))
          ),
          box(width=8,
          verbatimTextOutput("nasdaq1"),
          verbatimTextOutput("nasdaq2"),
          verbatimTextOutput("nasdaq3"),
          verbatimTextOutput("nasdaq4"),
          verbatimTextOutput("nasdaq5"),
          verbatimTextOutput("nasdaq6"),
          verbatimTextOutput("nasdaq7"),
          verbatimTextOutput("nasdaq8"),
          verbatimTextOutput("nasdaq9")
          ),
          infoBoxOutput("info1"),
          valueBoxOutput("valnasdaq1"),
          valueBoxOutput("valnasdaq2")
          ))
          
          
)
)
)

server<-function(input,output){
 
   sidebar1<-function(){
  selectedData <- reactive({
    Stock[which(Stock$Symbol==input$xcol),input$ycol]
  })
  
  
  #selectedData2<-reactive({
  # select(selectedData(),input$ycol)
  #})
  select<-reactive({
    ts(selectedData(),frequency = 12 ,start=c(2014,5),end=c(2017,5))})
  
  output$plot1<-renderDygraph({
    dygraph(select(),main="Stock Chart")%>%
      dyOptions(drawGrid = input$showgrid)
  })
  
  selected1<-reactive({
    ts(selectedData())
  })
  
  observe({
    input$go1
    
    output$pt1<-renderDygraph({
      dygraph(selected1(),main="Past Trend")
      
      
    })
    
  })
  
  
  predicted <- reactive({
    hw <- HoltWinters(select())
    predict(hw,n.ahead=input$months)
  })
  
  
  
  ntext<-eventReactive(input$goButton,{
    print(round(predicted(),1))
  })
  observe({
    input$goButton
  
  output$abc<-renderPrint({
    #print("Forecasted values:")
    ntext()
  })
  })
  observe({
    input$go
    
    output$pt<-renderDygraph({
      dygraph(predicted(),main="Forecasts")%>%
        dyOptions(drawGrid = input$showgrid)
      
      
    })
    
  })
  

  
   }
   sidebar1()
MA<-function(){   
OH<-Stock$High.Price-Stock$Open.Price
  
OL<-Stock$Open.Price-Stock$Low.Price
  
OH_OL<-OH-OL
  
Stock<-data.frame(Stock,OH,OL,OH_OL)
  
Dominant<-ifelse(OH_OL>0,"High","Low")
  
Stock<-data.frame(Stock,Dominant)

selectedData1 <- reactive({
  Stock[which(Stock$Symbol==input$xyz),]
})
  
market1<-reactive(
  
x<-Stock[which(Stock$Symbol==input$xyz & Stock$Dominant=="High"),]
)

market<-reactive({
 Perc_high=(nrow(market1())/nrow(selectedData1()))*100
  
round(Perc_high,2) 
 
  
})
market2<-reactive({
  Perc_low=100-market()
  round(Perc_low,2)

  })
output$value1<-renderValueBox({
  valueBox(paste0(market(),"%"),"High",color="green",icon=icon("level-up"))
})
output$value2<-renderValueBox({
  valueBox(paste0(market2(),"%"),"Low",color="red",icon=icon("level-down"))
})

selectedData2<-reactive({
  Stock[which(Stock$Symbol==input$xyz),'OH']
})
selectedData3<-reactive({
  Stock[which(Stock$Symbol==input$xyz),'OH_OL']
})
selectedData4<-reactive({
  Stock[which(Stock$Symbol==input$xyz),'OL']
})
selectedData5<-reactive({
  Stock[which(Stock$Symbol==input$xyz),'Date']
}
)

a<-reactive({

p2<-ggplot(aes(x=selectedData5(),y=selectedData3()),data=selectedData1())+xlab("Date")+ylab("High-Low")+geom_line(colour="blue")+geom_line()+geom_line(aes(x=selectedData5(),y=selectedData2()),data=selectedData1(),colour="green")+geom_line(aes(x=selectedData5(),y=-selectedData4()),data=selectedData1(),colour="red")+stat_smooth(method = 'loess')
  plot(p2)
})
output$ggplot<-renderPlot({
  a()
})
}
MA()
RA<-function(){
  
  selectedData6<-reactive({
    
      Stock[which(Stock$Symbol==input$qwe),input$poi]
    })
  
  selectedData11<-reactive(
  xts(selectedData6(), order.by = Stock$Date[which(Stock$Symbol==input$qwe)])
  )

   
    selectedData7<- reactive({
  MACD(selectedData11(), nFast=12, nSlow=26, nSig=9, maType=SMA, percent = FALSE)
  
    })
    selectedData10<-reactive({
     Lag(ifelse(selectedData7()$macd < selectedData7()$signal, -1, 1))
    })
  selectedData9<-reactive({
     ROC(selectedData6())*selectedData10()
    
  })
  
  
selectedData8<-reactive({
 t(table.DownsideRisk(selectedData9(), Rf=.04/12, MAR =.05/12, p=.99))
  
    })

observe({
  input$risk
  output$down1<-renderPlot({
textplot(format.df(selectedData8(), na.blank=TRUE, numeric.dollar=FALSE,cdec=rep(3,dim(selectedData8())[2])), rmar = 1, cmar =0.5,max.cex=1,cex=2, halign = "center", valign = "top", row.valign="center",wrap.rownames=20, wrap.colnames=10, mar = c(0,0,3,0)+0.1)
            title(main="Risk Statistics summary")
  })
  
})
 
  output$down<-renderPlot({
    chart.Drawdown(selectedData9(),
                   main="Drawdown from Peak Equity Attained",
                   legend.loc="topright",colorset = 1:12)
  })

  
  
  output$value3<-renderValueBox({
    valueBox(selectedData8()[1,1],colnames(selectedData8())[1],color = "yellow",icon = icon("hand-o-right"))
  })
  output$value4<-renderValueBox({
    valueBox(selectedData8()[1,2],colnames(selectedData8())[2],color = "green",icon=icon("line-chart"))
  })
  output$value5<-renderValueBox({
    valueBox(selectedData8()[1,3],colnames(selectedData8())[3],color = "red",icon = icon("warning"))
  })
  output$value6<-renderValueBox({
    valueBox(selectedData8()[1,7],colnames(selectedData8())[7],color = "aqua",icon=icon("question-circle-o"))
  })
   }
RA()
ma<-function(){
  selectedmacd1<-reactive({
    
    Stock[which(Stock$Symbol==input$macdgrid),input$macdgrid2]
  })
  
  selectedmacd2<-reactive(
    xts(selectedmacd1(), order.by = Stock$Date[which(Stock$Symbol==input$macdgrid)])
  )
  
  
  selectedmacd3<- reactive({
    MACD(selectedmacd2(), nFast=12, nSlow=26, nSig=9, maType=SMA, percent = FALSE)
    
  })
  selectedmacd4<-reactive({
    x=selectedmacd3()$macd
    y=selectedmacd3()$signal
   x-y
  })
  
output$macdgg<-renderPlot({
  plot(selectedmacd4(),type="h")
})
  }
ma()

rsi<-function(){
  selectedrsi1<-reactive({
    
    Stock[which(Stock$Symbol==input$rsigrid),'Close.Price']
  })
  
  selectedrsi2<-reactive(
    xts(selectedrsi1(), order.by = Stock$Date[which(Stock$Symbol==input$rsigrid)])
  )
  
  
  selectedrsi3<- reactive({
    RSI(selectedrsi2())
    
  })
  selectedrsi4<-reactive(
    data.frame(selectedrsi1(),selectedrsi3())
  )
 selectedrsi5<-reactive(
   ymd(rownames(selectedrsi4()))
 )
 selectedrsi6<-reactive(
  selectedrsi4()$EMA
 )
 
 
 output$rsigg<-renderPlot(
  ggplot(selectedrsi4(),aes(selectedrsi5(),selectedrsi6()),colour="blue")+geom_line(colour="blue")+geom_line(aes(,30),colour="green")+geom_line(aes(,70),colour="red")
 )
}
rsi()

nasdaq<-function(){
  
  
    countyes = 0
    countno = 0
    #factor 1
  selectednasdaq1<-reactive({
      x1<-tail(var1[which(var1$Symbol==input$recgrid),"Total.Traded.Quantity"],50)
    mean(x1)
  })
 selectednasdaq2<-reactive({
    if(selectednasdaq1() > 250000){
      
      countyes <- countyes + 1
      
      a<-c("50 Day Average Data Volume is greater than 250000",countyes)
      a
    } else{
      
      countno <- 0
      
      b<-c("50 Day Average Data Volume is less than 250000",countno)
      b
    }
 })
  output$nasdaq1<-renderPrint({
  selectednasdaq2()
  })
  #factor 2
  selectednasdaq3<-reactive(
  x2<-tail(var1[which(var1$Symbol==input$recgrid),"Close.Price"],1)
  )
  selectednasdaq4<-reactive({
  y2<-var1[which(var1$Symbol==input$recgrid & var1$Date=="January"),"Close.Price"]
  mean(y2)
  })
  selectednasdaq5<-reactive({
  if(selectednasdaq3() > selectednasdaq4()){
    
    countyes<-countyes  + 1
    
    c<-c("Short Term Price Movement is higher than 3 month ago",countyes)
  c
    } else{
      countno <- 0
    
    d<-c("Short Term Price Movement is less than 3 month ago",countno)
    d
  }
  })
  
  output$nasdaq2<-renderPrint(
    selectednasdaq5()
  )
  
  #factor 3
  selectednasdaq6<-reactive(
  x3<-tail(var2[which(var2$Symbol==input$recgrid),"Close.Price"],1)
  )
  selectednasdaq7<-reactive({
  c<-var2[which(var2$Symbol==input$recgrid & var2$Date=="2016"),"Close.Price"]
  mean(c)
  })
  selectednasdaq8<-reactive({
  if(selectednasdaq6() > selectednasdaq7()){
    countyes <- countyes + 1
    e<-c("Long Term Price Movement is YES",countyes)
e
      } else {
    countno <- 0
    f<-c("Long Term Price Movement is NO",countno)
    f
  }
  })
  
  output$nasdaq3<-renderPrint(
    selectednasdaq8()
  )
  #factor 4
  selectednasdaq9<-reactive(
  r1<-var[which(var$Symbol==input$recgrid & var$Narration=="13-Mar"),"Revenue"]
  )
  selectednasdaq10<-reactive(
  r2<-var[which(var$Symbol==input$recgrid & var$Narration=="14-Mar"),"Revenue"]
  )
  selectednasdaq11<-reactive(
  r3<-var[which(var$Symbol==input$recgrid & var$Narration=="15-Mar"),"Revenue"]
  )
  selectednasdaq12<-reactive(
  r4<-var[which(var$Symbol==input$recgrid & var$Narration=="16-Mar"),"Revenue"]
  )
  
  selectednasdaq13<-reactive({
  if(selectednasdaq9() > selectednasdaq10() & selectednasdaq10() > selectednasdaq11() & selectednasdaq11() > selectednasdaq12()) {
    countyes <- countyes + 1
    g<-c("Revenue is increasing Over 4 Years",countyes)
    g
      } else {
    countno <- 0
    h<-c("Revenue is not increasing Over 4 Years",countno)
    h
  }  
  })
  output$nasdaq4<-renderPrint(
    selectednasdaq13()
  )
  #factor 5
  selectednasdaq14<-reactive(
  x5<-var[which(var$Symbol==input$recgrid),"EPS"]
  )
  selectednasdaq15<-reactive({
  if(selectednasdaq14()[1] < selectednasdaq14()[2] & selectednasdaq14()[2] < selectednasdaq14()[3] & selectednasdaq14()[3] < selectednasdaq14()[4]){
    countyes <- countyes + 1
    i<-c("EPS is increasing Over 4 Years",countyes)
    i
  } else{
    countno <- 0
    j<-c("EPS is  not increasing Over 4 Years",countno)
    j
  }
  })
  output$nasdaq5<-renderPrint(
    selectednasdaq15()
  )
  #factor 6
  selectednasdaq16<-reactive(
  x6<-var[which(var$Symbol==input$recgrid & var$Narration=="14-Mar"),"Net.profit"]
)
  selectednasdaq17<-reactive(
  y6<-var[which(var$Symbol==input$recgrid & var$Narration=="15-Mar"),"Net.profit"]
  )
  
  selectednasdaq18<-reactive({
  if(selectednasdaq17() > selectednasdaq16()){
    countyes <- countyes + 1
    k<-c("Net Income is increasing year over year",countyes)
k
      } else {
    countno <- 0
    l<-c("Net Income is not increasing year over year",countno)
  l
    }
    })
  output$nasdaq6<-renderPrint(
    selectednasdaq18()
  )

  #factor 8
  selectednasdaq19<-reactive(
  x8<-var[which(var$Symbol==input$recgrid),"Price"]
  )
  selectednasdaq20<-reactive({
  mts<-ts(selectednasdaq19(),start=2013,end=2016,frequency = 1)
  fit<-auto.arima(mts)
  data.frame(forecast(fit,1))
})
  selectednasdaq21<-reactive(
  a<-selectednasdaq20()$Point.Forecast[1]
  )
  selectednasdaq22<-reactive(
  y8<-tail(var[which(var$Symbol==input$recgrid),"Price"],1)
  )
  
selectednasdaq23<-reactive({  
  if(selectednasdaq21() > selectednasdaq22()){
    countyes <- countyes + 1
    m<-c("The 12 month price target is above the current stock price",countyes)
  m
    } else{
    countno <- 0
    n<-c("The 12 month price target is not above the current stock price",countno)
  n
    }
})
output$nasdaq7<-renderPrint(
  selectednasdaq23()
)


#factor 10
selectednasdaq24<-reactive(
x10<-var[which(var$Symbol==input$recgrid),"EPS"]
)
selectednasdaq25<-reactive({
mts<-ts(selectednasdaq24(),start=2013,end = 2016,frequency = 1)
fit<-auto.arima(mts)
data.frame(forecast(fit,3))
})
selectednasdaq26<-reactive(
a<-selectednasdaq25()$Point.Forecast[1]
)
selectednasdaq27<-reactive(
b<-selectednasdaq25()$Point.Forecast[2]
)
selectednasdaq28<-reactive(
c<-selectednasdaq25()$Point.Forecast[3]
)
selectednasdaq29<-reactive({
if(selectednasdaq26() < selectednasdaq27() & selectednasdaq27() < selectednasdaq28()){
  countyes <- countyes + 1
  o<-c("Earning forecast is increasing year over year",countyes)
  o
} else{
  countno <- 0
  p<-c("Earning forecast is not increasing year over year",countno)
  p
}
})
output$nasdaq8<-renderPrint(
   selectednasdaq29()
)
#factor 12
selectednasdaq30<-reactive(
x12<-tail(var[which(var$Symbol==input$recgrid),"IPR"],1)
)
selectednasdaq31<-reactive(
y12<-tail(var[which(var$Symbol==input$recgrid),"CPR"],1)
)
selectednasdaq32<-reactive({
if(selectednasdaq31() > selectednasdaq30()){
  countyes <- countyes + 1
  q<-c("Company's P/E ratio is lower than its industry P/E ratio",countyes)
  q
} else {
  countno <- countno + 1
  r<-c("Company's P/E ratio is greater than its industry P/E ratio",countno)
  r
}
  })
output$nasdaq9<-renderPrint(
  selectednasdaq32()
)
selectednasdaq33<-reactive(
  print(input$recgrid)
)
output$info1<-renderInfoBox(
  infoBox("Stock Symbol",selectednasdaq33())
)

selectednasdaq34<-reactive(
  as.numeric(selectednasdaq2()[2])+as.numeric(selectednasdaq5()[2])+as.numeric(selectednasdaq8()[2])+as.numeric(selectednasdaq13()[2])+as.numeric(selectednasdaq15()[2])+as.numeric(selectednasdaq18()[2])+as.numeric(selectednasdaq23()[2])+as.numeric(selectednasdaq29()[2])+as.numeric(selectednasdaq32()[2])
)

 

  
  selectednasdaq35<-reactive({
  s<-9-selectednasdaq34()
  s
})
  selectednasdaq36<-reactive({
    
    total = selectednasdaq34()+selectednasdaq35()
    rp = (selectednasdaq34()/total) * 100
    rp
  })
  
  
    
  output$valnasdaq1<-renderValueBox(
    valueBox(selectednasdaq33(),ifelse(selectednasdaq34()>selectednasdaq35(),"Recommended","Not recommended"),color = ifelse(selectednasdaq34()>selectednasdaq35(),"green","red"),icon=icon(ifelse(selectednasdaq34()>selectednasdaq35(),"hand-o-up","hand-o-down")))
  )
output$valnasdaq2<-renderValueBox(
  valueBox("Reliability Factor",ifelse(selectednasdaq34()>selectednasdaq35(),paste0(round(selectednasdaq36(),2),"%"),"Nil"),color=ifelse(selectednasdaq34()>selectednasdaq35(),"green","red"),icon=icon(ifelse(selectednasdaq34()>selectednasdaq35(),"hand-o-up","hand-o-down")))
)

  }

nasdaq()
}
shinyApp(ui,server)