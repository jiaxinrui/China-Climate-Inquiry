
library(vcd)
library(RColorBrewer)
library(readxl)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dashboard)
library(maps)
library(mapdata)
data1<-read_excel("Temperature.xlsx")
data2<-read_excel("Windlevel.xlsx")
data3<-read_excel("Precipitation.xlsx")
header <- dashboardHeader(title = "Welcome to China")
sidebar <- dashboardSidebar(
selectInput("column","please choose a city:",c(data1$City))
)
body <- dashboardBody(
  textOutput("caption"),
  plotOutput("plot1"),
  plotOutput("plot"),
  plotOutput("barplot"),
  plotOutput("plot3")
)
ui <- dashboardPage(header, sidebar, body)
MONTH<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sept","Oct"," Nov","Dec")
server <- function(input, output){
  output$caption=renderText(paste("Display Temperature:",input$column,sep = ""))
  output$caption=renderText(paste(input$column,"-","Temperature",sep = ""))

  output$barplot <- renderPlot({
    if(input$column=="BEIJING"){
      a1<-as.numeric(data1[1,2])
      a2<-as.numeric(data1[1,3])
      a3<-as.numeric(data1[1,4])
      a4<-as.numeric(data1[1,5])
      a5<-as.numeric(data1[1,6])
      a6<-as.numeric(data1[1,7])
      a7<-as.numeric(data1[1,8])
      a8<-as.numeric(data1[1,9])
      a9<-as.numeric(data1[1,10])
      a10<-as.numeric(data1[1,11])
      a11<-as.numeric(data1[1,12])
      a12<-as.numeric(data1[1,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="SHANGHAI"){
      a1<-as.numeric(data1[2,2])
      a2<-as.numeric(data1[2,3])
      a3<-as.numeric(data1[2,4])
      a4<-as.numeric(data1[2,5])
      a5<-as.numeric(data1[2,6])
      a6<-as.numeric(data1[2,7])
      a7<-as.numeric(data1[2,8])
      a8<-as.numeric(data1[2,9])
      a9<-as.numeric(data1[2,10])
      a10<-as.numeric(data1[2,11])
      a11<-as.numeric(data1[2,12])
      a12<-as.numeric(data1[2,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="HAERBIN"){
      a1<-as.numeric(data1[3,2])
      a2<-as.numeric(data1[3,3])
      a3<-as.numeric(data1[3,4])
      a4<-as.numeric(data1[3,5])
      a5<-as.numeric(data1[3,6])
      a6<-as.numeric(data1[3,7])
      a7<-as.numeric(data1[3,8])
      a8<-as.numeric(data1[3,9])
      a9<-as.numeric(data1[3,10])
      a10<-as.numeric(data1[3,11])
      a11<-as.numeric(data1[3,12])
      a12<-as.numeric(data1[3,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="CHENGDU"){
      a1<-as.numeric(data1[4,2])
      a2<-as.numeric(data1[4,3])
      a3<-as.numeric(data1[4,4])
      a4<-as.numeric(data1[4,5])
      a5<-as.numeric(data1[4,6])
      a6<-as.numeric(data1[4,7])
      a7<-as.numeric(data1[4,8])
      a8<-as.numeric(data1[4,9])
      a9<-as.numeric(data1[4,10])
      a10<-as.numeric(data1[4,11])
      a11<-as.numeric(data1[4,12])
      a12<-as.numeric(data1[4,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="HANGZHOU"){
      a1<-as.numeric(data1[5,2])
      a2<-as.numeric(data1[5,3])
      a3<-as.numeric(data1[5,4])
      a4<-as.numeric(data1[5,5])
      a5<-as.numeric(data1[5,6])
      a6<-as.numeric(data1[5,7])
      a7<-as.numeric(data1[5,8])
      a8<-as.numeric(data1[5,9])
      a9<-as.numeric(data1[5,10])
      a10<-as.numeric(data1[5,11])
      a11<-as.numeric(data1[5,12])
      a12<-as.numeric(data1[5,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="CHONGQING"){
      a1<-as.numeric(data1[6,2])
      a2<-as.numeric(data1[6,3])
      a3<-as.numeric(data1[6,4])
      a4<-as.numeric(data1[6,5])
      a5<-as.numeric(data1[6,6])
      a6<-as.numeric(data1[6,7])
      a7<-as.numeric(data1[6,8])
      a8<-as.numeric(data1[6,9])
      a9<-as.numeric(data1[6,10])
      a10<-as.numeric(data1[6,11])
      a11<-as.numeric(data1[6,12])
      a12<-as.numeric(data1[6,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="SANYA"){
      a1<-as.numeric(data1[7,2])
      a2<-as.numeric(data1[7,3])
      a3<-as.numeric(data1[7,4])
      a4<-as.numeric(data1[7,5])
      a5<-as.numeric(data1[7,6])
      a6<-as.numeric(data1[7,7])
      a7<-as.numeric(data1[7,8])
      a8<-as.numeric(data1[7,9])
      a9<-as.numeric(data1[7,10])
      a10<-as.numeric(data1[7,11])
      a11<-as.numeric(data1[7,12])
      a12<-as.numeric(data1[7,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="QINGDAO"){
      a1<-as.numeric(data1[8,2])
      a2<-as.numeric(data1[8,3])
      a3<-as.numeric(data1[8,4])
      a4<-as.numeric(data1[8,5])
      a5<-as.numeric(data1[8,6])
      a6<-as.numeric(data1[8,7])
      a7<-as.numeric(data1[8,8])
      a8<-as.numeric(data1[8,9])
      a9<-as.numeric(data1[8,10])
      a10<-as.numeric(data1[8,11])
      a11<-as.numeric(data1[8,12])
      a12<-as.numeric(data1[8,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="ZHANGJIAJIE"){
      a1<-as.numeric(data1[9,2])
      a2<-as.numeric(data1[9,3])
      a3<-as.numeric(data1[9,4])
      a4<-as.numeric(data1[9,5])
      a5<-as.numeric(data1[9,6])
      a6<-as.numeric(data1[9,7])
      a7<-as.numeric(data1[9,8])
      a8<-as.numeric(data1[9,9])
      a9<-as.numeric(data1[9,10])
      a10<-as.numeric(data1[9,11])
      a11<-as.numeric(data1[9,12])
      a12<-as.numeric(data1[9,13])
      EM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    if(input$column=="XIAN"){
      a1<-as.numeric(data1[10,2])
      a2<-as.numeric(data1[10,3])
      a3<-as.numeric(data1[10,4])
      a4<-as.numeric(data1[10,5])
      a5<-as.numeric(data1[10,6])
      a6<-as.numeric(data1[10,7])
      a7<-as.numeric(data1[10,8])
      a8<-as.numeric(data1[10,9])
      a9<-as.numeric(data1[10,10])
      a10<-as.numeric(data1[10,11])
      a11<-as.numeric(data1[10,12])
      a12<-as.numeric(data1[10,13])
      TEM<-c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12)
      colors <- c("green","orange","brown")
      barplot(TEM,names.arg = MONTH,xlab="Month",ylab="Temperature",col=colors)}
    })
  
  output$plot <- renderPlot({
    if(input$column=="BEIJING"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[1,2],data1[1,3],data1[1,4],data1[1,5],data1[1,6],data1[1,7],data1[1,8],data1[1,9],data1[1,10],data1[1,11],data1[1,12],data1[1,13])
      plot(MONTH, WINDLEVEL,col="green",fg="pink")}
    if(input$column=="SHANGHAI"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[2,2],data1[2,3],data1[2,4],data1[2,5],data1[2,6],data1[2,7],data1[2,8],data1[2,9],data1[2,10],data1[2,11],data1[2,12],data1[2,13])
      plot(MONTH, WINDLEVEL,col="pink",fg="green")}
    if(input$column=="HAERBIN"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      wINDLEVEL<-c(data1[3,2],data1[3,3],data1[3,4],data1[3,5],data1[3,6],data1[3,7],data1[3,8],data1[3,9],data1[3,10],data1[3,11],data1[3,12],data1[3,13])
      plot(MONTH, WINDLEVEL,col="blue",fg="yellow")}
    if(input$column=="CHENGDU"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[4,2],data1[4,3],data1[4,4],data1[4,5],data1[4,6],data1[4,7],data1[4,8],data1[4,9],data1[4,10],data1[4,11],data1[4,12],data1[4,13])
      plot(MONTH, WINDLEVEL,col="red",fg="blue")}
    if(input$column=="HANGZHOU"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[5,2],data1[5,3],data1[5,4],data1[5,5],data1[5,6],data1[5,7],data1[5,8],data1[5,9],data1[5,10],data1[5,11],data1[5,12],data1[5,13])
      plot(MONTH, WINDLEVEL,col="black",fg="green")}
    if(input$column=="CHONGQING"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[6,2],data1[6,3],data1[6,4],data1[6,5],data1[6,6],data1[6,7],data1[6,8],data1[6,9],data1[6,10],data1[6,11],data1[6,12],data1[6,13])
      plot(MONTH, WINDLEVEL,col="yellowgreen",fg="red")}
    if(input$column=="SANYA"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[7,2],data1[7,3],data1[7,4],data1[7,5],data1[7,6],data1[7,7],data1[7,8],data1[7,9],data1[7,10],data1[7,11],data1[7,12],data1[7,13])
      plot(MONTH, WINDLEVEL,col="purple",fg="blue")}
    if(input$column=="QINGDAO"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[8,2],data1[8,3],data1[8,4],data1[8,5],data1[8,6],data1[8,7],data1[8,8],data1[8,9],data1[8,10],data1[8,11],data1[8,12],data1[8,13])
      plot(MONTH, WINDLEVEL,col="black",fg="green")}
    if(input$column=="ZHANGJIAJIE"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[9,2],data1[9,3],data1[9,4],data1[9,5],data1[9,6],data1[9,7],data1[9,8],data1[9,9],data1[9,10],data1[9,11],data1[9,12],data1[9,13])
      plot(MONTH, WINDLEVEL,col="brown",fg="blue")}
    if(input$column=="XIAN"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      WINDLEVEL<-c(data1[10,2],data1[10,3],data1[10,4],data1[10,5],data1[10,6],data1[10,7],data1[10,8],data1[10,9],data1[10,10],data1[10,11],data1[10,12],data1[10,13])
      plot(MONTH, WINDLEVEL,col="orange",fg="green")}
  })
  
  output$plot3 <- renderPlot({
    if(input$column=="BEIJING"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[1,2],data3[1,3],data3[1,4],data3[1,5],data3[1,6],data3[1,7],data3[1,8],data3[1,9],data3[1,10],data3[1,11],data3[1,12],data3[1,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="SHANGHAI"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[2,2],data3[2,3],data3[2,4],data3[2,5],data3[2,6],data3[2,7],data3[2,8],data3[2,9],data3[2,10],data3[2,11],data3[2,12],data3[2,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="HAERBIN"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[3,2],data3[3,3],data3[3,4],data3[3,5],data3[3,6],data3[3,7],data3[3,8],data3[3,9],data3[3,10],data3[3,11],data3[3,12],data3[2,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="CHENGDU"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[4,2],data3[4,3],data3[4,4],data3[4,5],data3[4,6],data3[4,7],data3[4,8],data3[4,9],data3[4,10],data3[4,11],data3[4,12],data3[2,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="HANGZHOU"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[5,2],data3[5,3],data3[5,4],data3[5,5],data3[5,6],data3[5,7],data3[5,8],data3[5,9],data3[5,10],data3[5,11],data3[5,12],data3[2,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="CHONGQING"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[6,2],data3[6,3],data3[6,4],data3[6,5],data3[6,6],data3[6,7],data3[6,8],data3[6,9],data3[2,10],data3[6,11],data3[6,12],data3[6,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="SANYA"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[2,2],data3[7,3],data3[7,4],data3[7,5],data3[7,6],data3[7,7],data3[7,8],data3[7,9],data3[7,10],data3[7,11],data3[7,12],data3[7,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="QINGDAO"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[8,2],data3[8,3],data3[8,4],data3[8,5],data3[8,6],data3[8,7],data3[8,8],data3[8,9],data3[8,10],data3[8,11],data3[8,12],data3[8,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="ZHANGJIAJIE"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[9,2],data3[9,3],data3[9,4],data3[9,5],data3[9,6],data3[9,7],data3[9,8],data3[9,9],data3[9,10],data3[9,11],data3[9,12],data3[9,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
    if(input$column=="XIAN"){
      MONTH<-c(1,2,3,4,5,6,7,8,9,10,11,12)
      PRECIPITATION<-c(data3[10,2],data3[10,3],data3[10,4],data3[10,5],data3[10,6],data3[10,7],data3[10,8],data3[10,9],data3[10,10],data3[10,11],data3[10,12],data3[10,13])
      plot(MONTH,PRECIPITATION,col="red",bg="yellow",xlab="Month",ylab="Precipitation",col.main="green",font.main=2,type="b")
    }
  })
  
  output$plot1 <- renderPlot({
    if(input$column=="BEIJING"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     BEIJING,116.4666667,39.9
                     
                     ")
      
      map("china",  ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
    }
    if(input$column=="SHANGHAI"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     SHANGHAI,121.4833333,31.23333333
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
    }
    if(input$column=="HAERBIN"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     HAERBIN,126.6833333,45.75
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="CHONGQING"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     CHONGQING,106.5333333,29.53333333
                     
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="SANYA"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     SANYA,110,19.09
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="QINGDAO"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     QINGDAO,119.30,35.35
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="XIAN"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     XIAN,108.56,34.15
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="ZHANGJIAJIE"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     ZHANGJIAJIE,110.47,29.13
                     
                     
                     ")
      
      map("china",ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="CHENGDU"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     CHENGDU,104.07,30.67
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
    if(input$column=="HANGZHOU"){
      par(mar=rep(0,4))
      dat = read.csv(text = "city,jd,wd
                     HANGZHOU,120.15,30.28
                     
                     
                     ")
      
      map("china", ylim = c(18, 54),fill = TRUE, col = rainbow(200), panel.first = grid())
      points(dat$jd, dat$wd, pch = 19, col = rgb(0, 0, 0, 0.5))
      text(dat$jd, dat$wd, dat[, 1], cex = 0.9, col = rgb(0,
                                                          0, 0, 0.7), pos = c(2, 4, 4, 4, 3, 4, 2, 3, 4, 2, 4, 2, 2,
                                                                              4, 3, 2, 1, 3, 1, 1, 2, 3, 2, 2, 1, 2, 4, 3, 1, 2, 2, 4, 4, 2))
      axis(1, lwd = 0); axis(2, lwd = 0); axis(3, lwd = 0); axis(4, lwd = 0)
      
      
    }
  })
    
}

shinyApp(ui = ui,server = server)
