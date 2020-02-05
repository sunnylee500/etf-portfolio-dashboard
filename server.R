#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(timetk)
library(tidyverse)
library(tidyquant)
library(highcharter)
library(googleVis)
etfs <- read.csv(file = "/Users/YunseonLee/Desktop/NYCDSA_Proj/etf_shiny/etfs.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    vtickers <- reactive({
        vt <- as.character(unlist(strsplit(input$tickers,",")))
        vt
    })
    
    vweights <- reactive({
        vw <- as.numeric(unlist(strsplit(input$weights,",")))
        vw
    })
    
    #Table of ETF info: ticker, name, L1, L2
    etf_names <- reactive({
            validate(need(input$tickers, "Please select tickers"))
            vt <- vtickers()
            etf_info <- etfs%>%filter(Symbol%in%vt)
            etf_info
    })
    
    #Table of monthly prices given the tickers 
    prices_monthly <- reactive({
        if(input$submit > 0){
            vt <- isolate(vtickers())
            pm <- getSymbols(vt, src='yahoo')%>%
                map(~Ad(get(.)))%>%
                reduce(merge)%>%
                na.omit()%>%
                to.monthly(indexAt='last',OHLC=FALSE)%>%
                `colnames<-`(vt)
            pm
        }
    })
    
    #Table of returns given the tickers
    ticker_returns <- reactive({
        if(input$submit > 0){
            pm <- isolate(prices_monthly())
            treturns <- na.omit(Return.calculate(pm, method = "log"))
            treturns
        }
    })
    
    #Character of shared start + end date 
    timeframe <- reactive({
        if(input$submit > 0){
            treturns <- isolate(ticker_returns())
            start_date <- index(treturns[1])
            end_date <- index(tail(treturns, n=1))
            paste(start_date,end_date, sep=" to ")
        }
    })
    
    #List of risk contribution
    ticker_risks <- reactive({
        if(input$submit > 0){
            treturns <- isolate(ticker_returns())
            vw <- isolate(vweights())
            trisks <- StdDev(treturns, weights=vw, portfolio_method="component")
            trisks
        }
    })
    
    #Table of Tickers, Weights, Risk Contribution
    riskreturn <- reactive({
        if(input$submit > 0){
            trisks <- isolate(ticker_risks())
            vw <- isolate(vweights())
            rrtibble <- tibble(labels(trisks$pct_contrib_StdDev),vw,trisks$pct_contrib_StdDev)%>%
                rename('Tickers'='labels(trisks$pct_contrib_StdDev)',
                       'Weights'='vw',
                       'Risk'='trisks$pct_contrib_StdDev')
            rrtibble
        }
    })
    
    # #Gather on riskreturn
    # riskreturn2 <- reactive({
    #     if(input$submit > 0){
    #         rrtibble <- riskreturn()
    #         rrtibble2 <- gather(rrtibble, key=Attribute, value, Weights:Risk, na.rm=TRUE )
    #         rrtibble2
    #     }
    # })
    
    #Table of asset class and weight
    asset_table <- reactive({
        if(input$submit > 0){
            vt <- isolate(vtickers())
            etf_assets <- etfs%>%filter(Symbol%in%vt)%>%group_by(L2_Asset_Class)%>%
                summarise(n=n())%>%mutate(ratio=n/sum(n))%>%select(L2_Asset_Class,ratio)
            etf_assets
        }
    })
    
    #Table of asset class and risk contribution
    asset_risk <- reactive({
        req(input$tickers)
        req(input$weights)
        rrtibble <- riskreturn()
        etf_info <- etf_names()
        ar <- left_join(rrtibble, etf_info, by=c("Tickers"="Symbol"))
        ar_table <- ar%>%group_by(L2_Asset_Class)%>%summarise(risk_byasset = sum(Risk))
        ar_table
    })
    
    #ER of Annualized Rolling 12mo
    expected_return <- reactive({
        pm <- prices_monthly()
        vw <- isolate(vweights())
        annual_rolling <- na.omit(ROC(pm,n=12,type="discrete"))
        er <- round((sum(colMeans(annual_rolling) * vw) * 100), 2) 
        er
    })


    ################################################################
    output$tickerlist <- renderPrint({
        req(input$tickers)
        vtickers()
    })
    
    output$weightlist <- renderText({
        validate(need(input$tickers, "Please select tickers"),
                 need(input$weights, "Please enter weights"))
        vweights()
        ifelse(((sum(vweights())==1) & (length(vweights())==length(vtickers()))),
               "Great, weights sum to 1!",
               paste("Need to sum to 1. Number of weights needed = ",length(vtickers()),separate=""))
    })
    
    output$etf_table <- renderTable({
            etf_names()%>%select(Ticker=Symbol,Name=Name, Asset.Class=L1_Asset_Class)
    })
    
    output$time <- renderText({
        timeframe()
    })
    
    output$exp_return <- renderText({
        if(input$submit > 0){
            expected_return()
        }
    })
    
    output$ar_pie <- renderGvis({
        if(input$submit > 0){
            gvisPieChart(isolate(asset_risk()), labelvar="L2_Asset_Class", numvar="risk_byasset",
                         options = list(width =600,
                                       height = 400,
                                       slices = "{'0': {color: '#9f6d8a'}, '1': {color: '#7399c6'}}",
                                       title = "Contribution to Risk by Asset Class",
                                       titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:13}"))
        }
    })
    
    output$asset_pie <- renderGvis({
        if(input$submit > 0){
            gvisPieChart(asset_table(), labelvar="L2_Asset_Class", numvar="ratio",
                         options = list(width =800,
                                       height = 400,
                                       slices = "{'0': {color: '#9f6d8a'}, '1': {color: '#7399c6'}}",
                                       title = "Portfolio Weight by Asset Class",
                                       titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:13}"))
        }
    })
    
    output$rr_bargraph <- renderGvis({
        if(input$submit > 0){
            gvisColumnChart(riskreturn(), xvar = "Tickers", yvar = c("Weights","Risk"),
                            options = list(width=800,
                                           height=600,
                                           colors = "['#fcdb6d','#da602f']",
                                           title = "Portfolio Weights and Risk Contribution by Ticker",
                                           titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:15}"))
        }
    })
    
})
