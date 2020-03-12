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
etfs <- read.csv(file = "etfs.csv")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    vtickers <- reactive({
        vt <- as.character(unlist(strsplit(input$tickers,",")))
        vt
    })
    
    vtickers_new <- reactive({
        vt_new <- as.character(unlist(strsplit(input$tickers_new,",")))
        vt_new
    })
    
    vweights <- reactive({
        vw <- as.numeric(unlist(strsplit(input$weights,",")))
        vw
    })
    
    vweights_new <- reactive({
        vw_new <- as.numeric(unlist(strsplit(input$weights_new,",")))
        vw_new
    })
    
    #Table of ETF info: ticker, name, L1, L2
    etf_names <- reactive({
            validate(need(input$tickers, "Please select tickers"))
            vt <- vtickers()
            etf_info <- etfs%>%filter(Symbol%in%vt)
            etf_info
    })
    
    etf_names_new <- reactive({
        validate(need(input$tickers_new, "Please select tickers"))
        vt <- vtickers_new()
        etf_info <- etfs%>%filter(Symbol%in%vt)
        etf_info
    })
    
    ######################### 
    #Table of monthly prices given the tickers 
    prices_monthly <- reactive({
        if(input$submit > 0){
            vt <- vtickers()
            vt_new <- vtickers_new()
            both <- unique(c(vt, vt_new))
            
            pm <- getSymbols(both, src='yahoo')%>%
                map(~Ad(get(.)))%>%
                reduce(merge)%>%
                na.omit()%>%
                to.monthly(indexAt='last',OHLC=FALSE)%>%
                `colnames<-`(both)
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
            vw <- isolate(vweights())
            vt <- isolate(vtickers())
            treturns <- isolate(ticker_returns())
            treturns1 <- treturns[,vt]
            trisks <- StdDev(treturns1, weights=vw, portfolio_method="component")
            trisks
        }
    })
    
    ticker_risks_new <- reactive({
        if(input$submit > 0){
            vw <- isolate(vweights_new())
            vt <- isolate(vtickers_new())
            treturns <- isolate(ticker_returns())
            treturns1 <- treturns[,vt]
            trisks <- StdDev(treturns1, weights=vw, portfolio_method="component")
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
    
    riskreturn_new <- reactive({
        if(input$submit > 0){
            trisks <- isolate(ticker_risks_new())
            vw <- isolate(vweights_new())
            rrtibble <- tibble(labels(trisks$pct_contrib_StdDev),vw,trisks$pct_contrib_StdDev)%>%
                rename('Tickers'='labels(trisks$pct_contrib_StdDev)',
                       'Weights'='vw',
                       'Risk'='trisks$pct_contrib_StdDev')
            rrtibble
        }
    })
    
    
    #Joined tables of return risk and etf info 
    joined_rr_etfinfo <- reactive({
        req(input$tickers)
        req(input$weights)
        rrtibble <- riskreturn()
        etf_info <- etf_names()
        ar <- left_join(rrtibble, etf_info, by=c("Tickers"="Symbol"))
        ar
    })
    
    joined_rr_etfinfo_new <- reactive({
        req(input$tickers_new)
        req(input$weights_new)
        rrtibble <- riskreturn_new()
        etf_info <- etf_names_new()
        ar <- left_join(rrtibble, etf_info, by=c("Tickers"="Symbol"))
        ar
    })
    
    #Table of asset class and weight
    asset_table <- reactive({
        if(input$submit > 0){
            ar <- joined_rr_etfinfo()
            etf_assets <- ar%>%group_by(L2_Asset_Class)%>%summarise(totalweight=sum(Weights))%>%
                select(L2_Asset_Class,totalweight)
            etf_assets
        }
    })
    
    asset_table_new <- reactive({
        if(input$submit > 0){
            ar <- joined_rr_etfinfo_new()
            etf_assets <- ar%>%group_by(L2_Asset_Class)%>%summarise(totalweight=sum(Weights))%>%
                select(L2_Asset_Class,totalweight)
            etf_assets
        }
    })
    
    #Table of asset class and risk contribution
    asset_risk <- reactive({
        ar <- joined_rr_etfinfo()
        ar_table <- ar%>%group_by(L2_Asset_Class)%>%summarise(risk_byasset = sum(Risk))
        ar_table
    })
    
    asset_risk_new <- reactive({
        ar <- joined_rr_etfinfo_new()
        ar_table <- ar%>%group_by(L2_Asset_Class)%>%summarise(risk_byasset = sum(Risk))
        ar_table
    })
    
    #ER of Annualized Rolling 12mo
    expected_return <- reactive({
        vt <- vtickers()
        vw <- isolate(vweights())
        pm <- prices_monthly()
        pm1 <- pm[,vt]
        annual_rolling <- na.omit(ROC(pm1,n=12,type="discrete"))
        er <- round((sum(colMeans(annual_rolling) * vw) * 100), 2) 
        er
    })
    
    expected_return_new <- reactive({
        vt <- vtickers_new()
        vw <- isolate(vweights_new())
        pm <- prices_monthly()
        pm1 <- pm[,vt]
        annual_rolling <- na.omit(ROC(pm1,n=12,type="discrete"))
        er <- round((sum(colMeans(annual_rolling) * vw) * 100), 2) 
        er
    })
    
    ################################################################

    
    output$weightlist <- renderText({
        validate(need(input$tickers, "Please select tickers"),
                 need(input$weights, "Please enter weights"))
        vweights()
        ifelse(((sum(vweights())==1) & (length(vweights())==length(vtickers()))),
               "Great, weights sum to 1!",
               paste("Need to sum to 1. Number of weights needed = ",length(vtickers()),separate=""))
    })
    
    output$weightlist_new <- renderText({
        validate(need(input$tickers_new, "Please select tickers"),
                 need(input$weights_new, "Please enter weights"))
        vweights_new()
        ifelse(((sum(vweights_new())==1) & (length(vweights_new())==length(vtickers_new()))),
               "Great, weights sum to 1!",
               paste("Need to sum to 1. Number of weights needed = ",length(vtickers_new()),separate=""))
    })
    
    output$etf_table <- renderTable({
            etf_names()%>%select(Ticker=Symbol,Name=Name, Asset.Class=L1_Asset_Class)
    })
    
    output$etf_table_new <- renderTable({
        etf_names_new()%>%select(Ticker=Symbol,Name=Name, Asset.Class=L1_Asset_Class)
    })
    
    output$time <- renderText({
        timeframe()
    })
    
    output$exp_return <- renderText({
        if(input$submit > 0){
            expected_return()
        }
    })
    
    output$exp_return_new <- renderText({
        if(input$submit > 0){
            expected_return_new()
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
                                                            fontSize:13}",
                                       legend = "{position: 'left'}"))
        }
    })
    
    output$ar_pie_new <- renderGvis({
        if(input$submit > 0){
            gvisPieChart(isolate(asset_risk_new()), labelvar="L2_Asset_Class", numvar="risk_byasset",
                         options = list(width =600,
                                        height = 400,
                                        slices = "{'0': {color: '#9f6d8a'}, '1': {color: '#7399c6'}}",
                                        title = "Contribution to Risk by Asset Class",
                                        titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:13}",
                                        legend = "{position: 'left'}"))
        }
    })
    
    output$asset_pie <- renderGvis({
        if(input$submit > 0){
            gvisPieChart(asset_table(), labelvar="L2_Asset_Class", numvar="totalweight",
                         options = list(width =600,
                                       height = 400,
                                       slices = "{'0': {color: '#9f6d8a'}, '1': {color: '#7399c6'}}",
                                       title = "Portfolio Weight by Asset Class",
                                       titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:13}",
                                       legend = "{position: 'left'}"))
        }
    })
    
    output$asset_pie_new <- renderGvis({
        if(input$submit > 0){
            gvisPieChart(asset_table_new(), labelvar="L2_Asset_Class", numvar="totalweight",
                         options = list(width =600,
                                        height = 400,
                                        slices = "{'0': {color: '#9f6d8a'}, '1': {color: '#7399c6'}}",
                                        title = "Portfolio Weight by Asset Class",
                                        titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:13}",
                                        legend = "{position: 'left'}"))
        }
    })
    
    output$rr_bargraph <- renderGvis({
        if(input$submit > 0){
            gvisColumnChart(riskreturn(), xvar = "Tickers", yvar = c("Weights","Risk"),
                            options = list(width=600,
                                           height=500,
                                           colors = "['#fcdb6d','#da602f']",
                                           title = "Weight and Risk Contribution by Ticker",
                                           titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:15}",
                                           legend = "{position: 'bottom'}"))
        }
    })
    
    output$rr_bargraph_new <- renderGvis({
        if(input$submit > 0){
            gvisColumnChart(riskreturn_new(), xvar = "Tickers", yvar = c("Weights","Risk"),
                            options = list(width=600,
                                           height=500,
                                           colors = "['#fcdb6d','#da602f']",
                                           title = "Weight and Risk Contribution by Ticker",
                                           titleTextStyle="{color: '#3d4854',
                                                            fontName:'Verdana',
                                                            fontSize:15}",
                                           legend = "{position: 'bottom'}"))
        }
    })
    
    output$top100_etf <- renderTable({
        etfs%>%top_n(100)%>%mutate(Rank=c(1:100))
    })
    
})
