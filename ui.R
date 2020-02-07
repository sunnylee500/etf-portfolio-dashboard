#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

shinyUI(fluidPage(
    tags$head(tags$style(
        HTML('
         #exp_return {
            font-family:Verdana;
            font-style: bold;
            font-size: 150%;
            text-align:center
         }
        
        #exp_return_new {
            font-family:Verdana;
            font-style: bold;
            font-size: 150%;
            text-align:center
        } 
         
        #time {
            font-family:Verdana;
            font-style: bold;
            font-size: 150%;
            text-align:center
        }
             
             ')
    )),
    fluidRow(
        column(1),
        column(10,
               tags$div(
                   tags$h1("Portfolio Risk and Return"),
                   tags$br()),
            tabsetPanel(type = "tabs",
                        
            tabPanel("Portfolio Inputs", 
               tags$div(
                   tags$h3("Current Portfolio"),
                   tags$hr()),
               
               fluidRow(
                   column(6,
                          tableOutput("etf_table")),
                   column(1),
                   column(3,
                          selectInput("tickers", "Current Tickers:", etfs[[1]], selected=c("SPY","AGG"),multiple=TRUE, selectize=TRUE),
                          textInput("weights",label="Current Weights:", value="0.8,0.2"),
                          textOutput("weightlist"))),
               
               tags$div(
                   tags$h3("Comparative Portfolio"),
                   tags$hr()),
               
               fluidRow(
                   column(6,
                          tableOutput("etf_table_new")),
                   column(1),
                   column(3,
                          selectInput("tickers_new", "Comparative Tickers:", etfs[[1]], selected=c("SPY","AGG"),multiple=TRUE, selectize=TRUE),
                          textInput("weights_new",label="Comparative Weights:", value="0.6,0.4"),
                          textOutput("weightlist_new"),
                          actionButton("submit","Submit")))
            ),
            
            tabPanel("Portfolio Dashboard", 
                tags$div(
                    tags$br(),
                    tags$br(),
                    tags$h3("Portfolio Dashboard"),
                    tags$hr(),
                    tags$br()
                ),
                
                fluidRow(align = "center",
                         tags$h5("Common Time Frame"),
                         tags$hr(width="30%"),
                         textOutput("time"),
                         tags$br()
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                                tags$div(align="center",
                                         tags$h3("Current Portfolio"),
                                         tags$hr(),
                                         tags$br()),
                                tags$div(align="center",
                                         tags$h3("Comparative Portfolio"),
                                         tags$hr(),
                                         tags$br())
                    )
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                           tags$div(align="center",
                               tags$h5("Average 1YR Rolling Returns (%)"),
                               tags$hr(width="30%")),
                           tags$div(align="center",
                               tags$h5("Average 1YR Rolling Returns (%)"),
                               tags$hr(width="30%"))
                           )
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                           textOutput("exp_return"),
                           textOutput("exp_return_new")
                    )
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                           htmlOutput("rr_bargraph"),
                           htmlOutput("rr_bargraph_new"))
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                           htmlOutput("asset_pie"),
                           htmlOutput("asset_pie_new"))
                ),
                
                fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                           htmlOutput("ar_pie"),
                           htmlOutput("ar_pie_new"))
                )),
            
            tabPanel("Top 100 ETFs",
                fluidRow(
                    tags$div(
                        tags$h3("Largest ETFs: Top 100 ETFs By Assets"),
                        tags$br())
                ),
                fluidRow(
                    tableOutput("top100_etf")
                ))
            )),
                
        column(1)
    )
))

