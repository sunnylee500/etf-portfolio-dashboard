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
etfs <- read.csv(file = "/Users/YunseonLee/Downloads/etfs.csv")

shinyUI(fluidPage(
    tags$head(tags$style(
        HTML('
         #exp_return {
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
                   tags$br()
               ),
               
               tags$div(
                   tags$h3("Portfolio Inputs"),
                   tags$hr()
               ),
               
               fluidRow(
                   column(6,
                          tableOutput("etf_table")),
                   column(1),
                   column(3,
                          selectInput("tickers", "Choose Tickers:", etfs[[1]], selected=c("SPY","AGG"),multiple=TRUE, selectize=TRUE),
                          textInput("weights",label="Enter weights:", value="0.5,0.5"),
                          textOutput("weightlist"),
                          actionButton("submit","Submit"))
               ),
               

                tags$div(
                    tags$br(),
                    tags$br(),
                    tags$h3("Portfolio Dashboard"),
                    tags$hr(),
                    tags$br()
                ),
               
               fluidRow(
                   column(10,
                          fluidRow(
                              column(3),
                              column(3,
                                     tags$div(
                                         tags$h5("Average 1YR Rolling Returns (%)"),
                                         tags$hr()
                                     ),
                                     textOutput("exp_return")),
                              column(1),
                              column(4,
                                     tags$div(
                                         tags$h5("Time Frame"),
                                         tags$hr()
                                     ),
                                     textOutput("time"))
                          )
                   )
               ),
               
               tags$div(
                   tags$br(),
                   tags$br()
               ),
               
               fluidRow(
                   column(1),
                   column(9, align="center",
                          htmlOutput("rr_bargraph")
                   )
               ),
               
               tags$div(
                   tags$br()
               ),
               
               fluidRow(
                   column(10,
                          fluidRow(
                              column(4,
                                     htmlOutput("asset_pie")
                                     ),
                              column(3),
                              column(3,
                                     htmlOutput("ar_pie")
                                     )
                          )
                   )
               ),
               
               tags$div(
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
               )
        ),
        column(1)
    )
))

