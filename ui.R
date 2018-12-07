#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# =================================================
# : Coursera.org
# : Data Science Specialization - Capstone Project
# : December 2018
# :
# : Shiny Application: Predicting Next Word
# :
# : Author  - JeremiShane
# : twitter - @jeremi_
# =================================================

library(shiny)
source("skbt_functions.R")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

        # Application title
        titlePanel("Data Science Capstone Project - Next Word Prediction App"),

        # User interface controls1
        sidebarLayout(
                sidebarPanel(
                        p("Input a word or text and press <ENTER> or click <Predict> to see the next word(s) suggestions:"),
                        textInput(inputId="text", label = ""),
                        submitButton("Predict"),
                        HTML('<script type="text/javascript">
                                document.getElementById("text").focus();
                                </script>')
                        ),
                mainPanel(
                        tabsetPanel(

                                tabPanel("Result",
                                        conditionalPanel(condition = "input.text != ''",
                                                verbatimTextOutput("text"),
                                                verbatimTextOutput("cleaned"), verbatimTextOutput("msg"),
                                                selectInput("predicts","Word predictions:",choices=c(""))
                                        )
                                ),
                               tabPanel("Documentation", htmlOutput("help"),
                                       tags$div(id="help",
                                               HTML("<iframe id='ifrHelp' src='help.html' height='550' width='650'></iframe>")
                                        )
                                )
                        )
                )
        )

       )
)
