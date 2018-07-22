
library(googleVis)
library(DT)
library(shinythemes)
library(rsconnect)
library(plotly)
library(shiny)
library(ggplot2)

navbarPage(
  "Teknik Analisis dan Simulasi Data",
  ####Kelompok 4 - Density Plot ####
  tabPanel("Density Plot",
           # Application title
           titlePanel("Simulasi Distribusi Peluang"),
           
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               
               selectInput("a_dist", "Pilih Distribusi:", c(
                 "Normal"="rnorm",
                 "Beta"="rbeta",
                 "Binomial"="rbinom",
                 "Cauchy"="rcauchy",
                 "Chi Square"="rchisq",
                 "Exponensial"="rexp",
                 "Fisher"="rf",
                 "Gamma"="rgamma",
                 "Geometri"="rgeom",
                 "Hypergeometri"="rhyper",
                 "Log Normal"="rlnorm",
                 "Logistik"="rlogis",
                 "Weibull"="rweibull"
               )),
               sliderInput("a_nSample", "Masukan banyaknya sample :",
                           min = 10, max = 10000, value = 100),
               htmlOutput("a_ArgSelect"),
               htmlOutput("a_ArgSelect_extension"),
               checkboxInput("a_compare", "Bandingkan dengan distribusi lain", FALSE),
               actionButton("go", "Generate ulang!")
               
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               htmlOutput("a_mainPage")
             )
           )
  ),
  ####Kelompok 5 - Boostraps & Jacknife####
  tabPanel("Bootstrap & jacknife",
           # Application title
           titlePanel("Bootstrap and Jacknife"),
           
           # Sidebar with a slider input for number of bins 
           sidebarLayout(
             sidebarPanel(
               fileInput("file1", "Choose CSV File",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
               ),
               checkboxInput("header", "Data with columns name?", TRUE),
               selectInput("select", "Select columns to simulation", c()),
               sliderInput("sample", "Number of resamples:",
                           min = 0, max = 1000,
                           value = 10)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               DT::dataTableOutput("contents"),
               htmlOutput("summary")
             )
           )
  ),
  ####Kelompok 2 - Sampling Distribution####
  tabPanel("Sampling Distribution",
           "Kelompok 2"
  ),
  ####Kelompok 2 - CLT####
  tabPanel("CLT",
           "Kelompok 2"
  ),
  ####Kelompok 3 - Confidence Interval#### 
  tabPanel("Confidence Interval",
           sidebarPanel(
             selectInput(inputId="PilihDistribusi", label = "Distribusi Data", choices = c("Normal", "Chi-Square"),selected = "Normal"),
             sliderInput(inputId="NumberOfData",label = "Banyaknya Data",min = 100,max = 100000,value = 1000),
             tags$br(),
             tags$h4("Generate Data Sampel"),
             sliderInput(inputId="NumberOfSampel",label = "Number Of Sampel", min = 10, max = 100,value = 100,step = 1),
             sliderInput(inputId="SampelSize",label = "Sampel Size", min = 10,max = 1000,value = 100),
             tags$br(),
             uiOutput(outputId="CI"),
             textOutput(outputId="text"),
             tags$br(),
             uiOutput(outputId="Parameter")
           ),
           
           mainPanel(
             navbarPage("Plotts",
                        tabPanel("Plot Data Populasi",
                                 plotOutput(outputId="plotCI"),
                                 plotOutput(outputId="plotDataPopulasi")
                        ),
                        tabPanel("Summary Data Sampel",
                                 verbatimTextOutput(outputId="sampel")
                        )
             )
           )
  ),
  ####Kelompok 1 -  MOnte Carlo & MCMC####
  tabPanel("MOnte Carlo & MCMC",
             "Kelompok 1"
  ),
  
  theme = shinytheme('flatly')
)

