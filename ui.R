
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
  tabPanel("CLT",
           ####Analisis Seasonal####
           #pageWithSidebar(
  ####Kelompok 2 - Ilustrasi Histogram Hasil Simulasi####
           sidebarPanel(
             
             sliderInput(inputId="n","Ukuran sample (n)",value=30,min=5,max=100,step=2),
             sliderInput(inputId="r", "Jumlah pengulangan(r)",value=1000, min=1000, max=10000, step=1000),
             radioButtons("src.dist", "Tipe Distribusi:",
                          c("Exponential: Param1 = Mean,     Param2 = NOT USED" = "E",
                            "Normal:      Param1 = Mean,     Param2 = SD" = "N",
                            "Uniform:     Param1 = Min,      Param2 = Max" = "U",
                            "Poisson:     Param1 = Lambda,   Param2 = NOT USED" = "P",
                            "Cauchy:      Param1 = Location, Param2 = Scale" = "C",
                            "Binomial:    Param1 = Size,     Param2 = Success prob" = "B",
                            "Gamma:       Param1 = Shape,    Param2 = Scale" = "G",
                            "Chi Square:  Param1 = DF,       Param2 = ncp" = "X",
                            "Student t:   Param1 = DF,       Param2 = NOT USED" = "T")),
             
             numericInput("param1","Parameter 1:",10),
             numericInput("param2","Parameter 2:",2),
             actionButton("takeSample","Buat Plot")
           ),
           mainPanel(
             tabsetPanel(id = "CLT",
               tabPanel("Ilustrasi Histogram Hasil Simulasi",
                 plotOutput("plotSample")
               ),
               tabPanel("Ilustrasi Density Plot Hasil Simulasi",
                 plotOutput("plotSample2")
               )
             )#endtabset panel
           ) # end mainPanel
        #) #end sidebar
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
           fluidPage(headerPanel(
             HTML('Markov Chain Monte Carlo'
             ), "Markov Chain Monte Carlo"
           ),
           fluidRow(
             column(4,
                    wellPanel(
                      h4("Binomial Distribution (Likelihood) set-up"),
                      numericInput(inputId="n",label="Percobaan",value=20,step=1,min=1,max=5000),
                      numericInput(inputId="y",label="Sukses",value=13,step=1,max=20,min=0),
                      h4("Jumping Distribution is a Normal Distribution"),
                      numericInput(inputId="thetaproposalsd",label="Standar Deviasi Jumping Distribution",value=0.05,min=0.000001,max=0.2,step=0.01),
                      numericInput(inputId="thetastart",label="Nilai Awal Theta",value=0.5,step=0.1,min=0,max=1),
                      sliderInput(inputId="step", label=h4("Step"), 
                                  min = 1, max = 10000, value = 1, step = 1,
                                  animate=list(TRUE, interval=100,loop=FALSE)),
                      br()
                    )
             ),
             column(8,
                    tabsetPanel(
                      tabPanel("Plot",plotOutput("posteriorPlot")),
                      tabPanel("Table",tableOutput("drawTable"))
                    )
             )
           )
           )
  ),
  
  theme = shinytheme('flatly')
)

