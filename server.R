
library(googleVis)
library(DT)
library(shinythemes)
library(rsconnect)
library(plotly)
library(shiny)

function(input, output, session) {
  
  ####Kelompok 4 - Density Plot ####
  output$a_ArgSelect <- renderUI({
    switch (input$a_dist,
            "rnorm" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_mean", "Masukan nilai Mean :", value = 0),
              numericInput("a_sd", "Masukan nilai Standar Deviasi :", value = 1, min = 0)
            ),
            "rbeta" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_shape1", "Masukan nilai Shape 1 :", 
                           value = 1, min = 0),
              numericInput("a_shape2", "Masukan nilai Shape 2 :", 
                           value = 1, min = 0)
            ),
            "rbinom" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_size", "Masukan nilai Size :", 
                           value = 1, min = 1),
              numericInput("a_prob", "Masukan nilai prob :", 
                           value = 0.5, min = 0, max = 1, step = 0.05)
            ),
            "rcauchy" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_location", "Masukan nilai location :", 
                           value = 0),
              numericInput("a_scale", "Masukan nilai Scale :", 
                           value = 1, min = 0)
            ),
            "rchisq" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_df", "Masukan nilai df :", 
                           value = 1, min = 1)
            ),
            "rexp" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_rate", "Masukan nilai rate :", 
                           value = 1, min = 0.01, step = 1)
            ),
            "rf" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_df1", "Masukan nilai df1 :", 
                           value = 1, min = 1, step = 1),
              numericInput("a_df2", "Masukan nilai df2 :", 
                           value = 1, min = 1, step = 1)
            ),
            "rgamma" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_shape", "Masukan nilai shape :", 
                           value = 1, min = 0, step = 1),
              numericInput("a_rate", "Masukan nilai rate (1/scale) :", 
                           value = 1, min = 0.01, step = 1)
            ),
            "rgeom" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_prob", "Masukan nilai Prob :", 
                           value = 0.5, min = 0.01, max = 1, step = 0.05)
            ),
            "rhyper" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_m", "Masukan number of white balls in the urn :", 
                           value = 1, min = 0),
              numericInput("a_n", "Masukan number of black balls in the urn :", 
                           value = 1, min = 0)
            ),
            "rlnorm" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_meanlog", "Masukan nilai meanlog :", 
                           value = 0),
              numericInput("a_sdlog", "Masukan nilai sdlog :", 
                           value = 1, min = 0)
            ),
            "rlogis" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_location", "Masukan nilai location :", 
                           value = 0),
              numericInput("a_scale", "Masukan nilai scale :", 
                           value = 1)
            ),
            "rweibull" = list(
              sliderInput("a_nSize", "Masukan sample size :",
                          min = 1, max = input$a_nSample-1, value = 10),
              numericInput("a_shape", "Masukan nilai shape :", 
                           value = 1, min = 1),
              numericInput("a_scale", "Masukan nilai scale :", 
                           value = 1, min = 0)
            )
    )
    
  })
  
  output$ac_ArgSelect <- renderUI({
    switch (input$ac_dist,
            "rnorm" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_mean", "Masukan nilai Mean :", value = 0),
              numericInput("ac_sd", "Masukan nilai Standar Deviasi :", value = 1, min = 0)
            ),
            "rbeta" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_shape1", "Masukan nilai Shape 1 :", 
                           value = 1, min = 0),
              numericInput("ac_shape2", "Masukan nilai Shape 2 :", 
                           value = 1, min = 0)
            ),
            "rbinom" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_size", "Masukan nilai Size :", 
                           value = 1, min = 1),
              numericInput("ac_prob", "Masukan nilai prob :", 
                           value = 0.5, min = 0, max = 1, step = 0.05)
            ),
            "rcauchy" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_location", "Masukan nilai location :", 
                           value = 0),
              numericInput("ac_scale", "Masukan nilai Scale :", 
                           value = 1, min = 0)
            ),
            "rchisq" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_df", "Masukan nilai df :", 
                           value = 1, min = 1)
            ),
            "rexp" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_rate", "Masukan nilai rate :", 
                           value = 1, min = 0.01, step = 1)
            ),
            "rf" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_df1", "Masukan nilai df1 :", 
                           value = 1, min = 1, step = 1),
              numericInput("ac_df2", "Masukan nilai df2 :", 
                           value = 1, min = 1, step = 1)
            ),
            "rgamma" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_shape", "Masukan nilai shape :", 
                           value = 1, min = 0, step = 1),
              numericInput("ac_rate", "Masukan nilai rate (1/scale) :", 
                           value = 1, min = 0.01, step = 1)
            ),
            "rgeom" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_prob", "Masukan nilai Prob :", 
                           value = 0.5, min = 0.01, max = 1, step = 0.05)
            ),
            "rhyper" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_m", "Masukan number of white balls in the urn :", 
                           value = 1, min = 0),
              numericInput("ac_n", "Masukan number of black balls in the urn :", 
                           value = 1, min = 0)
            ),
            "rlnorm" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_meanlog", "Masukan nilai meanlog :", 
                           value = 0),
              numericInput("ac_sdlog", "Masukan nilai sdlog :", 
                           value = 1, min = 0)
            ),
            "rlogis" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_location", "Masukan nilai location :", 
                           value = 0),
              numericInput("ac_scale", "Masukan nilai scale :", 
                           value = 1)
            ),
            "rweibull" = list(
              sliderInput("ac_nSize", "Masukan sample size :",
                          min = 1, max = input$ac_nSample-1, value = 10),
              numericInput("ac_shape", "Masukan nilai shape :", 
                           value = 1, min = 1),
              numericInput("ac_scale", "Masukan nilai scale :", 
                           value = 1, min = 0)
            )
    )
    
  })
  
  
  output$a_ArgSelect_extension <- renderUI({
    if(is.null(input$a_k)) a_k<-1
    else a_k <- input$a_k
    switch (input$a_dist,
            "rhyper" = list(
              numericInput("a_k", "Masukan number of balls drawn from the urn :", 
                           value = a_k, min = 0, max = input$a_m+input$a_n)
            )
    )
    
  })
  
  output$ac_ArgSelect_extension <- renderUI({
    if(is.null(input$ac_k)) ac_k<-1
    else ac_k <- input$ac_k
    switch (input$ac_dist,
            "rhyper" = list(
              numericInput("ac_k", "Masukan number of balls drawn from the urn :", 
                           value = ac_k, min = 0, max = input$ac_m+input$ac_n)
            )
    )
    
  })
  
  a_dataset <- reactive({
    go<-input$go
    gen_sampel<-NULL
    switch (input$a_dist,
            "rnorm" = {
              gen_sampel$dat<-rnorm(input$a_nSample,input$a_mean,input$a_sd)
              gen_sampel$judul<-"Normal"
            },
            "rbeta" = {
              gen_sampel$dat<-rbeta(input$a_nSample,input$a_shape1,input$a_shape2)
              gen_sampel$judul<-"Beta"
            },
            "rbinom" = {
              gen_sampel$dat<-rbinom(input$a_nSample,input$a_size,input$a_prob)
              gen_sampel$judul<-"Binomial"
            },
            "rcauchy" = {
              gen_sampel$dat<-rcauchy(input$a_nSample,input$a_location,input$a_scale)
              gen_sampel$judul<-"Cauchy"
            },
            "rchisq" = {
              gen_sampel$dat<-rchisq(input$a_nSample,input$a_df)
              gen_sampel$judul<-"Chi Square"
            },
            "rexp" = {
              gen_sampel$dat<-rexp(input$a_nSample,input$a_rate)
              gen_sampel$judul<-"Exponensial"
            },
            "rf" = {
              gen_sampel$dat<-rf(input$a_nSample,input$a_df1,input$a_df2)
              gen_sampel$judul<-"Fisher"
            },
            "rgamma" = {
              gen_sampel$dat<-rgamma(input$a_nSample,input$a_shape,input$a_rate)
              gen_sampel$judul<-"Gamma"
            },
            "rgeom" = {
              gen_sampel$dat<-rgeom(input$a_nSample,input$a_prob)
              gen_sampel$judul<-"Geometri"
            },
            "rhyper" = {
              gen_sampel$dat<-rhyper(input$a_nSample,input$a_m,input$a_n,input$a_k)
              gen_sampel$judul<-"Hypergeometri"
            },
            "rlnorm" = {
              gen_sampel$dat<-rlnorm(input$a_nSample,input$a_meanlog,input$a_sdlog)
              gen_sampel$judul<-"Log Normal"
            },
            "rlogis" = {
              gen_sampel$dat<-rlogis(input$a_nSample,input$a_location,input$a_scale)
              gen_sampel$judul<-"Logistic"
            },
            "rweibull" = {
              gen_sampel$dat<-rweibull(input$a_nSample,input$a_shape,input$a_scale)
              gen_sampel$judul<-"Weibull"
            }
    )
    gen_sampel$sampel_terpilih <- a_generate_sampel(gen_sampel$dat,input$a_nSample,input$a_nSize);
    gen_sampel$distribusi <- input$a_dist;
    gen_sampel$cummean <- a_cummean(gen_sampel$sampel_terpilih)
    return((gen_sampel))
  })
  
  ac_dataset <- reactive({
    go<-input$go
    gen_sampel<-NULL
    switch (input$ac_dist,
            "rnorm" = {
              gen_sampel$dat<-rnorm(input$ac_nSample,input$ac_mean,input$ac_sd)
              gen_sampel$judul<-"Normal"
            },
            "rbeta" = {
              gen_sampel$dat<-rbeta(input$ac_nSample,input$ac_shape1,input$ac_shape2)
              gen_sampel$judul<-"Beta"
            },
            "rbinom" = {
              gen_sampel$dat<-rbinom(input$ac_nSample,input$ac_size,input$ac_prob)
              gen_sampel$judul<-"Binomial"
            },
            "rcauchy" = {
              gen_sampel$dat<-rcauchy(input$ac_nSample,input$ac_location,input$ac_scale)
              gen_sampel$judul<-"Cauchy"
            },
            "rchisq" = {
              gen_sampel$dat<-rchisq(input$ac_nSample,input$ac_df)
              gen_sampel$judul<-"Chi Square"
            },
            "rexp" = {
              gen_sampel$dat<-rexp(input$ac_nSample,input$ac_rate)
              gen_sampel$judul<-"Exponensial"
            },
            "rf" = {
              gen_sampel$dat<-rf(input$ac_nSample,input$ac_df1,input$ac_df2)
              gen_sampel$judul<-"Fisher"
            },
            "rgamma" = {
              gen_sampel$dat<-rgamma(input$ac_nSample,input$ac_shape,input$ac_rate)
              gen_sampel$judul<-"Gamma"
            },
            "rgeom" = {
              gen_sampel$dat<-rgeom(input$ac_nSample,input$ac_prob)
              gen_sampel$judul<-"Geometri"
            },
            "rhyper" = {
              gen_sampel$dat<-rhyper(input$ac_nSample,input$ac_m,input$ac_n,input$ac_k)
              gen_sampel$judul<-"Hypergeometri"
            },
            "rlnorm" = {
              gen_sampel$dat<-rlnorm(input$ac_nSample,input$ac_meanlog,input$ac_sdlog)
              gen_sampel$judul<-"Log Normal"
            },
            "rlogis" = {
              gen_sampel$dat<-rlogis(input$ac_nSample,input$ac_location,input$ac_scale)
              gen_sampel$judul<-"Logistic"
            },
            "rweibull" = {
              gen_sampel$dat<-rweibull(input$ac_nSample,input$ac_shape,input$ac_scale)
              gen_sampel$judul<-"Weibull"
            }
    )
    gen_sampel$sampel_terpilih <- a_generate_sampel(gen_sampel$dat,input$ac_nSample,input$ac_nSize);
    gen_sampel$distribusi <- input$ac_dist
    gen_sampel$cummean <- a_cummean(gen_sampel$sampel_terpilih)
    return((gen_sampel))
  })
  
  output$a_grafik_distribusi <- renderPlotly({
    # if(input$a_compare){
    #   dat<-data.frame(sampel_terpilih=c(a_dataset()$sampel_terpilih,ac_dataset()$sampel_terpilih))
    #   dat$distribusi<-c(rep(a_dataset()$judul,length(a_dataset()$sampel_terpilih)),
    #               rep(ac_dataset()$judul,length(ac_dataset()$judul)))
    #   p <- ggplot(dat, aes(x = sampel_terpilih)) +
    #         geom_density(aes(fill = distribusi), alpha = 0.2) + 
    #         ggtitle("Kernel Density estimates by group")
    # } 
    if(input$a_compare){
      dat1<-data.frame(a_dataset())
      dat2<-data.frame(ac_dataset())
      dat1$distribusi<-paste("1.",a_dataset()$judul)
      dat2$distribusi<-paste("2.",ac_dataset()$judul)
      dat<-rbind(dat1,dat2)
      p <- ggplot(dat, aes(sampel_terpilih, fill = distribusi)) + geom_density(alpha = 0.2)+ 
        ggtitle(paste("Distribusi",a_dataset()$judul,"vs",ac_dataset()$judul))
      
    } else {
      dat<-data.frame(a_dataset())
      p <- ggplot(dat, aes(sampel_terpilih, fill = distribusi)) + geom_density(alpha = 0.2)+ 
        ggtitle(paste("Distribusi",a_dataset()$judul))
    }
    p <- ggplotly(p);
    return(p)
  })
  
  output$a_grafik_cummean <- renderPlotly({
    if(input$a_compare){
      dat<-data.frame(Cummulative_Mean_1=a_cummean(a_dataset()$cummean))
      dat$Cummulative_Mean_2<-a_cummean(ac_dataset()$cummean)
      plot_ly(dat,y=~Cummulative_Mean_1, name = 'CumMean_1', type = 'scatter', mode = 'lines', 
              line = list(color = 'rgb(205, 12, 24)', width = 4))%>%
        add_trace(y = ~Cummulative_Mean_2, name = 'CumMean_2', line = list(color = 'rgb(22, 96, 167)', width = 4))%>%
        layout(title = paste("Grafik Cumulative Mean",a_dataset()$judul,"vs",ac_dataset()$judul))
    } else{
      dat<-data.frame(Cummulative_Mean=a_cummean(a_dataset()$cummean))
      plot_ly(dat,y=~Cummulative_Mean, type = 'scatter', mode = 'lines')%>%
        layout(title = paste("Grafik Cumulative Mean",a_dataset()$judul))
    }
  })
  
  output$ac_UI_grafik<-renderUI({
    if(input$a_nSample==input$ac_nSample) {
      list(
        plotlyOutput("a_grafik_distribusi"),
        tags$hr(),
        plotlyOutput("a_grafik_cummean")
      )
    } else{
      list(
        plotlyOutput("a_grafik_distribusi"),
        tags$hr(),
        "Grafik Cumulative Mean akan ditampilkan apabila banyaknya sampel antara kedua distribusi sama",
        HTML('<button id="ac_sameSample" type="button" 
             class="btn btn-default action-button shiny-bound-input"> 
             Samakan banyaknya sampel</button>'), 
        tags$script(paste(' document.getElementById("ac_sameSample").onclick = function() {
                          Shiny.onInputChange("ac_nSample",',input$a_nSample,'); }; '))
        
        )
    }
    })
  
  output$a_compareSide<-renderUI({
    list(
      selectInput("ac_dist", "Pilih Distribusi:", c(
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
      sliderInput("ac_nSample", "Masukan banyaknya sample :",
                  min = 10, max = 10000, value = 100),
      htmlOutput("ac_ArgSelect"),
      htmlOutput("ac_ArgSelect_extension")
    )})
  
  output$a_mainPage<- renderUI({
    if(input$a_compare){
      fluidRow(
        column(8,
               htmlOutput("ac_UI_grafik")
        ),
        column(4,
               htmlOutput("a_compareSide"),
               style="border:1px; padding:19px; border-radius:4px; background-color:#f5f5f5;")
      )}
    else
      list(
        plotlyOutput("a_grafik_distribusi"),
        tags$hr(),
        plotlyOutput("a_grafik_cummean")
      )
    
    
  })
  
  
  ####Kelompok 5 - Boostraps & Jacknife####
  
  output$contents <- DT::renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    inFile <- as.data.frame(read.csv(inFile$datapath, header = input$header))
    updateSelectInput(session, "select",
                      choices = names(inFile))
    output$summary <- renderPrint({ 
      cat(paste("<h4>Data :",input$select," | ",input$sample," sample(s)</h4>"))
      cat(paste("<br>Mean asli : ",mean(as.numeric(inFile[[input$select]]), na.rm = TRUE)))
      cat(paste("<br>Mean Bootstrap :",mean(get_bstp(data = as.numeric(inFile[[input$select]]),param = "mean", b=input$sample, na.rm = TRUE))))
      cat(paste("<br>Mean Jacknife :",mean(get_jackknife(data = as.numeric(inFile[[input$select]]),param = "mean", b=input$sample, na.rm = TRUE))))
      cat("<br>")
      cat(paste("<br>Median asli : ",median(as.numeric(inFile[[input$select]]), na.rm = TRUE)))
      cat(paste("<br>Median Bootstrap :",mean(get_bstp(data = as.numeric(inFile[[input$select]]),param = "median", b=input$sample, na.rm = TRUE))))
      cat(paste("<br>Median Jacknife :",mean(get_jackknife(data = as.numeric(inFile[[input$select]]),param = "median", b=input$sample, na.rm = TRUE))))
      cat("<br>")
      cat("<br>")
    })
    inFile
  })
  
  ####Kelompok 2 - Sampling Distribution####
  r <- 10000 # Number of replications... must be ->inf for sampling distribution!
  
  palette <- c("#a876e3",
               "#e55a76",
               "#ff7d58",
               "#44bb48",
               "#e55a76")
  
  set.seed(as.numeric(Sys.time()))
  
  # Create a reactive container for the data structures that the simulation
  # will produce. The rv$variables will be available to the sections of your
  # server code that prepare output for the UI, e.g. output$plotSample
  
  rv <- reactiveValues(sample = NULL,
                       all.sums = NULL,
                       all.means = NULL,
                       all.vars = NULL)
  
  # Note: We are giving observeEvent all the output connected to the UI actionButton.
  # We can refer to input variables from our UI as input$variablename
  observeEvent(input$takeSample,
               {
                 my.samples <- switch(input$src.dist,
                                      "E" = matrix(rexp   (input$n*input$r, input$param1             ),input$r),
                                      "N" = matrix(rnorm  (input$n*input$r, input$param1,input$param2),input$r),
                                      "U" = matrix(runif  (input$n*input$r, input$param1,input$param2),input$r),
                                      "P" = matrix(rpois  (input$n*input$r, input$param1)             ,input$r),
                                      "C" = matrix(rcauchy(input$n*input$r, input$param1,input$param2),input$r),
                                      "B" = matrix(rbinom (input$n*input$r, input$param1,input$param2),input$r),
                                      "G" = matrix(rgamma (input$n*input$r, input$param1,input$param2),input$r),
                                      "X" = matrix(rchisq (input$n*input$r, input$param1)             ,input$r),
                                      "T" = matrix(rt     (input$n*input$r, input$param1)             ,input$r))
                 
                 # It was very important to make sure that rv contained numeric values for plotting:
                 rv$sample <- as.numeric(my.samples[1,])
                 rv$all.sums <- as.numeric(apply(my.samples,1,sum))
                 rv$all.means <- as.numeric(apply(my.samples,1,mean))
                 rv$all.vars <- as.numeric(apply(my.samples,1,var))
               })
  
  output$plotSample <- renderPlot({
    # Plot yang terbentuk ketika pengguna menekan tombol "Buat Plot"
    if (input$takeSample) {
      # Create a 2x2 plot area & leave a big space (5) at the top for title
      par(mfrow=c(2,2), oma=c(0,0,5,0))
      hist(rv$sample,    main="Distribution of One Sample",            ylab="Frequency",col=palette[1])    
      hist(rv$all.sums,  main="Sampling Distribution of the Sum",      ylab="Frequency",col=palette[2])
      hist(rv$all.means, main="Sampling Distribution of the Mean",     ylab="Frequency",col=palette[3])
      hist(rv$all.vars,  main="Sampling Distribution of the Variance", ylab="Frequency",col=palette[4])
      
      mtext("Hasil Simulasi", outer=TRUE, cex=3)
    }
  }, height=660, width=900) # end plotSample
  
  observeEvent(input$takeSample2,
               {
                 my.samples <- switch(input$src.dist,
                                      "E" = matrix(rexp   (input$n*input$r2, input$param1             ),input$r2),
                                      "N" = matrix(rnorm  (input$n*input$r2, input$param1,input$param2),input$r2),
                                      "U" = matrix(runif  (input$n*input$r2, input$param1,input$param2),input$r2),
                                      "P" = matrix(rpois  (input$n*input$r2, input$param1)             ,input$r2),
                                      "C" = matrix(rcauchy(input$n*input$r2, input$param1,input$param2),input$r2),
                                      "B" = matrix(rbinom (input$n*input$r2, input$param1,input$param2),input$r2),
                                      "G" = matrix(rgamma (input$n*input$r2, input$param1,input$param2),input$r2),
                                      "X" = matrix(rchisq (input$n*input$r2, input$param1)             ,input$r2),
                                      "T" = matrix(rt     (input$n*input$r, input$param1)             ,input$r2))
                 
                 # It was very important to make sure that rv contained numeric values for plotting:
                 rv$sample <- as.numeric(my.samples[1,])
                 rv$all.sums <- as.numeric(apply(my.samples,1,sum))
                 rv$all.means <- as.numeric(apply(my.samples,1,mean))
                 rv$all.vars <- as.numeric(apply(my.samples,1,var))
               })
  
  output$plotSample2 <- renderPlot({
    # Plot yang terbentuk ketika pengguna menekan tombol "Buat Plot"
    if (input$takeSample) {
      # Create a 2x2 plot area & leave a big space (5) at the top for title
      par(mfrow=c(2,2), oma=c(0,0,5,0))
      plot(density(rv$sample))
      plot(density(rv$all.sums))
      plot(density(rv$all.means))
      plot(density(rv$all.vars))
      mtext("Hasil Simulasi", outer=TRUE, cex=3)
    }
  }, height=660, width=900)
  
  ####Kelompok 2 - CLT####
  
  
  ####Kelompok 3 - Confidence Interval#### 
  #data default
  distribusi<-"Normal"
  data<-rnorm(1000,0,1)
  #sampel default
  ss<-100
  nos<-100
  sampelData<-matrix(NA,ncol = ss,nrow = nos)
  meanMatrix<-matrix(NA,ncol = 1,nrow = nos)
  sdMatrix<-matrix(NA,ncol = 1,nrow = nos)
  ciMatrix<-matrix(NA,ncol = 2,nrow = nos)
  alfa<-0.05
  
  output$Parameter<-renderUI({
    if(input$PilihDistribusi=="Normal"){
      distribusi<<-"Normal"
      tagList(
        tags$h4("Generate Data Normal"),
        sliderInput(inputId = "mean",label = "Mean",min = -100,max = 100,value = 0,step = 0.1),
        sliderInput(inputId = "sd",label = "Standard Deviasi",min = -100,max=100,value=1,step=0.01)
        
      )
    }else{
      if(input$PilihDistribusi=="Chi-Square"){
        distribusi<<-"Chi-Square"
        tagList(
          tags$h4("Generate Data Chi-Square"),
          sliderInput(inputId = "df",label = "Derajat Bebas",min = 1,max = 200,step = 1,value = 10),
          tags$hr(),
          tags$br(),
          tags$h4("Generate Data Sampel")
        )
        
      }  
    }
  })
  #generate populasi
  generatePopulasi<-function(){
    if(input$PilihDistribusi=="Normal"){
      data<<-rnorm(input$NumberOfData,mean = input$mean,sd = input$sd)
    }else{
      if(input$PilihDistribusi=="Chi-Square"){
        data<<-rchisq(n = input$NumberOfData,df = input$df)
      }
    }
  }
  
  output$plotDataPopulasi<-renderPlot({
    generatePopulasi()
    hist(data,probability = TRUE,col=4,main = "Histogram Data Populasi")
    abline(v=mean(data),col=2,lwd=2)
  })
  
  
  
  #generate sampel
  generateSampel<-function(sampelSize,numberOfSampel,Alfa){
    ss<<-sampelSize
    nos<<-numberOfSampel
    sampelData<<-matrix(NA,ncol = ss,nrow = nos)
    meanMatrix<<-matrix(NA,ncol = 1,nrow = nos)
    alfa<<-Alfa
    ciMatrix<<-matrix(NA,ncol = 2,nrow = nos)
    for(i in seq(nos)){
      sampelData[i,]<<-sample(x = data,size = ss,replace = TRUE)
      meanMatrix[i,1]<<-mean(sampelData[i,])
      sdMatrix[i,1]<<-sd(sampelData[i,])
      ciMatrix[i,1]<<-meanMatrix[i,1]+qnorm((alfa/2),0,1)*sdMatrix[i,1]/sqrt(ss)
      ciMatrix[i,2]<<-meanMatrix[i,1]+qnorm(1-(alfa/2),0,1)*sdMatrix[i,1]/sqrt(ss)
    }
  }
  #generate CI
  generateCI<-function(alfa){
    alfa<<-input$alfa
    ciMatrix<<-matrix(NA,ncol = 2,nrow = nos)
    for(i in seq(nos)){
      ciMatrix[i,1]<<-meanMatrix[i,1]+qnorm(alfa/2,0,1)*sdMatrix[i,1]/sqrt(ss)
      ciMatrix[i,2]<<-meanMatrix[i,1]+qnorm(1-alfa/2,0,1)*sdMatrix[i,1]/sqrt(ss)
    }
  }
  output$CI<-renderUI({
    tagList(
      tags$hr(),
      tags$br(),
      tags$h4("Generate Confidence Interval"),
      sliderInput(inputId = "alfa",label = "Alfa",min = 0.01,max = 1,step = 0.01,value = 0.05)
    )
  })
  
  output$sampel<-renderPrint({
    if(distribusi!=input$PilihDistribusi){
      generatePopulasi()
    }
    generateSampel(sampelSize = input$SampelSize,numberOfSampel = input$NumberOfSampel,Alfa = input$alfa)
    summary(t(sampelData))
  })
  output$plotCI<-renderPlot({
    if(is.na(ciMatrix[1,1])){
      generateSampel(sampelSize = input$SampelSize,numberOfSampel = input$NumberOfSampel,Alfa = input$alfa)
    }
    if((ss!=input$SampelSize)||(nos!=input$NumberOfSampel)){
      generateSampel(sampelSize = input$SampelSize,numberOfSampel = input$NumberOfSampel,Alfa = input$alfa)
    }
    generateCI(input$alfa)
    xkor<-c(ciMatrix[,1],ciMatrix[,2])
    ykor<-c(seq(1,nos,by = 1),seq(1,nos,by = 1))
    miuData<-mean(sampelData)
    plot(xkor,ykor,xlab = "Rata-rata Sampel",ylab = "Data Ke",main = "Confidence Interval Nilai Rata-Rata Sampel")
    #create segment
    
    for(i in seq(nos)){
      #jika data berada miu berada diluar interval
      if((miuData>xkor[i+nos])||(miuData<xkor[i])){
        #segmen berwarna merah
        segments(xkor[i], ykor[i], xkor[i+nos], ykor[i+nos], col= 'red')  
      }else{
        #segment berwarna hitam
        segments(xkor[i], ykor[i], xkor[i+nos], ykor[i+nos], col= 'blue')  
      }
      
    }
    abline(v = miuData)
  })
  
  
  ####Kelompok 1 -  MOnte Carlo & MCMC####
  #update the max value for the number of successes when using arrows to change
  observe({
    val <- input$n
    valy <- input$y
    updateNumericInput(session, "y", min = 0, max = val, step = 1)
  })
  
  #generate chain if settings change
  simDraws<-reactive({
    input$y
    input$thetastart
    input$thetaproposalsd
    input$n
    
    #call sampler    
    data<-thetasampler(y=input$y,n=input$n,niter=niter,thetastartval=input$thetastart,thetasd=input$thetaproposalsd)
    data
  })
  
  #create posterior plot with all the overlays
  output$posteriorPlot<-renderPlot({
    
    #get info   	
    y<-input$y
    n<-input$n
    input$thetastart
    input$thetaproposalsd
    step<-input$step
    
    #grab data
    draws<-simDraws()
    
    #true posterior first
    x=seq(0,1,length=1000)
    plot(x,dbeta(x,shape1=y+1, shape2=n-y+1),type="l",ylab="posterior",xlab="theta",main="Plot of True Posterior",lwd=3)
    
    #histogram of data up to this step
    h<-hist(draws$theta[1:step],plot=FALSE,breaks=seq(from=0,to=1,by=0.05))
    
    #rescale max to have max of the posterior
    max<-max(h$counts)
    h$counts<-h$counts*max(dbeta(x,shape1=y+1, shape2=n-y+1))/max
    plot(h,add=TRUE,col=rgb(0.3,0.1,0.3,0.1))
    
    #add in value for step "step"
    if(step==1){
      #add in intial value
      abline(v=draws$theta[step],col="Blue",lwd=3)
      mtext(side=3,at=draws$theta[step],text="Theta1")
    } else {
      #add previous theta
      abline(v=draws$theta[step-1],col="green",lwd=3)
      mtext(side=1,at=draws$theta[step-1],text=paste("Theta",step-1,sep=""))
      
      #overlay jumping distribution
      lines(x,max(dbeta(x,shape1=y+1, shape2=n-y+1))*dnorm(x,mean=draws$theta[step-1],sd=input$thetaproposalsd)/max(dnorm(x,mean=draws$theta[step-1],sd=input$thetaproposalsd)),col="Purple",lwd=3)
      
      #add in jumped value
      abline(v=draws$theta[step-1]+draws$jump[step],col="Blue",lwd=3)
      mtext(side=3,at=draws$theta[step-1]+draws$jump[step],text="Theta*")
    }
    
    legend(x="topleft",legend=c("True Posterior","Jumping Distribution","Previous Theta","Candidate Theta"),col=c("Black","Purple","Green","Blue"),pch=15,lwd=3)
  })
  
  output$drawTable<-renderTable({
    
    #obtain inputs
    step<-input$step
    input$y
    start<-input$thetastart
    input$thetaproposalsd
    input$n
    
    #grab data
    draws<-simDraws()
    
    if(step==1){
      #Get draws
      out<-data.frame(Iter=step,Prev=NA,Jump=draws$jump[1:step],New=start,r=draws$r[1:step],runif=draws$runiforms[1:step],accept=draws$accept[1:step])
      colnames(out)<-c("Iteration","Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
      out
    } else {
      out<-data.frame(Iter=1:step,Prev=c(NA,draws$theta[1:(step-1)]),Jump=draws$jump[1:step],New=c(start,draws$theta[1:(step-1)]+draws$jump[2:step]),r=draws$r[1:step],runif=draws$runiforms[1:step],accept=draws$accept[1:step])
      colnames(out)<-c("Iteration","Previous Theta","Jump","New Theta","r Ratio","Random Uniform","Accept Theta?")
      tail(out,n=5)
    }
  })    
  
}