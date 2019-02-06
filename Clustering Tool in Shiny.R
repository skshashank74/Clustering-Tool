library(DT)
library(shiny)
library(shinydashboard)
#devtools::install_github('hadley/ggplot2')
library(ggplot2)
library(corrplot)
#install.packages("sacles")
library(scales)
library(dplyr)
library(rhandsontable)
library(caret)
library(lattice)
library(matrixStats)
library(plyr)
library(plotly)
library(reshape2)
library(RJDBC)
library(rJava)
library(RWeka)
#require(redshift)
#devtools::install_github("pingles/redshift-r")
library (RPostgreSQL)
library(DBI)
library(devtools)
library(Cairo)

#install.packages("colorspace")
rm(ui)
rm(server)

ui <- dashboardPage( title = "Hello Shiny", skin = "blue",
                     dashboardHeader( title = "Clustering Tool"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Data Upload", tabName = "first", icon = icon("th")),
                         menuItem("EDA", tabName = "second", icon = icon("dashboard")),
                         menuItem("Variable Treatment", tabName = "third", icon = icon("cogs")),
                         menuItem("Clsuter Creation", tabName = "fourth", icon = icon("braille")),
                         menuItem("Cluster Visualisation", tabName = "fifth", icon = icon("dashboard"))
                       )
                     ),
                     dashboardBody(
                       tabItems(
                         tabItem(tabName = "first", h2("IMPORT DATA"),
                                 fluidRow( 
                                   box( width = 12,
                                        radioButtons("option", "Choose Data Source Option", choices = c("Excel","Database"), inline = T)
                                   )
                                 ),
                                 fluidRow(
                                   box(title = "Select From Your System", height = 300, width = 4,
                                       fileInput("file", "Upload File"),
                                       h5("Maximum size of the file 1 GB"),
                                       radioButtons("sep", "Separators", choices = c(Comma = ",", Period = ".", Tilde = "~"), inline = T),
                                       checkboxInput("header", "Header?")
                                   )
                                   ,
                                   box(title = "Select From Data Base", height = 300, width = 8,
                                       div(style="height: 40px;",textInput("USERNAME", NULL, placeholder = "user")),
                                       div(style="height: 40px;",textInput("PASSWORD", NULL, placeholder = "password")),
                                       textAreaInput("TABLENAME", NULL, placeholder = "Write your Query", height = '120px'),
                                       actionButton("run", "Run")
                                   )
                                 ),
                                 fluidRow(
                                   tabBox( side = "left",  width = 12, height = 600,
                                           tabPanel(title = "Upload Table", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    dataTableOutput("table_primary")
                                           ),
                                           tabPanel(title = "Table for Clustering", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    uiOutput("col00"), actionButton(inputId = "update00", "Update"),
                                                    DT::dataTableOutput("table00", height = 200, width = "auto" )
                                                    
                                           ),
                                           tabPanel(title = "Table for Descriptors", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    uiOutput("col01"), actionButton(inputId = "update01", "Update"),
                                                    DT::dataTableOutput("table01", height = 200, width = "auto" )
                                           ),
                                           h3(" Please dont select same varibales for both Clustering and Descriptors")
                                   )  
                                 )
                         ),
                         tabItem(tabName = "second", h2("Exploratory Data Analysis"),
                                 fluidRow(
                                   box(width = 12, title = "Missing Data Stats",
                                       solidHeader = T,verbatimTextOutput("Count_of_missing_value"),
                                       verbatimTextOutput("na_in_each_col")),
                                   box(solidHeader = T, background = "black", width = 12, uiOutput("col1")),
                                   box( width = 12,
                                        radioButtons("missing_value", "Missing Value Treatment", 
                                                     choices = c("Proceed as it is",
                                                                 "Replace with Mean", "Replace with Zero"), inline = T))
                                 ),
                                 fluidRow(
                                   box(solidHeader = T, background = "black", width = 12, uiOutput("col001")),
                                   box(width = 6, height = 450, title = "Box Plot and Quantile Stats",status = "primary", solidHeader = T,
                                       background = "black" , plotOutput("boxplot", height = "300px"),
                                       br(),
                                       verbatimTextOutput("summ")),
                                   
                                   #tabBox( side = "left",  width = 6, height = 400, selected = "Box Plot",
                                   #      tabPanel(title = "Summary", status = "primary", solidHeader = T,
                                   #             background = "black" , verbatimTextOutput("summ")),
                                   #     tabPanel(title = "Box Plot", status = "primary", solidHeader = T,
                                   #            background = "black" , plotOutput("boxplot", height = "385px"))
                                   
                                   box(title = "Histogram", status = "primary", solidHeader = T,
                                       background = "black", plotOutput("histogram1", height = "300px"),
                                       sliderInput("bins", "Number of Breaks", 1,50,10)
                                   )
                                 ),
                                 fluidRow(
                                   box(width = 12, radioButtons("capping_option","Outlier Treatment",
                                                                choices = c("95 Percentile Cap (Default)", "Manual Capping"), inline = T)),
                                   box(solidHeader = T, background = "black", width = 12, uiOutput("col2"),
                                       sliderInput("min", "Select Lower Range", 0.0,0.05,0.00),
                                       sliderInput("max", "Select Upper Range", 0.95,1.0,1.0 ),
                                       box(width = 12,
                                         box(verbatimTextOutput("count")),
                                         box(verbatimTextOutput("count2"))),
                                       actionButton(inputId = "update1", "Submit")
                                   )
                                 ), 
                                 fluidRow(
                                   box(solidHeader = T, background = "black", width = 12, uiOutput("col002")),
                                   box(width = 6, height = 450, title = "Box Plot after Capping",status = "primary", solidHeader = T,
                                       background = "black" , plotOutput("boxplot2", height = "300px"),
                                       br(),
                                       verbatimTextOutput("summary2")),
                                   box(title = "Histogram after Capping", status = "primary", solidHeader = T,
                                       background = "black", plotOutput("histogram2", height = "300px"),
                                       sliderInput("bins2", "Number of Breaks", 1,50,10)
                                   )
                                 )
                         ),
                         tabItem(tabName = "third", h2("Relation between variables and Capping"),
                                 fluidRow(box("Correlation Matrix",solidHeader = T, background = "black", width = 6,
                                              plotOutput("cormatrix")),
                                          box("Scatter Plot", solidHeader = T, background = "black", width = 6,
                                              uiOutput("col3"),
                                              uiOutput("col4"),
                                              plotOutput("scatplot2", height = "250px"))
                                 ),
                                 fluidRow(box(title = "View the table", width = 12, height = 325, status = "primary", solidHeader = T,
                                              verbatimTextOutput("table3"),
                                              tags$head(tags$style("#table3{color:black; font-size:12px; font-style:italic;
                                                                   overflow-y:scroll; max-height: 250px; background: ghostwhite;}")),
                                              br(),
                                              downloadButton("downloadclustertable", "Download"))
                                              )
                                 ),
                         tabItem(tabName = "fourth", h2("Cluster Creation and Allocation"),
                                 fluidRow(box(width = 12, background = "black", "Press the button to get optimal number of clusters",
                                              actionButton("execute", "Run"))
                                 ),
                                 fluidRow(box("Elbow curve",solidHeader = T, background = "black", width = 6,
                                              plotOutput("elbow_curve")),
                                          box("Hierarchical Cluster",solidHeader = T, background = "black", width = 6,
                                              plotOutput("hclust_plot"))
                                 ),
                                 fluidRow(box(sliderInput("clustno", "Number of Clusters", 1,50,2),
                                              actionButton(inputId = "submit", "Update"))
                                 ),
                                 fluidRow(
                                   tabBox( side = "left",  width = 12, height = 400,
                                           tabPanel(title = "Cluster Size", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    dataTableOutput("size")
                                           ),
                                           tabPanel(title = "Cluster Centroids", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    dataTableOutput("center" ),
                                                    downloadButton("table4", "Download")
                                           ),
                                           tabPanel(title = "Cluster Mean Value", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    dataTableOutput("cluster_vis")
                                           ),
                                           
                                           tabPanel(title = "Significance of Cluster Mean", status = "primary", solidHeader = T,
                                                    background = "black" ,
                                                    DT::dataTableOutput("cluster_zscore", height = "auto", width = "auto" ),
                                                    downloadButton("table5", "Download")
                                           )
                                   )              
                                 )
                         ),
                         tabItem(tabName = "fifth", h2("Cluster Allocation"),
                                 fluidRow(box(width = 12,  DT::dataTableOutput("final_table"))),
                                 fluidRow(
                                   tabBox(side = "left",  width = 12, height = 600,
                                          tabPanel(title = "Relationship between Variables", status = "primary", solidHeader = T,
                                                   background = "black",
                                                   box( background = 'black', width = 3, height = 500,
                                                        uiOutput("col5"),
                                                        radioButtons("functiontype1", "Select Function", choices = c("Sum","Mean"), inline = T),
                                                        uiOutput("col6"),
                                                        radioButtons("functiontype2", "Select Function", choices = c("Sum","Mean"), inline = T),
                                                        uiOutput("col7"),
                                                        radioButtons("functiontype3", "Select Function", choices = c("Sum","Mean"), inline = T),
                                                        actionButton("run1", "Submit")),
                                                   box(background = 'black', width = 9, height = 500,
                                                       plotlyOutput("bubblechart", height = 'auto', width = 'auto'))
                                          ),
                                          tabPanel(title = "View of Cluster Centre", status = "primary", solidHeader = T,
                                                   background = "black",
                                                   box( background = 'black',width = 12, height = 500,
                                                        uiOutput("col8"),
                                                        plotlyOutput("clusterbarchart", height = 'auto', width = 'auto')
                                                   )
                                          ),
                                          tabPanel(title = "View of Descriptor Values", status = "primary", solidHeader = T,
                                                   background = "black",
                                                   box(width = 12,
                                                       uiOutput("col9"), 
                                                       plotlyOutput("Descriptorbarchart", height = 'auto', width = 'auto')
                                                   )
                                          ),
                                          tabPanel(title = "Descriptor Table", status = "primary", solidHeader = T,
                                                   background = "black" ,
                                                   dataTableOutput("descriptor_vis")
                                          )
                                   )
                                 )
                                 
                         )
                         )
                       )
                     )



server <- function(input, output) {
  
  
  
  
  Raw_Dataframe_Excel <- reactive({
    file_to_read = input$file
    if(is.null(file_to_read)){return()}
    df <- read.table(file_to_read$datapath , sep = input$sep, header = input$header)
    #na.omit(df)
  })
  
  
  Raw_Dataframe_Database <- eventReactive(input$run, {
    driver <- JDBC("com.amazon.redshift.jdbc41.Driver", "~/.redshiftTools/redshift-driver.jar", identifier.quote="`")
    url <- sprintf("jdbc:postgresql://redshift.amazonaws.com:)
    jconn <- dbConnect(driver, url, input$USERNAME, input$PASSWORD)
    dbListTables(jconn)
    m <- dbGetQuery(jconn, input$TABLENAME)
  })
  
  Raw_Dataframe <- reactive({
    if(identical(input$option, "Excel")){
      Raw_Dataframe_Excel()
    } else {
      Raw_Dataframe_Database()
    }
  })
  
  output$table_primary <- DT::renderDataTable({
    DT::datatable(Raw_Dataframe(), options = list(orderClasses = TRUE, pageLength = 10,lengthMenu = c(2, 12, 18)), fillContainer =T,)
  })
  
  
  output$col00 <- renderUI({
    selectInput("dropdown00", "Select Variables",  names(Raw_Dataframe()), selected = names(Raw_Dataframe())[1], multiple = T )
  })
  
  Dataframe<-eventReactive(input$update00,{Raw_Dataframe()[ ,c(input$dropdown00)]})
  
  output$table00 <- DT::renderDataTable({
    DT::datatable(Dataframe(), options = list(orderClasses = TRUE, pageLength = 10), fillContainer =T)
  })
  
  output$col01 <- renderUI({
    selectInput("dropdown01", "Select Variables",  names(Raw_Dataframe()), selected = names(Raw_Dataframe())[1], multiple = T )
  })
  
  Dataframe_Descrip<-eventReactive(input$update01,{Raw_Dataframe()[ ,c(input$dropdown01)]})
  
  output$table01 <- DT::renderDataTable({
    DT::datatable(Dataframe_Descrip(), options = list(orderClasses = TRUE, pageLength = 10), fillContainer =T)
  })
  
  
  
  output$Count_of_missing_value<-  renderText({
    total_na <- sum(is.na(New_Variable_data()))
    paste("Total no. of Missing Value in Dataset =",  total_na, sep = " ")
  })
  
  output$na_in_each_col <- renderPrint({
    sapply(New_Variable_data(), function(y) sum(length(which(is.na(y)))))
  })
  
  output$col1 <- renderUI({
    selectInput("dropdown1", "Select the Variable",  names(Dataframe()), selected = names(Dataframe())[1], multiple = T)
  })
  
  #no_missing <- reactive({
  #  Dataframe()})
  
  #mean_missing <- reactive({
  #cd <- replace(Dataframe()[,c(input$dropdown1)],is.na(Dataframe()[,c(input$dropdown1)]),
  #      mean(Dataframe()[,c(input$dropdown1)],na.rm = TRUE))})
  #col_n <- c(input$dropdown1)
  #index_no <- match(col_n, names(Dataframe()))
  #for (i in index_no) {
  # Dataframe()[is.na(Dataframe()[,i]),i] <- round(mean(Dataframe()[,i],na.rm = TRUE),2)
  #}})
  
  #zero_replace_missing <- reactive({
  #zer <- replace(Dataframe()[,c(input$dropdown1)],is.na(Dataframe()[,c(input$dropdown1)]),
  #                0)})
  #col_n <- c(input$dropdown1)
  #index_no <- match(col_n, names(Dataframe()))
  # Dataframe()[,index_no][is.na(Dataframe()[,index_no])] <- 0
  #})
  
  
  New_Variable_data <- reactive({
    a <- Dataframe()
    if(identical(input$missing_value, "Proceed as it is")){
      a
    } else if (identical(input$missing_value, "Replace with Zero")){
      col_n <- c(input$dropdown1)
      ind_no <- match(col_n, names(a))
      a[,ind_no][is.na(a[,ind_no])] <- 0
      return(a)
    } else {
      col_n <- c(input$dropdown1)
      index_no <- match(col_n, names(a))
      for (i in index_no) {
        a[is.na(a[,i]),i] <- round(mean(a[,i],na.rm = TRUE),2)
      }
      return(a)
    }})
  
  output$col001 <- renderUI({
    selectInput("dropdown001", "Select the Variable",  names(New_Variable_data()), selected = names(New_Variable_data())[1], multiple = F)
  })
  
  output$summ <- renderPrint({summary(New_Variable_data()[,c(input$dropdown001)])})
  
  #output$boxplot <- renderPlot({boxplot(Dataframe()[,c(input$dropdown1)], horizontal = T, border = T, col = "blue")})
  output$boxplot <- renderPlot({boxplot(New_Variable_data()[,c(input$dropdown001)], horizontal = T, border = T, col = "blue")})
  
  
  output$histogram1 <- renderPlot({hist(New_Variable_data()[,c(input$dropdown001)], breaks = input$bins, col = "#6699FF", probability = T,, main = paste("Histogram of",input$dropdown001), xlab = input$dropdown001)
    lines(density(rnorm(length(New_Variable_data()[,c(input$dropdown001)]), mean(New_Variable_data()[,c(input$dropdown001)]),
                        sd(New_Variable_data()[,c(input$dropdown001)]) ), adjust = 2), col ="Red", lwd=2)
  })
  
  output$col2 <- renderUI({
    selectInput("dropdown2", "Select the Variable(For Manual Capping)",  names(New_Variable_data()), multiple = T)
  })
  
  #output$count<- renderText({
  # quantiles_count <- round(quantile(Dataframe()[,c(input$dropdown2)], c(input$min, input$max)))
  #total_count <- length(Dataframe()[,c(input$dropdown2)])
  #min_count <- length(which(Dataframe()[,c(input$dropdown2)] < quantiles_count[1]) == TRUE)
  #max_count <- length(which(Dataframe()[,c(input$dropdown2)] > quantiles_count[2]) == TRUE)
  #paste(paste("Count of total observation =",total_count, sep = " "),
  #      paste("Count of Min Outliers =", min_count, "Count of Max Outliers =", max_count, sep = " "),
  #      sep = "\n")
  
  #})
  
  output$count <- renderPrint({
    total_row <- nrow(New_Variable_data())
   
    Cap_max <- function(x){ stopifnot(is.numeric(x))
      quantiles <- quantile( x, c(input$max) , na.rm = TRUE)
      max_e <- length(which(x > quantiles[1]))
    }
    
    Max_data <- as.data.frame(lapply(New_Variable_data(), Cap_max))
    
    m <- c(input$dropdown2)
    matc <- match(m, names(New_Variable_data()))
    
    cat(cat("Count of Total Observation =",total_row, sep = " ", "\n"),
        (for(i in matc){
          cat("Count of Max Outliers of",names(New_Variable_data())[i],"=", Max_data[1,i], sep = " ","\n")
        })
        )
  })
  
  output$count2 <- renderPrint({
    Cap_min <- function(x){ stopifnot(is.numeric(x))
      quantiles <- quantile( x, c(input$min) , na.rm = TRUE)
      min_e <- length(which(x < quantiles[1]))
    }
    
    Min_data <- as.data.frame(lapply(New_Variable_data(), Cap_min))
    
    m <- c(input$dropdown2)
    matc <- match(m, names(New_Variable_data()))
    
     for(i in matc){
      cat("Count of Min Outliers of",names(New_Variable_data())[i],"=", Min_data[1,i], sep = " ","\n")
    }
    
  })
  
  
  #capping_option
  default_capping <- reactive({
    a <- New_Variable_data()
    for(i in 1:ncol(a)) {
      a[,i] <- squish(a[,i],quantile(a[,i],c(0.05,0.95)))
    }
    return(a)
  })
  
  manual_capping <- eventReactive(input$update1,{
    b <- New_Variable_data()
    col_n <- c(input$dropdown2)
    index_no <- match(col_n, names(b))
    for (i in index_no) {
      b[,i] <- squish(b[,i], round(quantile(b[,i],c(input$min, input$max))))
    }
    return(b)
  })
  
  Dataframe2 <- reactive({
    if(identical(input$capping_option, "95 Percentile Cap (Default)")){
      default_capping()
    } else {
      manual_capping()
    }})
  
  dropdown_option <- reactive({
    if(identical(input$capping_option, "95 Percentile Cap (Default)")){
      as.vector(names(default_capping()))
    } else {
      c(input$dropdown2)
    }
  })
  
  output$col002 <- renderUI({
    selectInput("dropdown002", "Select the Variable", dropdown_option() , selected = names(Dataframe2())[1], multiple = F)
  })
  
  
  
  
  #Dataframe1 <- eventReactive(input$update1,{
  # squish(Dataframe()[,c(input$dropdown2)], round(quantile(Dataframe()[,c(input$dropdown2)], c(input$min, input$max))))
  #})
  
  output$summary2 <- renderPrint({
    summary(Dataframe2()[,c(input$dropdown002)])
  })
  
  output$boxplot2 <- renderPlot({
    boxplot(Dataframe2()[,c(input$dropdown002)], horizontal = T, border = T, col = "blue")
  })
  
  output$histogram2 <- renderPlot({hist(Dataframe2()[,c(input$dropdown002)], breaks = input$bins2, col = "#6699FF", probability = T,main = paste("Histogram of",input$dropdown002),xlab = input$dropdown002)
    lines(density(rnorm(length(Dataframe2()[,c(input$dropdown002)]), mean(Dataframe2()[,c(input$dropdown002)]),
                        sd(Dataframe2()[,c(input$dropdown002)]) ), adjust = 2), col ="Red", lwd=2) })
  
  
  
  ####)
  
  #memory <- reactiveValues(dat = NULL)
  #capped <- reactive({Dataframe1()})
  
  #Dataframe2 <- eventReactive(input$savechanges, {
  #  isolate(dat <- memory$dat)
  #  if (is.null(dat)) {
  #    memory$dat <- data.frame(xvals = capped())
  #    names(memory$dat)[1] <- input$dropdown2
  #  } else {
  #    #print(capped())
  #    #df.tmp <- capped()
  #    #names(df.tmp) <- input$dropdown1
  #    memory$dat[input$dropdown2] <- capped()
  #    #memory$dat <- cbind(dat,capped())
  #}
  # return(memory$dat)
  #})
  
  ###
  
  output$table3 <- renderPrint({round(Dataframe2(), digits = 3)})
  
  output$downloadclustertable <- downloadHandler(
    filename = function(){
      paste("Cluster_Capped_values", "csv", sep=".")
    },
    content = function(file){
      write.csv(Dataframe2(),file)
    }
  )
  
  output$cormatrix <- renderPlot({corrplot(cor(Dataframe2()),type = "lower", method = "ellipse", order = "hclust",
                                           hclust.method = "centroid")})
  output$col3 <- renderUI({
    selectInput("dropdown3", "Select the X AXIS",  names(Dataframe2()), selected = names(Dataframe2())[1] )
  })
  output$col4 <- renderUI({
    selectInput("dropdown4", "Select the Y AXIS",  names(Dataframe2()), selected = names(Dataframe2())[1] )
  })
  
  output$scatplot2 <- renderPlot({
    ggplot(Dataframe2(), aes(Dataframe2()[,c(input$dropdown3)], Dataframe2()[,c(input$dropdown4)])) + geom_point()
    
  })
  
  elbowcurve <- eventReactive(input$execute,{
    set.seed(1230)
    wcsx = vector()
    for (i in 1:50) wcsx[i] = sum(kmeans(Dataframe2(), i, nstart = 20)$withinss)
    plot(1:50,
         wcsx,
         type = 'b',
         main = paste('The Elbow Method'),
         xlab = 'Number of clusters',
         ylab = 'WCSS')
  })
  
  output$elbow_curve <- renderPlot({
    elbowcurve()
  })
  
  hclust_render  <- eventReactive(input$execute,{
    df <- Dataframe2()
    if(nrow(df) > 5000){
      set.seed(1337)
      samps <- sample(nrow(df),5000)
      df_samp <- df[samps,]
    } else{
      df_samp <- df
    }
    rm(df)
    h <- hclust(dist(df_samp, method = "euclidean"), method = "complete")
    i <- which.max(diff(h$height))
    cut_height <- (h$height[i] + h$height[i+1])/2
    plot(h,  ylab = "Height")
    abline(h=cut_height, col="red", lty=2)
  })
  
  output$hclust_plot <- renderPlot({
    hclust_render()
  })
  
  clusters <- eventReactive(input$submit,{
    set.seed(123)
    
    normalise <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                          min(x, na.rm=TRUE))}
    
    mydata <- Dataframe2()
    
    mydata <- as.data.frame(lapply(Dataframe2(), normalise))
    
    
    kmeans(mydata, input$clustno, iter.max = 100, nstart=20)
  })
  
  Clustersize <- reactive({ dd2 <- data.frame(Size = clusters()$size)})
  
  output$size <- renderDataTable({
    
    datatable(Clustersize())%>%
      formatStyle(names(Clustersize()),
                  background = styleColorBar(range(Clustersize()), 'lightblue'),
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'black')
    
  })
  
  
  output$center <- renderDataTable({
    
    dd1 <- data.frame(round(clusters()$centers, digits = 3))
    
  })
  
  output$table4 <- downloadHandler(
    filename = function(){
      paste("Centre Values", "csv", sep=".")
    },
    content = function(file){
      write.csv(clusters()$centers,file)
    }
  )
  
  
  Cluster_Vis_Data <- reactive({
    
    d.min <- round(colwise(min)(Dataframe2()), digits = 2)
    d.max <- round(colwise(max)(Dataframe2()), digits = 2)
    d.d <-  rbind.data.frame(d.min,d.max)
    
    m <- data.frame(clusters()$centers)
    
    abc <- data.frame()
    temp <- data.frame()
    for (i in 1:nrow(m))
      for (j in 1:ncol(m))
        temp[i,j] <- (m[i,j]*(d.d[2,j]-d.d[1,j]))+d.d[1,j]
    abc <- round(rbind(abc,temp), digits = 2)
    colnames(abc) <- colnames(m)
    abc
  })
  
  output$cluster_vis <- renderDataTable({datatable(Cluster_Vis_Data())%>%
      formatStyle(names(Cluster_Vis_Data()),
                  background = styleColorBar(range(Cluster_Vis_Data()), 'lightblue'),
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'black')
  })
  
  
  Cluster_Vis_Data_Zvalue <- reactive({
    d.sd <-  round(colwise(sd)(Dataframe2()), digits = 2)
    d.mean <-  round(colMeans(Dataframe2()), digits = 2)
    d.d1 <-  rbind.data.frame(d.sd,d.mean)
    
    m1 <- data.frame(Cluster_Vis_Data())
    
    abc1 <- data.frame()
    temp1 <- data.frame()
    for (i in 1:nrow(m1))
      for (j in 1:ncol(m1))
        temp1[i,j] <- (m1[i,j] - d.d1[2,j])/d.d1[1,j]
    abc1 <- round(rbind(abc1,temp1), digits = 2)
    colnames(abc1) <- colnames(m1)
    abc1
    
  })
  
  output$cluster_zscore <- renderDataTable({
    datatable(Cluster_Vis_Data_Zvalue())%>%
      formatStyle(names(Cluster_Vis_Data_Zvalue()),
                  background = styleInterval(c(-3,-2,-1,1,2,3),c('#133B76','#386EBF','#AECCFA','#E4EEFD','#AECCFA','#386EBF','#133B76')),
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'black' )
  })
  
  
  Dataframe_Cluster <- reactive({df <- cbind.data.frame(Dataframe2(), clusters()$cluster)})
  
  output$final_table <- DT::renderDataTable({
    DT::datatable(Dataframe_Cluster(), options = list(orderClasses = TRUE, pageLength = 10), fillContainer =T)
  })
  
  output$table5 <- downloadHandler(
    filename = function(){
      paste("Cluster Allocation", "csv", sep=".")
    },
    content = function(file){
      write.csv(Dataframe_Cluster(),file)
    }
  )
  
  
  FinalData_1 <- reactive({Dataframe_Cluster()[,-ncol(Dataframe_Cluster())]})
  
  output$col5 <- renderUI({
    selectInput("dropdown5", "Select the X AXIS",  names(FinalData_1()), selected = names(FinalData())[1] )
  })
  
  output$col6 <- renderUI({
    selectInput("dropdown6", "Select the Y AXIS",  names(FinalData_1()), selected = names(FinalData())[1] )
  })
  output$col7 <- renderUI({
    selectInput("dropdown7", "Select the Bubble Size",  names(FinalData_1()), selected = names(FinalData())[1] )
  })
  
  FinalData <- reactive({Dataframe_Cluster()})
  
  plot_data <- eventReactive(input$run1,{
    
    Cluster_Number <- unique(FinalData()[,"clusters()$cluster"])
    x_data <- if(identical(input$functiontype1, "Sum")){
      aggregate(FinalData()[,input$dropdown5], by=list(FinalData()[,"clusters()$cluster"]), FUN=sum)
    } else {
      aggregate(FinalData()[,input$dropdown5], by=list(FinalData()[,"clusters()$cluster"]), FUN=mean)
    }
    
    Y_data <- if(identical(input$functiontype2, "Sum")){
      aggregate(FinalData()[,input$dropdown6], by=list(FinalData()[,"clusters()$cluster"]), FUN=sum)
    } else {
      aggregate(FinalData()[,input$dropdown6], by=list(FinalData()[,"clusters()$cluster"]), FUN=mean)
    }
    bubble_size <- if(identical(input$functiontype3, "Sum")){
      aggregate(FinalData()[,input$dropdown7], by=list(FinalData()[,"clusters()$cluster"]), FUN=sum)
    } else {
      aggregate(FinalData()[,input$dropdown7], by=list(FinalData()[,"clusters()$cluster"]), FUN=mean)
    }
    
    h <- cbind.data.frame(Cluster_Number,x_data$x,Y_data$x, bubble_size$x )
    colnames(h) <- c('Cluster_Number', input$dropdow5,input$dropdow6, input$dropdown7)
    h
  })
  
  
  output$bubblechart <- renderPlotly({
    ggplot(plot_data(), aes(x = plot_data()[,2], y = plot_data()[,3], size = plot_data()[,4] )) +
      geom_point(shape = 21, colour = "black", fill = "blue" ) +
      scale_size(range = c(1, 10)) +
      theme(legend.position = "bottom", legend.direction = "horizontal",
            legend.box = "horizontal",
            legend.key.size = unit(1, "cm"),
            axis.line = element_line(size=1, colour = "black"),
            panel.grid.major = element_line(colour = "#d3d3d3"),
            panel.grid.minor = element_blank(),
            plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
            text=element_text(family="Tahoma"),
            axis.text.x=element_text(colour="black", size = 9),
            axis.text.y=element_text(colour="black", size = 9),
            axis.title.x=element_blank(),
            axis.title.y=element_blank())
  })
  
  clustertable <- reactive({
    tab <- data.frame(melt(Cluster_Vis_Data()))
    colnames(tab) <- c( 'Variable', 'Center')
    X <- data.frame(Cluster_Num = rep(1:nrow(Cluster_Vis_Data()), times = ncol(Cluster_Vis_Data())))
    cbind.data.frame(X,tab)
  })
  
  
  output$col8 <- renderUI({
    selectInput("dropdown8", "Select Cluster Number",  names(Cluster_Vis_Data()), selected = 1, multiple = T )
  })
  
  output$clusterbarchart <- renderPlotly({
    ggplot(subset.data.frame(clustertable(), Variable %in% c(input$dropdown8) ),aes(x=Cluster_Num,y=Center,fill=factor(Variable)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Cluster"
      )
  })
  
  Data_Descrip <- reactive({
    xy <- cbind.data.frame(Dataframe_Descrip(), clusters()$cluster)
    names(xy)[length(names(xy))] <- 'cluster_number'
    xy1 <- data.frame(aggregate(xy, by=list(xy[,'cluster_number']), FUN=mean))
    xy1 <- xy1[,c(-1,-ncol(xy1))]
    xy2 <- data.frame(melt(xy1))
    colnames(xy2) <- c( 'Variable', 'Mean_Value')
    Y <- data.frame(Cluster_Num = rep(1:nrow(xy1), times = ncol(xy1)))
    cbind.data.frame(Y,xy2)
  })
  
  output$col9 <- renderUI({
    selectInput("dropdown9", "Select Cluster Number",  names(Dataframe_Descrip()), selected = 1, multiple = T )
  })
  
  output$Descriptorbarchart <- renderPlotly({
    ggplot(subset.data.frame(Data_Descrip(), Variable %in% c(input$dropdown9) ),aes(x=Cluster_Num,y=Mean_Value,fill=factor(Variable)))+
      geom_bar(stat="identity",position="dodge")+
      scale_fill_discrete(name="Cluster")
    
  })
  
  output$descriptor_vis <- renderDataTable({
    
    s <- dcast(Data_Descrip(), formula = Cluster_Num~Variable, fun.aggregate = sum, value.var = "Mean_Value")
    s <- round(s, digits = 3)
    datatable(s)%>%
      formatStyle(names(s),
                  background = styleColorBar(range(s), 'lightblue'),
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center',
                  color = 'black')
    
  })
  
}

shinyApp(ui, server)
