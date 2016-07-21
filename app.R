################################################
############## LOAD PACKAGES ###################
################################################
rm(list=ls())

# Load additional packages
x <- c("openxlsx", "gplots", "reshape2", "scales", "RColorBrewer", "stats", "graphics", "ggplot2", "gdata", "plyr", "dendextend", "DT", "gridExtra", "htmlwidgets")
lapply(x, require, character.only=TRUE)

# Load shiny packages
library(shiny)
library(shinyFiles)
library(shinyjs)

################################################
################## UI.R ########################
################################################

ui <- fluidPage(
  titlePanel("Clustering Analysis with Shiny HeatMap (CASH)"),
  fluidRow(
    column(2,
           wellPanel(
             ########### Load Data ##########
             selectInput("file1",label= "Select an example dataset or upload your own with 'Load my own data.'", 
                         choices = c("Example Data File"="Example", "Load my own data" = "load_my_own")),
             conditionalPanel("input.file1 == 'load_my_own'",
                              fileInput('file2', 'Choose file to upload (maximum size 10 MB)', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))), 
             conditionalPanel("input.file1 == 'Example' & input.conditionedPanels==1",
                              downloadButton('downloadEx', 'Download Example DataSet')),
             conditionalPanel("input.conditionedPanels==1",
                              helpText("After data selection, to view generated HeatMap, click on HeatMap tab.")) 
           ),
           conditionalPanel("input.conditionedPanels == 2 | input.conditionedPanels == 3 |input.conditionedPanels == 4", 
                            wellPanel(
                              textInput("fname", "Type the file name you would like to save as", value = "HeatMap"),
                              downloadButton('downloadPlots', 'Download HeatMap and dendrograms'),
                              br(),
                              conditionalPanel("input.conditionedPanels == 3",   
                                               h5(downloadLink('downloadCuttree', 'Download Column clusters after cut-tree'))),
                              conditionalPanel("input.conditionedPanels == 4", 
                                               h5(downloadLink('downloadCuttree2', 'Download Row clusters after cut-tree')))
                            )
           )
    ),
    column(8,
           tabsetPanel(type = "tabs", 
                       tabPanel("ReadMe", htmlOutput("ReadMe"), tableOutput("Eg"), htmlOutput("Caption1"), tableOutput("Eg2"), htmlOutput("Caption2"), htmlOutput("blurp"), value = 1),
                       tabPanel("HeatMap", plotOutput("plot", width = 1200, height = 1200 ), value=2), 
                       tabPanel("Column Dendrogram", plotOutput("plot1", height= 600, width = 1200), htmlOutput("display"), br(), DT::dataTableOutput("df"), htmlOutput("pv"), htmlOutput("pvalue"), plotOutput("pvalplot1", height= 600, width = 600), value=3), 
                       tabPanel("Row Dendrogram", plotOutput("plot2", height = 600, width = 1200), htmlOutput("display2"), br(), DT::dataTableOutput("df2"), htmlOutput("pv2"), htmlOutput("pvalue2"), plotOutput("pvalplot2", height= 600, width = 600), value =4),
                       id = "conditionedPanels"
           )
    ),
    column(2, 
           conditionalPanel("input.conditionedPanels==2",
                            wellPanel(  
                              ########## HeatMap Clustering options ##########
                              h4("Heat Map Options"),
                              selectInput("norm", "Normalization type",
                                          c("row", "col", "both", "none")),
                              sliderInput("inSlider", "Scale Range",
                                          min = -10, max = 20, value = c(-2, 2)),
                              conditionalPanel("input.conditionedPanels==1 | input.conditionedPanels==2", 
                                               sliderInput("inSlider2", "Plot Margin dimensions",
                                                           min = 0, max = 15, value = c(10, 12)) )
                            ),
                            wellPanel(  
                              h4("Clustering Measures"),
                              selectInput("dist", "Distance Method",
                                          c("pearson correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                              selectInput("hclust", "Agglomerative Linkage Method", 
                                          c("average", "complete", "ward.D", "ward.D2", "single", "mcquitty", "median", "centroid")),
                              conditionalPanel("input.conditionedPanels==1 | input.conditionedPanels==2", 
                                               radioButtons("clust_byrow", "Row dendrogram", inline = TRUE, c("TRUE", "FALSE")) ),
                              conditionalPanel("input.conditionedPanels==1 | input.conditionedPanels==2", 
                                               radioButtons("clust_bycol", "Col dendrogram", inline = TRUE, c("TRUE", "FALSE")) ),
                              conditionalPanel("input.conditionedPanels==2", 
                                               radioButtons("dispRow", "Display Row labels?:", inline = TRUE,c("No", "Yes")),
                                               conditionalPanel("input.dispRow == 'Yes'", sliderInput("size1", "If yes, Row Label font size", min = 0.01, max = 3, value = 0.5)),
                                               radioButtons("dispCol", "Display Col labels?:", inline = TRUE, c("No", "Yes")),
                                               conditionalPanel("input.dispCol == 'Yes'", sliderInput("size2", "If yes, Col Label font size", min = 0.01, max = 3, value = 1.2) ))
                            ), 
                            
                            wellPanel(
                              h4("Heat Map colors"),
                              colourInput("low", "low", "blue", returnName = TRUE, palette = "limited", showColour = "background"),
                              colourInput("mid", "mid", "white", returnName = TRUE, palette = "limited", showColour = "background"),
                              colourInput("high", "high", "red", returnName = TRUE, palette = "limited", showColour = "background") )
           ) ,
           
           conditionalPanel("input.conditionedPanels==3 | input.conditionedPanels==4",
                            wellPanel(
                              conditionalPanel("input.conditionedPanels==3", 
                                               sliderInput("sizeClable", "Adjust Column Label font size", min = 0.01, max = 3, value = 0.8) ),
                              conditionalPanel("input.conditionedPanels==4", 
                                               sliderInput("sizeRlable", "Adjust Row Label font size", min = 0.01, max = 3, value = 0.42) ),
                              
                              # Column Dendrogram tab
                              conditionalPanel("input.conditionedPanels==3", 
                                               radioButtons("cutcolden", "Cut Col dendrogram?:", inline = TRUE, c("No" = FALSE, "Yes" = TRUE)) ),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE'", 
                                               numericInput("cuttree", "Cut Col Dendrogram at:", 2)),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE'",
                                               radioButtons("pvalue_cal", "Assess Gene set significance in separation of specimens into 2 clusters?:", c("No" = FALSE, "Yes" = TRUE), inline = TRUE)),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'" , 
                                               selectInput("file3", label= "Select a dataset or upload your own with 'Load my own data.'", choices = c("Meth Sampling Data" ="Meth.Example", "Load my own sampling data" = "load_my_own_s_data"))),
                              conditionalPanel("input.conditionedPanels==3 & input.file3 == 'load_my_own_s_data'",
                                               fileInput('file4', 'Choose file to upload to sample from to estimate significance of separation', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))) ,
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'", 
                                               numericInput("n", "Sample size for bootstrap:", 1000)),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'", 
                                               numericInput("n_iter", "No. of iterations for bootstrap:", 10)),
                              #conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'", 
                              #                 radioButtons("histplot1", "Display distribution of permuted p-values in comparison to obs p-value ?:", c( "Yes" = TRUE, "No" = FALSE), inline = TRUE)),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'", 
                                               actionButton("goButton", "Go!")),
                              conditionalPanel("input.conditionedPanels==3 & input.cutcolden == 'TRUE' & input.pvalue_cal == 'TRUE'", 
                                               p("Click the button to start sampling using bootstrap method for estimating the p-value. A progress indicator will appear shortly (~approx 10 s), on top of page indicating the status. Once complete, the p-value will be displayed in the main panel.")),
                              
                              # Row Dendrogram tab
                              conditionalPanel("input.conditionedPanels==4", 
                                               radioButtons("cutrowden", "Cut Row dendrogram?:", inline = TRUE, c("No" = FALSE, "Yes" = TRUE)) ),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE'", 
                                               numericInput("cuttree2", "Cut Row Dendrogram at:", 2)),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE'",
                                               radioButtons("pvalue_cal2", "Assess significance of samples in separation of gene set into 2 clusters (only for more than 2 cluster groups)?:", inline = TRUE, c("No" = FALSE, "Yes" = TRUE))),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE' " , 
                                               selectInput("file3", label= "Select a dataset or upload your own with 'Load my own data.'", choices = c("Example Meth Sampling Data" ="Meth.Example", "Load my own sampling data" = "load_my_own_s_data"))),
                              conditionalPanel("input.conditionedPanels==4 & input.file3 == 'load_my_own_s_data'",
                                               fileInput('file4', 'Choose file to upload to sample from to estimate significance of separation', accept=c('.xlsx','text/csv', 'text/comma-separated-values,text/plain', '.csv'))) ,
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE'", 
                                               numericInput("n", "Sample size for bootstrap:", 1000)),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE'", 
                                               numericInput("n_iter2", "No. of iterations for bootstrap:", 10)),
                              #conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE'", 
                              #                 radioButtons("histplot2", "Display distribution of permuted p-values in comparison to obs p-value ?:", c( "Yes" = TRUE, "No" = FALSE), inline = TRUE)),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE'", 
                                               actionButton("goButton2", "Go!")),
                              conditionalPanel("input.conditionedPanels==4 & input.cutrowden == 'TRUE' & input.pvalue_cal2 == 'TRUE'", 
                                               p("Click the button to start sampling using bootstrap method for estimating the p-value. A progress indicator will appear shortly (~approx 10 s), on top of page indicating the status. Once complete, the p-value will be displayed in the main panel."))
                              
                              )
           )
    )  
    
  ))


################################################
################ SERVER.R ######################
################################################

source("helper.R")
shiny.maxRequestSize=50*1024^2
#b1= list(numeric(), numeric(), numeric())
#b2 = list(numeric(), numeric(), numeric())

server <- function(input, output, session){
  
  ## Generate input formating instructions for the data
  output$ReadMe <- renderUI({
    str0 <- paste("&emsp;")
    str1 <- paste("Data should be input as a .txt or .xlsx or .csv file. The first two rows of the data file have information about the patients/specimens and their response/subtype; all remaining rows have gene expression data, one row per gene.  In the case of Microarray gene expression data in which there are several probes corresponding to a single gene, a unique identifier would need to be created to separately identify each probe such as, 'Gene 1_p1', 'Gene1_p2' indicating Gene 1 has two probes.  The columns represent the different experimental samples. A maximum of up to 10 different sample groups and 6 different gene groups may be used with this tool.")
    str2 <- paste("DATA FORMAT")
    str3 <- paste("&emsp; 	1.	The first line of the file contains the gene identifier 'gene_id' (column 1), gene group classification 'Groups' (column 2) followed by the patient IDs e.g. TCGA.01.1A2B, one per column, starting at column 3. Column 1 gene identifier has to be labelled 'gene_id' and column 2 header should be labelled 'Groups' for using this tool. Other titles may cause the program to display errors. ")
    str4 <- paste("&emsp; 	2.	The second line of the file contains the patient response classification e.g. Fav/Unf for favorable outcome group vs the unfavorable outcome group or Normal/Tumor, etc., in alphabetical order, starting at column 3. The first two columns for this row should be blank.")
    str5 <- paste("&emsp; 	3.	The remaining lines contain gene expression measurements one line per gene, described in the format below.")
    str6 <- paste("&emsp;&emsp;  a) Column_1. This should contain the gene name, for the user's reference.")
    str7 <- paste("&emsp;&emsp;  b)   Column_ 2.  This should contain the gene group classification e.g. O/U for Over-expressed/Under-expressed or Hyper/Hypo for hypermethylated/hypomethylated in alphabetical order. If only one gene group, use any alphabet e.g. A for each row instead.")
    str8 <- paste("&emsp;&emsp;  c)   Remaining Columns. These should contain the expression measurements as numbers. Data inputted should be non-negative. Columns and rows with zero variance should be removed from the data. Rows containing missing expression measurements, should be also be removed from the input data or it will cause the tool to run into errors." )
    str9 <- paste("NOTE: Clustering is based on scaled data, if the user chooses this option, prior to input into heatmap R function.")
    str10 <- paste("Example format for Data")
    HTML(paste(str0,str1,str0,h5(strong(str2)),str3,str4,str5, str6, str7,str8, str0, str0, strong(str9), str0,str0,str0,strong(em(str10)), str0,sep = '<br/>'))
  })
  
  output$Eg <- renderTable({
    colna1 <- c("gene_id", "Groups","TCGA.01.98GF", "TCGA.08.U5TD", "TCGA.02.D23F", "TCGA.01.12TD","TCGA.02.AOKO", "TCGA.12.T37D", "TCGA.16.Y2S5", "TCGA.01.KITD")
    colna2 <- c(" ", " ", rep("Normal", 3), rep("Tumor", 5))
    s1 <- c("BRCA1", "over", 1.47, 2.18, 5.87, 7.64, 3.40, 7.77, 5.15, 1.56 )
    s2 <- c("YWHAE", "over", 7.93, 2.76, 9.11, 6.96, 5.98, 8.19, 8.91, 0.98)
    s3 <- c("SFN1", "under",8.02, 8.00, 2.17, 1.12, 3.76, 0.02, 3.67, 9.76 )
    s4 <- c("BRAF", "under", 2.75, 5.99, 3.19, 3.09, 2.00, 0.99, 1.28, 8.17)
    
    d <- rbind.data.frame(colna2, s1, s2, s3, s4)
    colnames(d) <- colna1
    head(d)
  })
  
  output$Caption1 <- renderUI({
    str.0 <- paste("&emsp;")
    str.1 <- paste("Table 1 : Example dataset for two gene groups (over and under-expressed) and two patient groups (Normal, Tumor).")
    HTML(paste(strong(str.1),str.0, str.0, str.0, sep = '<br/>'))
  })
  
  output$Eg2 <- renderTable({
    coln1 <- c("gene_id", "Groups","GSM9981", "GSM1870", "GSM4618", "GSM7689", "GSM8772", "GSM1121","GSM1250", "GSM3112", "GSM4987", "GSM1277")
    coln2 <- c(" ", " ", rep("MM", 5), rep("MUGS", 2), "NPC", rep("SM", 2))
    s.1 <- c("YWHAE|210996_s_at", "A", 1.47, 2.18, 5.87, 9.12, 7.34, 1.56, 3.0, 7.77, 3.40, 1.56 )
    s.2 <- c("YWHAE|201020_at", "A", 1.98, 7.93, 2.76, 9.11, 8.46, 0.98, 5.98, 8.19, 8.91, 5.98)
    s.3 <- c("YWHAH|33323_r_at", "A", 8.02, 8.00, 2.17, 10.12, 8.76, 9.76, 3.76, 0.02, 3.67, 7.94)
    s.4 <- c("YWHAB|208743_s_at", "A", 2.75, 5.99, 3.19, 11.86, 6.54, 8.17, 2.00, 0.99, 2.00, 1.17)
    s.5 <- c("YWHAQ|213699_s_at", "A", 9.35, 8.96, 6.67, 8.33, 3.98, 7.11, 1.67, 1.01, 5.18, 8.17)
    
    d.f <- rbind.data.frame(coln2, s.1, s.2, s.3, s.4, s.5)
    colnames(d.f) <- coln1
    head(d.f)
  })
  
  
  output$Caption2 <- renderUI({
    strg.0 <- paste("&emsp;")
    strg.1 <- paste("Table 2: Example dataset for one gene group (marked A) and four patient groups (MM, MUGS, NPC and, SM).")
    strg.2 <- paste(" Both row and column dendrograms can be extracted in their specific tabs. Using the options on the right panel, dendrograms can be cut into desired no. of clusters (default at 2) and a pvalue of significance between the clusters can be determined using bootstrap method. ")
    HTML(paste(strong(strg.1),strg.0, strg.0,strg.2, strg.0, strg.0, sep = '<br/>'))
  })
  
  output$blurp <- renderUI({
    string0 <- paste("&emsp;")
    string2 <- paste("Terms of Use")
    string3 <- paste("This tool was prepared by members of the Winship Biostatistics and Bioinformatics Shared Resource (BBISR) of Emory University. ")
    string4 <- paste("Use of either should properly acknowledge the Winship BBISR in publications, abstracts, presentations, posters, grant proposals, etc. by using the following text	")
    string5 <- paste("&emsp; 'Research reported in this publication was supported in part by the Biostatistics and Bioinformatics  Shared resource of Winship Cancer Institute of Emory University and NIH/NCI under award number P30CA138292. The content is solely the responsibility of the authors and does not necessarily represent the official views of the National Institutes of Health. ")
    string6 <- paste("Authors- Manali Rupji, dual M.S., Bhakti Dwivedi Ph.D. & Jeanne Kowalski Ph.D." )
    string7 <- paste("Maintainer- Manali Rupji 'manali(dot)rupji(at)emory(dot)edu'")
    HTML(paste(string0, string0,strong(string2),string0, string3,string4,string0, string0, string5, string6, string7, sep = '<br/>'))
  })
  
  output$downloadEx <- downloadHandler(
    filename= function() {paste('Example data set_TCGA BRCA meth data.csv')}, 
    content = function(file) {
      d <- readRDS("BRCA.Example.data.rds")
      write.csv(d, file, row.names = FALSE) }
  )
  
  data_input <- reactive({
    if(input$file1 == 'Example'){
      d <- readRDS("BRCA.Example.data.rds")
    }
    else if(input$file1 == 'load_my_own'){
      inFile <- input$file2
      if (is.null(inFile))
        return(NULL)
      else if(grepl(".xlsx", inFile[1])) { d = read.xlsx(as.character(inFile$datapath), colNames = TRUE, rowNames = F) }
      else if(grepl(".csv", inFile[1])) { d = read.csv(as.character(inFile$datapath), header = TRUE, sep = ",", stringsAsFactors = F) }
      else if(grepl(".txt", inFile[1])) { d = read.table(as.character(inFile$datapath), header = TRUE, sep = "\t", stringsAsFactors = F) }
    }
    else 
      return(NULL)
    # dim(data)
    Dataset <- data.frame(d)
    return(Dataset)
  })
  
  output$plot <- renderPlot({
    if(!is.null(data_input()))
    {
      data <- data_input()
      
      # sort columns based on colnames
      data <- data[,order(data[1, ])]
      data <- data[order(data[,2]),]
      
      ### gene names, column name as gene
      gene <- as.character(data$gene_id)
      gene <- gene[-1]
      row.groups <- as.character(as.vector(data[,2]))
      row.groups <- row.groups[-1]
      row.groups.name <- names(table(row.groups))
      number.row.groups <<- length(row.groups.name)
      
      ### column groups
      col.groups <- as.character(as.vector(data[1,]))
      col.groups <- col.groups[c(-1, -2)] # calculate no. of column groups
      col.groups.name <- names(table(col.groups))
      number.col.groups <- length(col.groups.name)
      
      data <- data[-1, c(-1, -2)]
      rownames(data) <- gene
      data<-data[complete.cases(data),]
      data <- data.matrix(data)
      
      
      #print(data)
      
      ## Set color palette
      col1 = colorRampPalette(c(input$low,input$mid,input$high))(299)
      colors <- c(seq(-1,-0.2,length=100),seq(-0.2,0.2,length=100),seq(0.2,1,length=100)) # check slider
      
      ### Color vector for columns
      if(number.col.groups==1) { 
        cell <- c(rep(col.groups.name, number.col.groups))
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
      } else if(number.col.groups==2) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]))
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'  
      } else if(number.col.groups==3) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
      } else if(number.col.groups==4) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
      } else if(number.col.groups==5) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
      } else if(number.col.groups==6) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]]), rep(col.groups.name[6], table(col.groups)[[6]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+1:table(col.groups)[[6]]] <- 'darkgreen'
      } else if(number.col.groups==7) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]]), rep(col.groups.name[6], table(col.groups)[[6]]), rep(col.groups.name[7], table(col.groups)[[7]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+1:table(col.groups)[[6]]] <- 'darkgreen'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+1:table(col.groups)[[7]]] <- 'hotpink'
      } else if(number.col.groups==8) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]]), rep(col.groups.name[6], table(col.groups)[[6]]), rep(col.groups.name[7], table(col.groups)[[7]]), rep(col.groups.name[8], table(col.groups)[[8]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+1:table(col.groups)[[6]]] <- 'darkgreen'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+1:table(col.groups)[[7]]] <- 'hotpink'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+table(col.groups)[[7]]+1:table(col.groups)[[8]]] <- 'brown'
      } else if(number.col.groups==9) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]]), rep(col.groups.name[6], table(col.groups)[[6]]), rep(col.groups.name[7], table(col.groups)[[7]]), rep(col.groups.name[8], table(col.groups)[[8]]), rep(col.groups.name[9], table(col.groups)[[9]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+1:table(col.groups)[[6]]] <- 'darkgreen'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+1:table(col.groups)[[7]]] <- 'hotpink'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+table(col.groups)[[7]]+1:table(col.groups)[[8]]] <- 'brown'
      } else if(number.col.groups==10) {
        cell <- c(rep(col.groups.name[1], table(col.groups)[[1]]), rep(col.groups.name[2],table(col.groups)[[2]]), rep(col.groups.name[3],table(col.groups)[[3]]), rep(col.groups.name[4],table(col.groups)[[4]]), rep(col.groups.name[5],table(col.groups)[[5]]), rep(col.groups.name[6], table(col.groups)[[6]]), rep(col.groups.name[7], table(col.groups)[[7]]), rep(col.groups.name[8], table(col.groups)[[8]]), rep(col.groups.name[9], table(col.groups)[[9]]), rep(col.groups.name[10], table(col.groups)[[10]])) 
        cc1 <- rep(col1[50], length(cell))
        cc1[1:table(col.groups)[[1]]] <- 'pink'
        cc1[table(col.groups)[[1]]+1:table(col.groups)[[2]]] <- 'purple'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+1:table(col.groups)[[3]]] <- 'blue'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+1:table(col.groups)[[4]]] <- 'orange'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+1:table(col.groups)[[5]]] <- 'yellow'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+1:table(col.groups)[[6]]] <- 'darkgreen'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+1:table(col.groups)[[7]]] <- 'hotpink'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+table(col.groups)[[7]]+1:table(col.groups)[[8]]] <- 'brown'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+table(col.groups)[[7]]+table(col.groups)[[8]]+1:table(col.groups)[[9]]] <- 'darkorchid2'
        cc1[table(col.groups)[[1]]+table(col.groups)[[2]]+table(col.groups)[[3]]+table(col.groups)[[4]]+table(col.groups)[[5]]+table(col.groups)[[6]]+table(col.groups)[[7]]+table(col.groups)[[8]]+table(col.groups)[[9]]+1:table(col.groups)[[10]]] <- 'maroon'
      }
      
      ### Color vector for rows
      
      if(number.row.groups==1) { 
        cell2 <- c(rep(row.groups.name, number.row.groups))
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
      } else if(number.row.groups==2) {
        cell2 <- c(rep(row.groups.name[1], table(row.groups)[[1]]), rep(row.groups.name[2],table(row.groups)[[2]]))
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
        cc2[table(row.groups)[[1]]+1:table(row.groups)[[2]]] <- 'orange'  
      } else if(number.row.groups==3) {
        cell2 <- c(rep(row.groups.name[1], table(row.groups)[[1]]), rep(row.groups.name[2],table(row.groups)[[2]]), rep(row.groups.name[3],table(row.groups)[[3]])) 
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
        cc2[table(row.groups)[[1]]+1:table(row.groups)[[2]]] <- 'orange'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+1:table(row.groups)[[3]]] <- 'hotpink'
      } else if(number.row.groups==4) {
        cell2 <- c(rep(row.groups.name[1], table(row.groups)[[1]]), rep(row.groups.name[2],table(row.groups)[[2]]), rep(row.groups.name[3],table(row.groups)[[3]]), rep(row.groups.name[4],table(row.groups)[[4]])) 
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
        cc2[table(row.groups)[[1]]+1:table(row.groups)[[2]]] <- 'orange'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+1:table(row.groups)[[3]]] <- 'hotpink'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+1:table(row.groups)[[4]]] <- 'gray'
      } else if(number.row.groups==5) {
        cell2 <- c(rep(row.groups.name[1], table(row.groups)[[1]]), rep(row.groups.name[2],table(row.groups)[[2]]), rep(row.groups.name[3],table(row.groups)[[3]]), rep(row.groups.name[4],table(row.groups)[[4]]), rep(row.groups.name[5],table(row.groups)[[5]])) 
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
        cc2[table(row.groups)[[1]]+1:table(row.groups)[[2]]] <- 'orange'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+1:table(row.groups)[[3]]] <- 'hotpink'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+1:table(row.groups)[[4]]] <- 'gray'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+table(row.groups)[[4]]+1:table(row.groups)[[5]]] <- 'cyan'
      } else if(number.row.groups==6) {
        cell2 <- c(rep(row.groups.name[1], table(row.groups)[[1]]), rep(row.groups.name[2],table(row.groups)[[2]]), rep(row.groups.name[3],table(row.groups)[[3]]), rep(row.groups.name[4],table(row.groups)[[4]]), rep(row.groups.name[5],table(row.groups)[[5]]), rep(row.groups.name[6],table(row.groups)[[6]])) 
        cc2 <- rep(col1[50], length(cell2))
        cc2[1:table(row.groups)[[1]]] <- 'grey'
        cc2[table(row.groups)[[1]]+1:table(row.groups)[[2]]] <- 'orange'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+1:table(row.groups)[[3]]] <- 'hotpink'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+1:table(row.groups)[[4]]] <- 'gray'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+table(row.groups)[[4]]+1:table(row.groups)[[5]]] <- 'cyan'
        cc2[table(row.groups)[[1]]+table(row.groups)[[2]]+table(row.groups)[[3]]+table(row.groups)[[4]]+table(row.groups)[[5]]+1:table(row.groups)[[6]]] <- 'maroon'
      }
      
      ############# HEATPLOT2 EQUIVALENT HEATMAP2 CLUSTERING ###############
      
      #  data <- as.numeric(data)
      z <- zClust(data, scale =input$norm, zlim=c(input$inSlider[1],input$inSlider[2]))
      
      if(input$dist == "pearson correlation") {
        if(input$dispRow == "No" & input$dispCol=='No') {
          hm <- heatmap.2(z[[1]], labRow = NA, labCol= NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(x) hclust(x,method=input$hclust), distfun=function(x) as.dist((1-cor(t(x)))),cexRow=input$size1,cexCol =input$size2,  key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "No" & input$dispCol=='Yes' ) {
          hm <- heatmap.2(z[[1]], labRow=NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(x) hclust(x,method=input$hclust), distfun=function(x) as.dist((1-cor(t(x)))),cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "Yes" & input$dispCol=='No') {
          hm <- heatmap.2(z[[1]], labCol= NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(x) hclust(x,method=input$hclust), distfun=function(x) as.dist((1-cor(t(x)))),cexRow=input$size1,cexCol =input$size2,  key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "Yes" & input$dispCol=='Yes') {
          hm <- heatmap.2(z[[1]], Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), scale="none", hclust=function(x) hclust(x,method=input$hclust), distfun=function(x) as.dist((1-cor(t(x)))),cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        }
        
        if(number.col.groups==1) {
          legend("topright", legend = paste(col.groups.name), col = "pink", lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==2) {
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("pink", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==3) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("pink", "purple", "blue"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==4) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("pink", "purple","blue", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==5) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("pink", "purple","blue", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==6) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==7) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==8) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==9) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==10) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        }
      }  else {
        if(input$dispRow == "No" & input$dispCol=='No') {
          hm <- heatmap.2(z[[1]], labRow=NA, labCol = NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(c) {hclust(c,method=input$hclust)}, distfun=function(c) {dist(c,method=input$dist)},cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "No" & input$dispCol=='Yes') {
          hm <- heatmap.2(z[[1]], labRow=NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(c) {hclust(c,method=input$hclust)}, distfun=function(c) {dist(c,method=input$dist)},cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "Yes" & input$dispCol=='No') {
          hm <- heatmap.2(z[[1]], labCol = NA, scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(c) {hclust(c,method=input$hclust)}, distfun=function(c) {dist(c,method=input$dist)},cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        } else if(input$dispRow == "Yes" & input$dispCol=='Yes') {
          hm <- heatmap.2(z[[1]], scale="none", Rowv=eval(parse(text=paste(input$clust_byrow))), Colv=eval(parse(text=paste(input$clust_bycol))), hclust=function(c) {hclust(c,method=input$hclust)}, distfun=function(c) {dist(c,method=input$dist)},cexRow=input$size1,cexCol =input$size2,key=TRUE,keysize=1.0, margin = c(input$inSlider2[1],input$inSlider2[2]), density.info=c("none"),trace=c("none"),col=col1,ColSideColors=cc1, RowSideColors = cc2)
        }
        
        if(number.col.groups==1) {
          legend("topright", legend = paste(col.groups.name), col = "pink", lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==2) {
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("pink", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==3) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("pink", "purple", "blue"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==4) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("pink", "purple","blue", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==5) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("pink", "purple","blue", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==6) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==7) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==8) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==9) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        } else if(number.col.groups==10) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
        }
      } 
      
     coldendo <- reactive({
        par(cex = input$sizeClable)
        dend1 <- as.dendrogram(hm$colDendrogram)
        d <- data.frame(v1 =hm$colInd, v2=1:length(hm$colInd))
        m <- data.frame(v3 = 1:length(cc1), v4 = cc1)
        
        colbar <- data.frame(v1=d$v2, v4=m[match(d$v1, m$v3), 2])
        colbar <- colbar[,2]
        labels_colors(dend1) <- as.character(colbar)
        plot(dend1)
        
        if(number.col.groups==1) {
          legend("topright", legend = paste(col.groups.name), col = "pink", lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==2) {
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("pink", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==3) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("pink", "purple", "blue"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==4) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("pink", "purple","blue", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==5) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("pink", "purple","blue", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==6) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==7) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==8) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        } else if(number.col.groups==9) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable9)
        } else if(number.col.groups==10) {  
          legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
        }
      })
      
      output$plot1 <- renderPlot({
        coldendo()
      })
      
      
      colDen <- reactive({
        if(input$cutcolden == 'TRUE') {
          cuttable <- as.data.frame(cutree(as.hclust(hm$colDendrogram), k=as.numeric(input$cuttree))[as.hclust(hm$colDendrogram)$order])
          cuttable <- cbind.data.frame(rownames(cuttable), cuttable)
          names(cuttable)[1] <- "Sample"
          names(cuttable)[2] <- "Cluster"
          data_l1_l2 <- data_input()
          data_l1_l2 <- data_l1_l2[1,c(-1, -2)]
          t_data_l1_l2 <- t(data_l1_l2)
          t_data_l1_l2 <- cbind.data.frame(rownames(t_data_l1_l2), t_data_l1_l2)
          names(t_data_l1_l2)[1] <- "Sample"
          names(t_data_l1_l2)[2] <- "Group"
          m.cut.data <- merge(cuttable, t_data_l1_l2, by = "Sample", sort= F)
          m.cut.data <- m.cut.data[, c(1, 3, 2)]
        }
        else {
          return(NULL)
        } 
        
      })
      
      output$display <- renderUI({
        if(input$cutcolden != 'TRUE') {
          return(br(strong(em("Please select Cut Col dendrogram?: = 'Yes' to display column clusters. Also select value at which you would like to cut the col dendogram (default is at k= 2)"))))
        }
      })
      
      output$df <- DT::renderDataTable({
        if(input$cutcolden == 'TRUE') {
          DT::datatable(colDen(), options = list(
            lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
            pageLength = 5))
        }
      })
      
      output$downloadCuttree <- downloadHandler(
        filename = function() {
          paste(paste(input$fname, input$hclust, "clustering", input$dist, "distance", sep="_"), '_Col_Dendrogram_cutree_', 'k=', input$cuttree, '.csv', sep='') 
        },
        content = function(con) {
          write.csv(colDen(), con, quote=F, row.names = F)
        })
      
      output$pv <- renderUI({
        if(input$cutcolden == 'TRUE' & number.col.groups >= 2 ){
          HTML(paste("<br/>", br(strong(em(paste("Would you want to assess gene set significance in the separation of specimens into two clusters? (Yes/No)")))), sep = "<br>")) 
        }
        else 
          return(NULL)
      })
      
     
      output$pvalue <- renderUI({
        input$goButton
        
        isolate(
         
        if(input$cutcolden == 'TRUE') {
          pobs.col <- numeric()
          perms.col <- numeric()
          hc.cols <- as.hclust(hm$colDendrogram)
          cut <- as.data.frame(cutree(hc.cols, k=as.numeric(input$cuttree))[hc.cols$order])
          #cuttable <- cut_table()
          cut <- cbind.data.frame(rownames(cut), cut)
          names(cut)[1] <- "Sample"
          names(cut)[2] <- "Cluster"
          data_l1_l2 <- data_input()
          data_l1_l2 <- data_l1_l2[1,c(-1, -2)]
          t_data_l1_l2 <- t(data_l1_l2)
          t_data_l1_l2 <- cbind.data.frame(rownames(t_data_l1_l2), t_data_l1_l2)
          names(t_data_l1_l2)[1] <- "Sample"
          names(t_data_l1_l2)[2] <- "Group"
          mer <- merge(cut,t_data_l1_l2, by = "Sample")
          mer <- mer[, c(1, 3, 2)]
          
          if(input$pvalue_cal == TRUE) 
          {
            if(input$file3 == 'Meth.Example'){
              s_data <- readRDS("Meth27K.GW.BRCA.Example.data.rds")
            }
            else {
              inFile2 <- input$file4
              if (is.null(inFile2))
                return(NULL)
              else if(grepl(".xlsx", inFile2[1])) { s_data = read.xlsx(as.character(inFile2$datapath), colNames = TRUE, rowNames = F) }
              else if(grepl(".csv", inFile2[1])) { s_data = read.csv(as.character(inFile2$datapath), header = TRUE, sep = ",", stringsAsFactors = F) }
              else if(grepl(".txt", inFile2[1])) { s_data = read.table(as.character(inFile2$datapath), header = TRUE, sep = "\t", stringsAsFactors = F) }
            }
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Computing data", value = 0)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            updateProgress <- function(value = NULL, detail = NULL) {
              if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 10
              }
              progress$set(value = value, detail = detail)
            }
            
            
            
            # Bootstrap data, and pass in the updateProgress function so that it can update the progress indicator.
            b1 <<- bootstrapfun(obsdata=mer, samplingdata=s_data, distmethod = input$dist, clustmethod= input$hclust, scale=input$norm, n=as.numeric(input$n), k=as.numeric(input$cuttree), n.iter=input$n_iter, zlim=c(input$inSlider[1],input$inSlider[2]), sampler = "Column", updateProgress )
           
            hstring1 <- paste("&emsp;")
            hstring2 <- paste("The p-value to test the gene set significance in the separation of specimens into 2 clusters is =", b1$p.value, sep = " ")
            if(b1$p.value <= 0.05) {
              hstring3  <- paste("The gene set cluster is statistically significant, i.e., a random sample of CpG probes/gene sets of the same number is Not able to separate the specimens when compared to the CpG probes/gene sets of interest of the same class")
             } else {
              hstring3  <- paste("The gene set cluster is NOT statistically significant, i.e., a random sample of CpG probes/gene sets of the same number is able to separate the specimens when compared to the CpG probes/gene sets of interest of the same class")
           }
            
            HTML(paste(hstring1, h5(strong(hstring2)), h5(em(hstring3)), hstring1, hstring1,  sep = '<br/>'))
            
            
          }
        }
        
        )
        
       
      })
      
     
      
      output$pvalplot1 <- renderPlot({
        input$goButton
        
        isolate(
       
        if(input$cutcolden == TRUE & input$pvalue_cal == TRUE )
        {
        hist(b1$perms, main = "Histogram", xlab= paste("p-values from", input$n_iter, "permutations", sep = " "))
        abline(v=b1$p.obs, col ="red", lty=3) 
        
        }
        else
          return(NULL)
        )
        
        })
      
      
      
        rowdendo <- reactive({
        par(cex = input$sizeRlable)
        dend2 <- as.dendrogram(hm$rowDendrogram)
        dd <- data.frame(v1 =hm$rowInd, v2=1:length(hm$rowInd))
        mm <- data.frame(v3 = 1:length(cc2), v4 = cc2)
        
        colbar2 <- data.frame(v1=dd$v2, v4=mm[match(dd$v1, mm$v3), 2])
        colbar2 <- colbar2[,2]
        labels_colors(dend2) <- as.character(colbar2)
        plot(dend2, horiz = T)
        if(number.row.groups==1) {
          legend("topright", legend = paste(row.groups.name), col = "grey", lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } else if(number.row.groups==2) {
          legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2])), col = c("grey", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } else if(number.row.groups==3) {  
          legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3])), col = c("grey", "orange", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } else if(number.row.groups==4) {  
          legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4])), col = c("grey", "orange", "hotpink", "gray"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } else if(number.row.groups==5) {  
          legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4], row.groups.name[5])), col = c("grey", "orange", "hotpink", "gray", "cyan"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } else if(number.row.groups==6) {  
          legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4], row.groups.name[5], row.groups.name[6])), col = c("grey", "orange", "hotpink", "gray", "cyan", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeRlable)
        } 
      })
      
      output$plot2 <- renderPlot({
        par(cex= input$sizeRlable)
        rowdendo()
      })
      
      rowDen <- reactive({
        if(input$cutrowden == 'TRUE') {
          cuttable2 <- as.data.frame(cutree(as.hclust(hm$rowDendrogram), k=as.numeric(input$cuttree2))[as.hclust(hm$rowDendrogram)$order])
          cuttable2 <- cbind.data.frame(rownames(cuttable2), cuttable2)
          names(cuttable2)[1] <- "gene_id"
          names(cuttable2)[2] <- "Cluster"
          data_l1_l2_2 <- data_input()
          data_l1_l2_2 <- data_l1_l2_2[-1, c(1,2)]
          m2 <- merge(cuttable2, data_l1_l2_2, by = "gene_id", sort= F)
          m2 <- m2[, c(1, 3, 2)]
        }
        else {
          return(NULL)
        }
        
      })
      
      output$display2 <- renderUI({
        if(input$cutrowden != 'TRUE') {
          return(br(strong(em("Please select Cut Row dendrogram?: = 'Yes' to display row clusters. Also select value at which you would like to cut the row dendrogram (default is at k= 2)"))))
        }
      })
      
      output$df2 <- DT::renderDataTable({
        if(input$cutrowden == 'TRUE') {
          DT::datatable(rowDen(), options = list(
            lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
            pageLength = 5))
        }
      })
      
      output$downloadCuttree2 <- downloadHandler(
        filename = function() {
          paste(paste(input$fname, input$hclust, "clustering", input$dist, "distance", sep="_"), '_Row_Dendrogram_cutree_', 'k=', input$cuttree2, '.csv', sep='') 
        },
        content = function(con) {
          write.csv(rowDen(), con, quote=F, row.names = F)
        })
      
      output$pv2 <- renderUI({
        if(input$cutrowden == 'TRUE' & number.row.groups >= 2 ){
          HTML(paste("<br/>", paste("Would you want to assess significance of patients in the separation of genes into two clusters? (Yes/No)"), sep = "<br>")) 
        }
        else 
          return(NULL)
      })
      
      
      output$pvalue2 <- renderUI ({
        input$goButton2
        
        isolate(
        if(input$cutrowden == 'TRUE') {
          m2 <- rowDen()
          
          if(input$pvalue_cal2 == TRUE) 
          {
            if(input$file3 == 'Meth.Example'){
              s_data <- readRDS("Meth27K.GW.BRCA.Example.data.rds")
            }
            else {
              inFile2 <- input$file4
              if (is.null(inFile2))
                return(NULL)
              else if(grepl(".xlsx", inFile2[1])) { s_data = read.xlsx(as.character(inFile2$datapath), colNames = TRUE, rowNames = F) }
              else if(grepl(".csv", inFile2[1])) { s_data = read.csv(as.character(inFile2$datapath), header = TRUE, sep = ",", stringsAsFactors = F) }
              else if(grepl(".txt", inFile2[1])) { s_data = read.table(as.character(inFile2$datapath), header = TRUE, sep = "\t", stringsAsFactors = F) }
            }
            
            # Create a Progress object
            progress <- shiny::Progress$new()
            progress$set(message = "Computing data", value = 0)
            # Close the progress when this reactive exits (even if there's an error)
            on.exit(progress$close())
            
            updateProgress <- function(value = NULL, detail = NULL) {
              if (is.null(value)) {
                value <- progress$getValue()
                value <- value + (progress$getMax() - value) / 10
              }
              progress$set(value = value, detail = detail)
            }
            
            # Bootstrap data, and pass in the updateProgress function so that it can update the progress indicator.
            b2 <<- bootstrapfun(obsdata=m2, samplingdata=s_data, distmethod = input$dist, clustmethod= input$hclust, scale=input$norm, n=as.numeric(input$n), k=as.numeric(input$cuttree), n.iter=input$n_iter, zlim=c(input$inSlider[1],input$inSlider[2]), sampler = "Row", updateProgress )
          
            rhstring1 <- paste("&emsp;")
            rhstring2 <- paste("The p-value to test the sample significance in the separation of sample into 2 clusters is = =", em(b2$p.value), sep = " ")
            if(b2$p.value <= 0.05) {
              hstring3  <- paste("The cluster is statistically significant, i.e., a random sample of Sample sets of the same number is Not able to separate the gene sets when compared to the samples of interest of the same class")
            } else {
              hstring3  <- paste("The cluster is NOT statistically significant, i.e., a random sample of Sample sets of the same number is able to separate the gene sets when compared to the samples of interest of the same class")
            }
            
            HTML(paste(rhstring1, h5(strong(rhstring2)), h5(em(rhstring3)), rhstring1, rhstring1,  sep = '<br/>'))
            
            
            
          }
          
        
        }
        
       )
      })
      
      output$pvalueplot2 <- renderPlot({
        input$goButton2
        
        isolate(
        if(input$cutrowden == TRUE & input$pvalue_cal2 == TRUE )
        {
        hist(b2$perms)
        abline(v= b2$p.obs)
        }
        else
          return(NULL)
        )
      })
      
     
      
      ############################
      # Download plots  #
      ############################
      output$downloadPlots <- downloadHandler(
        
        filename <- function() {
          pdf_file <<- paste(input$fname, input$hclust, "clustering", input$dist, "distance", sep="_")
          paste('CASH_', pdf_file, Sys.time(),'.pdf', sep='')
        },
        content <- function(file) {
          pdf(file=paste(pdf_file,".pdf",sep="") , height= 10, width=10)
          df <- rbind.data.frame(c("Data Normalization Type", input$norm),
                                 c("Distance Method", input$dist),
                                 c("Clustering Method", input$hclust),
                                 c("Scale", paste(input$inSlider[1], input$inSlider[2], sep=":")),
                                 c("HeatMap colors", paste(input$low, input$mid, input$high, sep="-")))
          names(df)[1] <- "Parameters"
          names(df)[2] <- "Value Selected"
          grid.table(df, rows= NULL)
          
          eval(hm$call) # call the heatmap here
          if(number.col.groups==1) {
            legend("topright", legend = paste(col.groups.name), col = "pink", lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==2) {
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("pink", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==3) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("pink", "purple", "blue"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==4) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("pink", "purple","blue", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==5) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("pink", "purple","blue", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==6) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==7) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6],  col.groups.name[7])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==8) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6],  col.groups.name[7], col.groups.name[8] )), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==9) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6],  col.groups.name[7], col.groups.name[8], col.groups.name[9] )), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          } else if(number.col.groups==10) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6],  col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = 0.9)
          }
          
          par(cex = 0.6*input$sizeClable)
          #par(mar=c(5,7,4,2))
          dend1 <- as.dendrogram(hm$colDendrogram)
          d <- data.frame(v1 =hm$colInd, v2=1:length(hm$colInd))
          m <- data.frame(v3 = 1:length(cc1), v4 = cc1)
          
          colbar <- data.frame(v1=d$v2, v4=m[match(d$v1, m$v3), 2])
          colbar <- colbar[,2]
          labels_colors(dend1) <- as.character(colbar)
          plot(dend1, main="Column Dendrogram")
          if(number.col.groups==1) {
            legend("topright", legend = paste(col.groups.name), col = "pink", lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==2) {
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2])), col = c("pink", "purple"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==3) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3])), col = c("pink", "purple", "blue"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==4) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4])), col = c("pink", "purple","blue", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 2*input$sizeClable)
          } else if(number.col.groups==5) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5])), col = c("pink", "purple","blue", "orange", "yellow"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==6) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==7) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==8) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==9) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          } else if(number.col.groups==10) {  
            legend("topright", legend = paste(c(col.groups.name[1], col.groups.name[2], col.groups.name[3], col.groups.name[4], col.groups.name[5], col.groups.name[6], col.groups.name[7], col.groups.name[8], col.groups.name[9], col.groups.name[10])), col = c("pink", "purple","blue", "orange", "yellow", "darkgreen", "hotpink", "brown", "darkorchid2", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeClable)
          }
          
          par(cex = input$sizeRlable)
          dend2 <- as.dendrogram(hm$rowDendrogram)
          dd <- data.frame(v1 =hm$rowInd, v2=1:length(hm$rowInd))
          mm <- data.frame(v3 = 1:length(cc2), v4 = cc2)
          
          colbar2 <- data.frame(v1=dd$v2, v4=mm[match(dd$v1, mm$v3), 2])
          colbar2 <- colbar2[,2]
          labels_colors(dend2) <- as.character(colbar2)
          plot(dend2, horiz = T, main="Row Dendrogram")
          if(number.row.groups==1) {
            legend("topright", legend = paste(row.groups.name), col = "grey", lty= 1, lwd = 10, pt.cex = 1, cex = 3*input$sizeRlable)
          } else if(number.row.groups==2) {
            legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2])), col = c("grey", "orange"), lty= 1, lwd = 10, pt.cex = 1, cex = 3*input$sizeRlable)
          } else if(number.row.groups==3) {  
            legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3])), col = c("grey", "orange", "hotpink"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeRlable)
          } else if(number.row.groups==4) {  
            legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4])), col = c("grey", "orange", "hotpink", "gray"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeRlable)
          } else if(number.row.groups==5) {  
            legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4], row.groups.name[5])), col = c("grey", "orange", "hotpink", "gray", "cyan"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeRlable)
          } else if(number.row.groups==6) {  
            legend("topright", legend = paste(c(row.groups.name[1], row.groups.name[2], row.groups.name[3], row.groups.name[4], row.groups.name[5], row.groups.name[6])), col = c("grey", "orange", "hotpink", "gray", "cyan", "maroon"), lty= 1, lwd = 10, pt.cex = 1, cex = input$sizeRlable)
          } 
          
          if(input$goButton) {
            hist(b1$perms, main = "Gene set significance in separation of Clusters", xlab= paste("p-values from", input$n_iter, "permutations", sep = " "))
            abline(v=b1$p.obs, col ="red", lty=3, lwd = 3)
          }
          
          if(input$goButton2 & exists("b2", envir= environment()) ) {
            hist(b2$perms, main = "Cluster significance in separation of gene sets ", xlab= paste("p-values from", input$n_iter, "permutations", sep = " "))
            abline(v=b2$p.obs, col ="blue", lty=3)
          }
          dev.off()
          
         file.copy(paste(pdf_file,'.pdf', sep='') ,file, overwrite=TRUE)
        }
      )
      
    } else {
      return(NULL)
    }
  })
    
  
}

shinyApp(ui= ui, server= server)