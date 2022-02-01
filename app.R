#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

######Load necessary packages first: ##############
library(shiny)
library(RNifti)
library(shinythemes)
library(shinyWidgets)
library(neurobase)
#library(stringr)
library(tidyverse)
#library(ggplot2)
#library(plotly)
library(sortable)


#####Limits######
max_file_size = 30
options(shiny.maxRequestSize = max_file_size*1024^2) #allow max _ * 1024^2 MB/files


####Functions####
get_full_ext <- function(path) {
    str_extract(path,"(?<=\\.).*")
}


get_nif_path <- function(datapath) {
#this function changes extension of temp file from .gz to .nii.gz    
    
    if (get_full_ext(datapath)=="gz"){
        
        newdatapath <- sub("gz$", "nii.gz", datapath)
        file.copy(datapath, newdatapath)
        datapath <- newdatapath
        
    } 
    nif <- RNifti::readNifti(datapath)

}

rndr_nif_slice <- function(path,slice) {
    outfile <- tempfile(fileext='.png')
    png(outfile, width = 600, height = 500)
    img <- RNifti::readNifti(path)
    img <- dropEmptyImageDimensions(img)
    image(img, z=slice, plot.type = "single")
    dev.off()
    list(src=outfile)
}


calc_vol <- function(img) {
    sm <- sum(img!=0)
    pix <- slot(img, "pixdim")
    vol <- (sm*pix[1]*pix[2]*pix[3])/1000
  
    return(vol)
    
}

calc_reg <- function(img) {
  
  if(max(img)>1){
    nc <- sum(img==1)
    edema <- sum(img==2)
    ne <- sum(img==3)
    ec <- sum(img==4)
  }
  else {
    nc <- NA
    edema <- NA
    ne <- NA
    ec <- NA
  }
  
  list(nc=nc, edema=edema, ne=ne, ec=ec)
}

calc_dims <- function(arr) {
    pix <- slot(arr, "pixdim")
    d <- dim(arr)
    
    z_dim = 0
    for (row in 1:d[1]) {
        for (col in 1:d[2]){
            idx = which(arr[row,col,]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > z_dim) {z_dim <- rw}}}
    z_dim = z_dim*pix[3]
    
    y_dim=0
    for (row in 1:d[1]) {
        for (slice in 1:d[3]){
            idx = which(arr[row,,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > y_dim) {y_dim <- rw}}}
    y_dim = y_dim*pix[2]
    
    x_dim=0
    for (col in 1:d[2]) {
        for (slice in 1:d[3]){
            idx = which(arr[,col,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > x_dim) {x_dim <- rw}}}
    x_dim = x_dim*pix[1]
    
    
    list(x_dim=x_dim, y_dim=y_dim, z_dim=z_dim)
}





############ Define UI for application ##################
ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel(title="Quantitative Brain Lesion Characteristic Exploration"),
                sidebarLayout(
                    sidebarPanel(width=3,
                                 
                                 fileInput("segin", "Upload cross-sectional segmentations as .nii or .nii.gz", multiple=TRUE, placeholder = "default data"),
                                 radioButtons("defdata","Or choose a default dataset",choices=c("Longitudinal","Cross-sectional"),selected="Longitudinal"),
                                 helpText("The maximum file upload size is", max_file_size, "MB."),
                                 
                                 actionButton("calc", "Calculate"),
                                 br(),
                                 
                                 h5("Mean Segmentation # of pixels:"),
                                 verbatimTextOutput("pix", placeholder=TRUE),
                                 
                                 h5("Mean Segmentation volume [cm^3]:"), #make the mm3 look nicer
                                 verbatimTextOutput("vol",placeholder=TRUE),
                                 
                                 
                    ), #sidebarPanel
                    
                    mainPanel(
                        tabsetPanel(type='tab',
           
                                    tabPanel("Longitudinal analysis",
                                             fluidPage(
                                             
                                                  column(4,htmlOutput("ranklist"),
                                                         
                                                         tags$style(
                                                          HTML("
                                                            .rank-list-container.custom-sortable {
                                                              background-color: black;
                                                            }
                                                            .custom-sortable .rank-list-item {
                                                              background-color: teal;
                                                            }
                                                          ") #html
                                                       ) #tags$style
                                                  ),
                                                  column(8, 
                                                         column(2, 
                                                                br(),
                                                                actionButton("loplotit", "Plot")),
                                                         column(3, selectInput("loplottype", "Select plot type", choices = c("Scatter", "Bar plot"), selected = "Scatter")),
                                                         column(3, selectInput("lodatatype", "Select data type", choices = c("Raw data", "Z score"), selected = "Raw data")),
                                                         column(3, selectInput("loplotvar", "Select variable", choices = c("Mean volume", "Max dimensions"), selected = "Mean volume")),
                                                         
                                                         plotOutput("rankedplot")
                                                         )
                                             
                                             )),
                                    
                                     tabPanel("Cross-sectional analysis", 
                                             fluidPage(
                                               
                                                 column(1, 
                                                        br(),
                                                        actionButton("plotit", "Plot")),
                                                 column(2, selectInput("plottype", "Select plot type", choices = c("Scatter", "Boxplot", "Bar plot"), selected = "Scatter")),
                                                 column(2, selectInput("datatype", "Select data type", choices = c("Raw data", "Z score"), selected = "Raw data")),
                                                 column(2, selectInput("plotvar", "Select variable", choices = c("Mean volume", "Max dimensions"), selected = "Mean volume")),
                                             plotOutput("myplot", width = "100%")
                                             )), #tabPanel + fluidPage  
                                    
                                
                                    
                                     tabPanel("Data summary & Download",
                                             fluidPage(
                                                 column(12, 
                                                        br(),
                                                        helpText("The following data was calculated from the uploaded files:"),
                                                        tableOutput("table")),         
                                                 column(3,
                                                        textInput("downloadname", "Name of file to be saved:", value = "seg_data_analysis",placeholder=TRUE),
                                                        downloadButton('downloadData', 'Download table summary')),
                       
                                             ))
                        ) #tabsetPanel
                    ) #mainPanel
                ) #sidebarLayout
) #fluidPage   


############## Define server logic #######################
server <- function(input, output) {
    
    data <- reactive({ 
        #req(input$segin)
        datapath <- input$segin$datapath
        
        if (is.null(datapath)) {
          if (input$defdata=="Cross-sectional")
        {images <- readRDS(file="./images2.Rda")
        labels <- list("gbm_pat01_seg.nii.gz", "gbm_pat02_seg.nii.gz", "gbm_pat03_seg.nii.gz", "gbm_pat04_seg.nii.gz", "gbm_pat05_seg.nii.gz", "gbm_pat06_seg.nii.gz", "gbm_pat07_seg.nii.gz", "gbm_pat08_seg.nii.gz", "gbm_pat09_seg.nii.gz", "gbm_pat10_seg.nii.gz", "gbm_pat11_seg.nii.gz", "gbm_pat12_seg.nii.gz")}
        
          if (input$defdata=="Longitudinal")
        {images <- readRDS(file="./loimages2.Rda")
        labels <- list("brats_tcia_pat153_0002_seg.nii", "brats_tcia_pat153_0109_seg.nii", "brats_tcia_pat153_0165_seg.nii", "brats_tcia_pat153_0181_seg.nii", "brats_tcia_pat153_0277_seg.nii", "brats_tcia_pat153_0294_seg.nii")}
        }
        
        else {
        
        labels <- strsplit(input$segin$name, " ")
        num_images=length(datapath)
        #images <- vector(mode = "list", length = num_images)
        images <- matrix(nrow=num_images, ncol=9)
        
        for (i in 1:num_images) {
            nif <- get_nif_path(datapath[i])
            #nam <- paste0("seg", i)
            #images[[i]] <- assign(nam,nif)
            
            dims <- calc_dims(nif)
            reg <- calc_reg(nif)
            
            images[i,1] <- sum(nif!=0)
            images[i,2] <- calc_vol(nif)
            images[i,3] <- dims$x_dim
            images[i,4] <- dims$y_dim
            images[i,5] <- dims$z_dim
            images[i,6] <- reg$nc
            images[i,7] <- reg$ec
            images[i,8] <- reg$ne
            images[i,9] <- reg$edema
        }} 
        
        list(imgData=as.matrix(images), path=datapath, labels=labels)
        
    })
    

###### sidebar #############
    
    observeEvent(input$calc, {
        output$pix <- renderText({
            req(input$calc)
            info <- data()
            images <- info$imgData
            mpix <- as.integer(mean(images[,1]))
        })
    })
    
    observeEvent(input$segin, { #becomes empty when new files are uploaded
        output$pix <- renderText({
        })
    })
    
    observeEvent(input$calc, {
        output$vol <- renderText({
            req(input$calc)
            info <- data()
            images <- info$imgData
            mvol <- as.integer(mean(images[,2]))
        })
    })
    
    observeEvent(input$segin, {
        output$vol <- renderText({
        })
    })    
    
    
    
###### Cross-sectional analysis ###########    
    output$myplot <- renderPlot({
        req(input$plotit)
        
        info <- data()
        df <- setNames(data.frame(info$imgData),c("pix","vol", "xdim", "ydim", "zdim"))
        xvar <- 1:nrow(df)
        
        if(input$plotvar == "Max dimensions") {
          
          
          yvar <- df[3:5]
          title <- "Maximum dimensions of segmentations"
          ylab <- "dimension [mm]"
            xdim <- df$xdim
            ydim <- df$ydim
            zdim <- df$zdim
          
            if(input$datatype == "Z score") {
 
            xdim <- (xdim - mean(xdim))/sd(xdim)
            ydim <- (ydim - mean(ydim))/sd(ydim)
            zdim <- (zdim - mean(zdim))/sd(zdim)
            yvar <- data.frame(xdim,ydim,zdim)
            ylab <- "z score"
            title <- paste0("Z score of ", title)}
          
          plot(y=xdim,x=xvar,  col="red", main=title, ylab=ylab, xlab = "subject ID", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
          points(y=ydim, x=xvar, col="green")
          points(y=zdim, x=xvar, col="blue")
          legend("topright", legend = c("xdim", "ydim", "zdim"), pch=1, col=c("red", "green", "blue"))
          axis(1, xvar)
        
          if(input$plottype=="Boxplot") {p  <- boxplot(yvar, main=title,
                                                     ylab=ylab)}
          if(input$plottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=TRUE,legend.text=TRUE, col=c("red","green","blue"),names.arg=1:nrow(df), main=title,
                                                      xlab="subject ID", ylab=ylab)} 
        }
      else {
          yvar <- df$vol
          title <- "Segmentation volumes"
          ylab <- "vol [cm^3]"
          
          if(input$datatype == "Z score") {
            yvar <- (yvar - mean(yvar))/sd(yvar)
            ylab <- "z score"
            title <- paste0("Z score of ", title)}
        
        # p <- ggplot(data = df) + geom_point(mapping = aes(x=1:nrow(df), y=yvar)) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + xlab("segmentation idx")+ylab(ylab)+ 
        #     scale_alpha(guide = 'none')
        
        plot(y = yvar, x=xvar, main=title,xlab="subject ID", ylab=ylab)
        axis(1, xvar)
        
        if(input$plottype=="Boxplot") {p  <- boxplot(yvar, main=title,
                                                      ylab=ylab)}
        if(input$plottype=="Bar plot") {p <-  barplot(yvar, main=title,names.arg=1:nrow(df),
                                                      xlab="subject ID", ylab=ylab)} 
        }
        
        #return(p)
        
    })
##### Longitudinal analysis ############   
    table_data <- reactive({
      info <- data()
      df <- data.frame(cbind(info$labels,info$imgData)) 
      df <- setNames(df, c("Filename", "#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim", "Necrotic core", "Enhancing core", "Non-enhancing core", "Edema")) #%>% 
        #formatRound(c("#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim"), digits=2)
      #df <- apply(df,2,as.character)
      #table_order <- ranklist_data()
    })

    rv <- reactiveValues(data = data.frame())
    
    observe({rv$data <- data()$labels})

        
    output$ranklist <- renderUI({
      
      ranklist <- rank_list(
        text = "Organize data points in time by dragging",
        labels = rv$data,
        input_id = "ranklist",
        class = c("default-sortable", "custom-sortable"),
        )
      
    })
    
    observeEvent(input$ranklist, {
      rnk <- rank(input$ranklist)
      rv$orgdata <- table_data()[rnk,1:ncol(table_data())]
    })
    
    
    output$rankedplot <- renderPlot({
      req(input$loplotit)
      
      info <- data()
      df <- rv$orgdata
      xvar <- 1:nrow(df)
      
      if(input$loplotvar == "Max dimensions") {
        
        
        yvar <- df[4:6]
        title <- "Maximum dimensions of segmentations"
        ylab <- "dimension [mm]"
        xdim <- as.numeric(df$X_dim)
        ydim <- as.numeric(df$Y_dim)
        zdim <- as.numeric(df$Z_dim)
        yvar <- data.frame(xdim,ydim,zdim)
        
        if(input$lodatatype == "Z score") {
          
          xdim <- (xdim - mean(xdim))/sd(xdim)
          ydim <- (ydim - mean(ydim))/sd(ydim)
          zdim <- (zdim - mean(zdim))/sd(zdim)
          yvar <- data.frame(xdim,ydim,zdim)
          ylab <- "z score"
          title <- paste0("Z score of ", title)}
        
        plot(y=xdim,x=xvar,  col="red", main=title, ylab=ylab, xlab = "Time points", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
        points(y=ydim, x=xvar, col="green")
        points(y=zdim, x=xvar, col="blue")
        legend("topright", legend = c("xdim", "ydim", "zdim"), pch=1, col=c("red", "green", "blue"))
        axis(1, xvar)

        if(input$loplottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=TRUE,legend.text=TRUE, col=c("red","green","blue"),names.arg=1:nrow(df), main=title,
                                                     xlab="Time points", ylab=ylab)} 
      }
      else {
        yvar <- as.numeric(df$`Volume[cm^3]`)
        title <- "Segmentation volumes"
        ylab <- "vol [cm^3]"
        
        if(input$lodatatype == "Z score") {
          yvar <- (yvar - mean(yvar))/sd(yvar)
          ylab <- "z score"
          title <- paste0("Z score of ", title)}
        
        # p <- ggplot(data = df) + geom_point(mapping = aes(x=1:nrow(df), y=yvar)) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + xlab("segmentation idx")+ylab(ylab)+ 
        #     scale_alpha(guide = 'none')
        
        plot(y = yvar, x=xvar, main=title,xlab="Time points", ylab=ylab)
        axis(1, xvar)
        
        if(input$loplottype=="Boxplot") {p  <- boxplot(yvar, main=title,
                                                     ylab=ylab)}
        if(input$loplottype=="Bar plot") {p <-  barplot(yvar, main=title,names.arg=1:nrow(df),
                                                      xlab="Time points", ylab=ylab)} 
      }
      
    })
    
    
##### Data summary & Download ###############    
    
    
    output$table <- renderTable({
      return(rv$orgdata)
       })
    
    # output$table <- renderDT(
    # 
    #   input$rank_list_basic,
    #   #expr <-  rank_list_basic,
    #   expr <- table_data(),
    #   #expr <- apply(df,2,as.numeric),
    #   style = "bootstrap",
    #   selection = "single",
    # 
    #   options = list(
    #     pageLength=12,
    #     searching = FALSE,
    #     info = TRUE
    #   )
    # )
    
    output$downloadData <- downloadHandler(
     
        filename = function() {
          paste(input$downloadname,'.csv', sep='')
        },
        content = function(file) {
          write.csv2(apply(rv$orgdata,2,as.character), file)
          #write_csv2(table_data(),file)
        }
      )

    # 
    # output$text <- renderUI({
    #   req(input$submit)
    #   nifImg <- data()
    #   l <- vector(mode = "list", length = input$numpts)
    #   for(n in 1:input$numpts)
    #     {nif <- get_nif_path(nifImg$path[n])
    #     dims <- calc_dims(nif)
    #     nam <- paste0("pt", n)
    #     p <- paste("Pt", n, ": ", as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
    #     l[[n]] <- assign(nam,p)}
    #     HTML(paste(c(l[1:n]), sep = '<br/>'))
    #     })
    
    
    
}#server

########## Run the application ############
shinyApp(ui = ui, server = server)
