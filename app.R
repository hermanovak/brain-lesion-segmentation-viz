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
library(rhandsontable)

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
    sm <- sum(img)
    pix <- slot(img, "pixdim")
    vol <- (sm*pix[1]*pix[2]*pix[3])/1000
    
    return(vol)
    
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
                titlePanel(title="Automatic Image segmentation"),
                sidebarLayout(
                    sidebarPanel(width=3,
                                 
                                 fileInput("segin", "Upload segmentations as .nii or .nii.gz", multiple=TRUE, placeholder = "GBM data as default"), 
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
                                    
                                    tabPanel("Plots", 
                                             fluidPage(
                                               
                                                 column(1, actionBttn("plotit", "Plot")),
                                                 column(2, selectInput("plottype", "Select plot type", choices = c("Scatter", "Boxplot", "Bar plot"), selected = "Scatter")),
                                                 column(2, selectInput("datatype", "Select data type", choices = c("Raw data", "Z score"), selected = "Raw data")),
                                                 column(2, selectInput("plotvar", "Select plotted variable", choices = c("Mean volume", "Max dimensions"), selected = "Mean volume")),
                                             plotOutput("myplot", width = "100%")
                                             )), #tabPanel + fluidPage
                                         
                                     tabPanel("Table",
                                             fluidPage(
                            
                                                 column(4,
                                                        numericInput("numpts", "Select number of time points", value = 2),
                                                       actionButton("submit", "Submit"),
                                                       h5("Maximum dimensions in each direction [mm]:"),
                                                        verbatimTextOutput("dimi", placeholder = TRUE),),
                                                
                                                 column(8, 
                                                        helpText("The following data was calculated from the uploaded files:"),
                                                        tableOutput("table")),
                                                 
                                                 
                                             )),
                                    
                                    tabPanel("Single patient",
                                             fluidPage(
                                             
                                                  column(3,htmlOutput("ranklist"),
                                                         
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
                                                  column(8, plotOutput("rankedplot"))
                                             
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
        
        if (is.null(datapath))
        {images <- readRDS(file="./images.Rda")
        datapath <- c("./defaultImages/gbm_pat01_seg.nii.gz", "./defaultImages/gbm_pat02_seg.nii.gz")
        labels <- list("gbm_pat01_seg.nii.gz", "gbm_pat02_seg.nii.gz", "gbm_pat03_seg.nii.gz", "gbm_pat04_seg.nii.gz", "gbm_pat05_seg.nii.gz", "gbm_pat06_seg.nii.gz", "gbm_pat07_seg.nii.gz", "gbm_pat08_seg.nii.gz", "gbm_pat09_seg.nii.gz", "gbm_pat10_seg.nii.gz", "gbm_pat11_seg.nii.gz", "gbm_pat12_seg.nii.gz")}
        
        else {
        
        labels <- strsplit(input$segin$name, " ")
        num_images=length(datapath)
        #images <- vector(mode = "list", length = num_images)
        images <- matrix(nrow=num_images, ncol=5)
        
        for (i in 1:num_images) {
            nif <- get_nif_path(datapath[i])
            #nam <- paste0("seg", i)
            #images[[i]] <- assign(nam,nif)
            
            dims <- calc_dims(nif)
            
            images[i,1] <- sum(nif)
            images[i,2] <- calc_vol(nif)
            images[i,3] <- dims$x_dim
            images[i,4] <- dims$y_dim
            images[i,5] <- dims$z_dim
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
    
    
    
###### Plots ###########    
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
          
          plot(y=xdim,x=xvar,  col="red", main=title, ylab=ylab, xlab = "segmentation", ylim=c(min(yvar)-0.05*min(yvar),max(yvar)+0.1*max(yvar)))
          points(y=ydim, x=xvar, col="green")
          points(y=zdim, x=xvar, col="blue")
          legend("topright", legend = c("xdim", "ydim", "zdim"), pch=1, col=c("red", "green", "blue"))
          axis(1, xvar)
        
          if(input$plottype=="Boxplot") {p  <- boxplot(yvar, main=title,
                                                     xlab="segmentation", ylab=ylab)}
          if(input$plottype=="Bar plot") {p <- barplot(t(as.matrix(yvar)),beside=TRUE,legend.text=TRUE, col=c("red","green","blue"),names.arg=1:nrow(df), main=title,
                                                      xlab="segmentation", ylab=ylab)} 
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
        
        plot(y = yvar, x=xvar, main=title,xlab="segmentation", ylab=ylab)
        axis(1, xvar)
        
        if(input$plottype=="Boxplot") {p  <- boxplot(yvar, main=title,
                                                      xlab="segmentation", ylab=ylab)}
        if(input$plottype=="Bar plot") {p <-  barplot(yvar, main=title,names.arg=1:nrow(df),
                                                      xlab="segmentation", ylab=ylab)} 
        }
        
        #return(p)
        
    })
##### Single patient ############   
    
    
    output$ranklist <- renderUI({    
      info <- data()
      rank_list_basic <- rank_list(
        text = "Organize data points in time by dragging",
        labels = info$labels,
        input_id = "rank_list_basic",
        class = c("default-sortable", "custom-sortable")
      )
      return(rank_list_basic)

    })
    
    output$rankedplot <- renderPlot({
    
    })
    
    
##### Table ###############    
    # output$dims1 <- renderText({
    #     nifImg <- data()
    #     nif <- get_nif_path(nifImg$path[1])
    #     dims <- calc_dims(nif)
    #     paste(as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
    # })
    # 
    # output$dims2 <- renderText({
    #     nifImg <- data()
    #     nif <- get_nif_path(nifImg$path[2])
    #     dims <- calc_dims(nif)
    #     paste(as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
    #     
    # })
    
    output$dimi <- renderPrint({
        req(input$submit)
        nifImg <- data()
        l <- vector(mode = "list", length = input$numpts)
        for(n in 1:input$numpts)
            {nif <- get_nif_path(nifImg$path[n])
            dims <- calc_dims(nif)
            nam <- paste0("pt", n)
            p <- paste("Pt", n, ": ", as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
            l[[n]] <- assign(nam,p)
            #output <- paste(l[1], l[2],sep = '<br/>')}
            output <- c(l[1:n])
            }
        return(l)
    })
    
    
    output$table <- renderTable({
      info <- data()
      df <- cbind(info$labels,info$imgData)
      df <- setNames(data.frame(df),c("File name", "#Pixels","Volume[cm^3]", "X_dim", "Y_dim", "Z_dim"))
    })
    
    
    
    
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
    
    # output$im1 <- renderImage({
    #     nifImg <- data()
    #     path <- nifImg$path[1]
    #     slice <- input$slice
    #     rndr_nif_slice(path=path,slice=slice)
    # 
    # }, deleteFile = FALSE)
    # 
    # output$im2 <- renderImage({
    #     nifImg <- data()
    #     path <- nifImg$path[2]
    #     slice <- input$slice
    #     rndr_nif_slice(path=path,slice=slice)
    # 
    # }, deleteFile = FALSE)
   
   #  output$dim_change <- renderText({
   #      info <- data()
   #      nif1 <- get_nif_path(info$path[1])
   #      dim1 <- calc_dims(nif1)
   #      nif2 <- get_nif_path(info$path[2])
   #      dim2 <- calc_dims(nif2)
   #      x_diff <- as.integer(dim1$x_dim - dim2$x_dim)
   #      y_diff <- as.integer(dim1$y_dim - dim2$y_dim)
   #      z_diff <- as.integer(dim1$z_dim - dim2$z_dim)
   #      
   #      paste("The dim. change is",x_diff, "in x,", y_diff, "in y and", z_diff, "in z [mm]")
   #  }) 
   #  
   #  
   # output$vol_change <- renderText({
   #     info <- data()
   #     vol <- info$imgData[,2]
   #     vol_change <- as.integer(vol[1] - vol[2])
   #     paste("The volume change is", vol_change, "cm^3")
   # }) 

    
}#server

########## Run the application ############
shinyApp(ui = ui, server = server)
