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
library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)

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
                                 
                                 #numericInput("slice", "Choose which slice to analyze", value=1, min=1, max=200),
                                 
                                 
                    ), #sidebarPanel
                    
                    mainPanel(
                        tabsetPanel(type='tab',
                                    
                                    tabPanel("Multiple patients", 
                                             
                                             #numericInput("slice", "Choose which slice to analyze", value=11, min=1, max=200), #make dependent on dimensions
                                             actionBttn("plotit", "Plot"),selectInput("plottype", "Select plot type", choices = c("Scatter", "Boxplot", "Bar plot"), selected = "Scatter"),
                                             plotOutput("myplot", width = "100%")
                                             ), #tabPanel
                                    tabPanel("Single patient",
                                             fluidRow(
                                                 
                                                 column(width=4, align="center",
                                                        h3("Segmentation #1")),
                                                 column(width=4,align="center",
                                                        h3("Comparison")),
                                                 column(4,align="center",
                                                        h3("Segmentation #2")),
                                                 
                                                 column(4, align="center",
                                                        h5("Maximum dimensions in each direction [mm]:"),
                                                        verbatimTextOutput("dims1", placeholder=TRUE)),
                                                 column(4,align="center",
                                                        h5("Difference between 1st and 2nd segmentation"),
                                                        verbatimTextOutput("dim_change", placeholder=TRUE)),
                                                 column(4,align="center",
                                                        h5("Maximum dimensions in each direction [mm]:"),
                                                        verbatimTextOutput("dims2", placeholder=TRUE)),
                                                 
                                                 column(width=4, offset=4,align="center",
                                                        verbatimTextOutput("vol_change", placeholder = TRUE)),
                                                 
                                                 column(width=6,align="center",
                                                    imageOutput("im1",)
                                                    ),
                                                 column(width=6,align="center",
                                                    imageOutput("im2")
                                                    )
                                                 
                                                 
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
        {images <- readRDS(file="./data.Rda")
        datapath <- c("./defaultImages/gbm_pat01_seg.nii.gz", "./defaultImages/gbm_pat02_seg.nii.gz")}
        
        else {
        num_images=length(datapath)
        #images <- vector(mode = "list", length = num_images)
        images <- matrix(nrow=num_images, ncol=2)
        
        for (i in 1:num_images) {
            nif <- get_nif_path(datapath[i])
            #nam <- paste0("seg", i)
            #images[[i]] <- assign(nam,nif)
            
            images[i,1] <- sum(nif)
            images[i,2] <- calc_vol(nif)
            
            
        }}
        
        list(imgData=as.matrix(images), path=datapath)
        
    })
    


    
    observeEvent(input$calc, {
        output$pix <- renderText({
            req(input$calc)
            info <- data()
            images <- info$imgData
            mpix <- as.integer(mean(images[,1]))
        })
    })
    
    observeEvent(input$segin, {
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
    
    output$dims1 <- renderText({
        nifImg <- data()
        nif <- get_nif_path(nifImg$path[1])
        dims <- calc_dims(nif)
        paste(as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
    })
    
    output$dims2 <- renderText({
        nifImg <- data()
        nif <- get_nif_path(nifImg$path[2])
        dims <- calc_dims(nif)
        paste(as.integer(dims$x_dim), "in x,", as.integer(dims$y_dim), "in y,", as.integer(dims$z_dim), "in z")
        
    })
    
    output$myplot <- renderPlot({
        req(input$plotit)
        
        info <- data()
        df <- setNames(data.frame(info$imgData),c("pix","vol"))
        p <- ggplot(data = df) + geom_point(mapping = aes(x=1:nrow(df), y=vol)) + ggtitle("Segmentation volumes") + theme(plot.title = element_text(hjust = 0.5)) + xlab("segmentation index")+ylab("Volume [cm^3]")+ 
            scale_alpha(guide = 'none')
        
        if(input$plottype=="Boxplot") {p  <- boxplot(df$vol, main="Segmentation volumes",
                                                      xlab="vol [cm^3]")}
        if(input$plottype=="Bar plot") {p <-  barplot(df$vol, main="Segmentation volumes",
                                                      xlab="vol [cm^3]")}
        
        return(p)
        
    })
    
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
   
    output$dim_change <- renderText({
        info <- data()
        nif1 <- get_nif_path(info$path[1])
        dim1 <- calc_dims(nif1)
        nif2 <- get_nif_path(info$path[2])
        dim2 <- calc_dims(nif2)
        x_diff <- as.integer(dim1$x_dim - dim2$x_dim)
        y_diff <- as.integer(dim1$y_dim - dim2$y_dim)
        z_diff <- as.integer(dim1$z_dim - dim2$z_dim)
        
        paste("The dim. change is",x_diff, "in x,", y_diff, "in y and", z_diff, "in z [mm]")
    }) 
    
    
   output$vol_change <- renderText({
       info <- data()
       vol <- info$imgData[,2]
       vol_change <- as.integer(vol[1] - vol[2])
       paste("The volume change is", vol_change, "cm^3")
   }) 
    
}

########## Run the application ############
shinyApp(ui = ui, server = server)
