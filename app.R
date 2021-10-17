#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

#Load necessary packages first:
library(shiny)
#library(RNifti)
#library(RNiftyReg)
library(shinythemes)
library(shinyWidgets)
library(oro.nifti)
#library(EBImage)
library(neurobase)
library(stringr)
library(tidyverse)
library(ggplot2)
library(plotly)

#Limits
max_file_size = 30
options(shiny.maxRequestSize = max_file_size*1024^2) #allow max _ * 1024^2 MB/files

#Functions

####################################################

#replace their readNIfTI so that I can comment out calibrateImage (saves 5 seconds on the 12 images)
readNIfTI <- function (fname, verbose = TRUE, warn = -1, reorient = TRUE, 
          call = NULL, read_data = TRUE, rescale_data = TRUE) 
{
    if (is.null(call)) {
        call <- match.call()
    }
    oldwarn <- getOption("warn")
    options(warn = warn)
    if (verbose) {
        cat(paste("  fname =", fname), fill = TRUE)
    }
    pathVector <- unlist(strsplit(fname, "/"))
    file.name <- pathVector[length(pathVector)]
    path <- paste(pathVector[-length(pathVector)], collapse = "/")
    if (length(pathVector) > 1) {
        fname <- paste(path, file.name, sep = "/")
    }
    else {
        fname <- file.name
    }
    fname <- sub("\\.gz$", "", fname)
    fname <- sub("\\.nii$", "", fname)
    fname <- sub("\\.hdr$", "", fname)
    fname <- sub("\\.img$", "", fname)
    nii <- paste(fname, "nii", sep = ".")
    niigz <- paste(fname, "nii.gz", sep = ".")
    hdr <- paste(fname, "hdr", sep = ".")
    hdrgz <- paste(fname, "hdr.gz", sep = ".")
    img <- paste(fname, "img", sep = ".")
    imggz <- paste(fname, "img.gz", sep = ".")
    args = list(fname, onefile = TRUE, gzipped = TRUE, verbose = verbose, 
                warn = warn, reorient = reorient, call = call, read_data = read_data, 
                rescale_data = rescale_data)
    if (file.exists(niigz)) {
        if (verbose) {
            cat(paste("  files =", niigz), fill = TRUE)
        }
        args$onefile = TRUE
        args$gzipped = TRUE
        nim = do.call(oro.nifti:::.read.nifti.content, args = args)
    }
    else {
        if (file.exists(nii)) {
            if (verbose) {
                cat(paste("  files =", nii), fill = TRUE)
            }
            args$onefile = TRUE
            args$gzipped = FALSE
            nim = do.call(.read.nifti.content, args = args)
        }
        else {
            if (file.exists(hdrgz) && file.exists(imggz)) {
                if (verbose) {
                    cat(paste("  files =", hdrgz, "and", imggz), 
                        fill = TRUE)
                }
                args$onefile = FALSE
                args$gzipped = TRUE
                nim = do.call(.read.nifti.content, args = args)
            }
            else {
                if (file.exists(hdr) && file.exists(img)) {
                    if (verbose) {
                        cat(paste("  files =", hdr, "and", img), 
                            fill = TRUE)
                    }
                    args$onefile = FALSE
                    args$gzipped = FALSE
                    nim = do.call(.read.nifti.content, args = args)
                }
                else {
                    stop("File(s) not found!")
                }
            }
        }
    }
    if (read_data) {
        #nim = calibrateImage(nim, infok = TRUE)
    }
    options(warn = oldwarn)
    return(nim)
}

assignInNamespace("readNIfTI",readNIfTI,ns="oro.nifti") #this tells it to use my function instead of theirs

####################################################

get_full_ext <- function(path) {
    str_extract(path,"(?<=\\.).*")
}


get_nif_path <- function(datapath) {
    
    
    if (get_full_ext(datapath)=="gz"){
        
        newdatapath <- sub("gz$", "nii.gz", datapath)
        file.copy(datapath, newdatapath)
        datapath <- newdatapath
        
    } 
    nif <- readNIfTI(datapath, reorient=FALSE)

}

rndr_nif_slice <- function(path,slice) {
    outfile <- tempfile(fileext='.png')
    png(outfile, width = 600, height = 500)
    img <- readNIfTI(path, reorient=FALSE)
    img <- dropEmptyImageDimensions(img)
    #img <- rotateFixed(img,180) #not valid for nifti
    image(img, z=slice, plot.type = "single")
    dev.off()
    list(src=outfile)
}


sum_pix <- function(img) {
    arr <- slot(img,".Data")
    sm <- as.integer(sum(arr==1))
    return(sm)
}

calc_vol <- function(img) {
    sm <- as.numeric(sum_pix(img))
    pix <- slot(img, "pixdim")
    vol <- as.integer((sm*pix[2]*pix[3]*pix[4])/1000)
    
    return(vol)
    
}

calc_dims <- function(img) {
    arr <- slot(img,".Data")
    pix <- slot(img, "pixdim")
    d <- dim(arr)
    
    z_dim = 0
    for (row in 1:d[1]) {
        for (col in 1:d[2]){
            idx = which(arr[row,col,]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > z_dim) {z_dim <- rw}}}
    z_dim = as.integer(z_dim*pix[4])
    
    y_dim=0
    for (row in 1:d[1]) {
        for (slice in 1:d[3]){
            idx = which(arr[row,,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > y_dim) {y_dim <- rw}}}
    y_dim = as.integer(y_dim*pix[3])
    
    x_dim=0
    for (col in 1:d[2]) {
        for (slice in 1:d[3]){
            idx = which(arr[,col,slice]!=0)
            if (length(idx)==0) {rw=0}
            else {rw = idx[length(idx)] - idx[1]}
            if (rw > x_dim) {x_dim <- rw}}}
    x_dim = as.integer(x_dim*pix[2])
    
    
    list(x_dim=x_dim, y_dim=y_dim, z_dim=z_dim)
}




# # Define UI for application 
ui <- fluidPage(theme = shinytheme("darkly"),
                titlePanel(title="Automatic Image segmentation"),
                sidebarLayout(
                    sidebarPanel(width=3,
                                 
                                 fileInput("segin", "Upload segmentations as .nii or .nii.gz", multiple=TRUE, placeholder = "No Default Yet"), 
                                 helpText("The maximum file upload size is", max_file_size, "MB."),
                                 
                                 br(),
                                 
                                 h5("Mean Segmentation # of pixels:"),
                                 verbatimTextOutput("pix", placeholder=TRUE),
                                 
                                 h5("Mean Segmentation volume [cm^3]:"), #make the mm3 look nicer
                                 verbatimTextOutput("vol",placeholder=TRUE),
                                 
                                 numericInput("slice", "Choose which slice to analyze", value=1, min=1, max=200),
                                 
                                 
                    ), #sidebarPanel
                    
                    mainPanel(
                        tabsetPanel(type='tab',
                                    
                                    tabPanel("Multiple patients", 
                                             
                                             #numericInput("slice", "Choose which slice to analyze", value=11, min=1, max=200), #make dependent on dimensions
                                             actionBttn("plotit", "Plot"),
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


# Define server logic 
server <- function(input, output) {
    
    data <- reactive({ 
        req(input$segin)
        
        datapath <- input$segin$datapath
        
        # if(is.null(datapath))
        # {datapath <- ("./gbm_pat01_seg.nii.gz", "gbm_pat02_seg.nii.gz", "gbm_pat03_seg.nii.gz", "gbm_pat04_seg.nii.gz")} #need to make it a character string of 4 things
                        #now in defaultImages
        
        # if (is.null(datapath))
        # {images <- readRDS(file="~/R/edits2/data.Rda")}
        
        num_images=length(datapath)
        #images <- vector(mode = "list", length = num_images)
        images <- matrix(nrow=num_images, ncol=2)
        
        for (i in 1:num_images) {
            nif <- get_nif_path(datapath[i])
            #nam <- paste0("seg", i)
            #images[[i]] <- assign(nam,nif)
            
            images[i,1] <- sum_pix(nif)
            images[i,2] <- calc_vol(nif)
            
        }
        
        list(imgData=as.matrix(images), path=datapath)
        
    })
    

 
    
    output$pix <- renderText({
        info <- data()
        images <- info$imgData
        mpix <- as.integer(mean(as.integer(images[,1])))

    })
    
    output$vol <- renderText({
        info <- data()
        images <- info$imgData
        mvol <- as.integer(mean(as.integer(images[,2])))
    })
    
    output$dims1 <- renderText({
        nifImg <- data()
        nif <- readNIfTI(nifImg$path[1], reorient=FALSE)
        dims <- calc_dims(nif)
        paste(dims$x_dim, "in x,", dims$y_dim, "in y,", dims$z_dim, "in z")
    })
    
    output$dims2 <- renderText({
        nifImg <- data()
        nif <- readNIfTI(nifImg$path[2], reorient=FALSE)
        dims <- calc_dims(nif)
        paste(dims$x_dim, "in x,", dims$y_dim, "in y,", dims$z_dim, "in z")
        
    })
    
    output$myplot <- renderPlot({
        req(input$segin)
        
        if (input$plotit == TRUE) {
        
        info <- data()
        df <- setNames(data.frame(info$imgData),c("pix","vol"))
        ggplot(data = df) + geom_point(mapping = aes(x=1:nrow(df), y=vol)) + ggtitle("Segmentation volumes") + theme(plot.title = element_text(hjust = 0.5)) + xlab("segmentation index")+ylab("Volume [cm^3]")+ 
            scale_alpha(guide = 'none')}
        
        else {return()}
        
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
        nif1 <- readNIfTI(info$path[1], reorient=FALSE)
        dim1 <- calc_dims(nif1)
        nif2 <- readNIfTI(info$path[2], reorient=FALSE)
        dim2 <- calc_dims(nif2)
        x_diff <- dim1$x_dim - dim2$x_dim
        y_diff <- dim1$y_dim - dim2$y_dim
        z_diff <- dim1$z_dim - dim2$z_dim
        
        paste("The dim. change is",x_diff, "in x,", y_diff, "in y and", z_diff, "in z [mm]")
    }) 
    
    
   output$vol_change <- renderText({
       info <- data()
       vol <- info$imgData[,2]
       vol_change <- vol[1] - vol[2]
       paste("The volume change is", vol_change, "cm^3")
   }) 
    
}

# Run the application 
shinyApp(ui = ui, server = server)
