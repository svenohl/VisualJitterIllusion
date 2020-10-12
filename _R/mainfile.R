#------------------------------------------
# Function to generate random noise images
#
# by Sven Ohl
# Octobe 2020
#------------------------------------------

#------------------------------------------
# Input:
# Size 
# Ratio (kind of spatial frequency)
# Shape (1=square, 2= circle)
# Location
# Number of images

# save as png
# gradual change of color
#------------------------------------------

#------------------------------------------
# load libraries
#library(ggplot2)
#library(imager) #themeCollapse
#library(tuneR)#noise
#pn <- noise(kind=c("pink"))
#pm <- matrix(pn@left,ncol=300)


# paths
path_wd <- getwd()
path_images <- paste(path_wd,"/_images/",sep="")
#-----------------------------------------

#------------------------------------------
# global variables
screen.x <- 20
screen.y <- 20
screen.ratio <- 100

center.x <- round(screen.x/2)
center.y <- round(screen.y/2)

dyn.size <- 6
lastimage <- 10
#------------------------------------------

#------------------------------------------
Row_start <- screen.x/2-dyn.size/2
Row_end   <- screen.x/2+dyn.size/2
Col_start <- screen.y/2-dyn.size/2
Col_end   <- screen.y/2+dyn.size/2

recidx = matrix(seq(Row_start,Row_end,1),nrow=(1+Row_end-Row_start),ncol=(1+Col_end-Col_start),byrow = T) + matrix(screen.y*(seq(Col_start-1,Col_end-1,1)),nrow=(1+Row_end-Row_start),ncol=(1+Col_end-Col_start),byrow = F)
#------------------------------------------

#------------------------------------------
# define static display
display <- matrix(ifelse(runif(screen.x*screen.y)<1.5,0,0.5),nrow=screen.x,ncol=screen.y)
#display <- matrix((runif(screen.x*screen.y,0,1)),nrow=screen.x,ncol=screen.y)
#bk <- display
#image(display,col=grey.colors(2,0,1))

# for loop
for (i in 1:lastimage){

# create dynamic changes
#display[recidx] <- runif(length(recidx))
#------------------------------------------

#------------------------------------------
# save as png
if (i <= lastimage/2){
  # output name
  if(i<10){outputname <- paste("img00",i,sep="")}
  if(i>=10 & i<100){outputname <- paste("img0",i,sep="")}
  if(i>=100){outputname <- paste("img",i,sep="")}

  png(filename=paste(outputname,".png",sep=""),width=screen.x*screen.ratio,height=screen.y*screen.ratio)
  par(mar=c(0,0,0,0))
  image(display,col=gray.colors(100,0,1))
  dev.off()
  }
  
if (i > lastimage/2){
    # update only gradual color changes
    display[recidx] <- display[recidx] + 0.2

    if(i<10){outputname <- paste("img00",i,sep="")}
    if(i>=10 & i<100){outputname <- paste("img0",i,sep="")}
    if(i>=100){outputname <- paste("img",i,sep="")}

    png(filename=paste(outputname,".png",sep=""),width=screen.x*screen.ratio,height=screen.y*screen.ratio)
    par(mar=c(0,0,0,0))
    image(display,col=grey.colors(n=100,start=0.5,end=1))
    dev.off()
}
}
#------------------------------------------

#------------------------------------------
# convert pngs to one gif using ImageMagick
system("convert -delay 8 *.png example_4.gif")
#file.remove(list.files(pattern=".png"))
#------------------------------------------