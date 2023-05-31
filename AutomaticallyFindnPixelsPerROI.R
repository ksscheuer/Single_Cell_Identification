mydat <- read.table('ROIs_1_to_95.dat')

nrois <- mydat[1,1] #first line in dat file gives total number of ROIs

pixelsperroi <- data.frame(matrix(nrow=nrois,ncol=2))
colnames(pixelsperroi) <- c("ROI","nPixels")
pixelsperroi$ROI <- 1:nrois

for (i in 1:nrois) {
  roinumber <- i-1 #dat files number rois starting with 0, so first roi is indicated by 0
  if (1 %in% diff(which(mydat[,1]==roinumber))) { #if three consecutive rows have same number 
                                                  #ie row 11 = 2, row 12 = 2, and row 13 = 2 
                                                  #meaning that ROI 3 consists of 1 pixel
    roistart <- which(mydat[,1]==roinumber)[which(diff(which(mydat[,1]==roinumber))==1)][1] 
                                                               #first instance of difference of 1 
                                                               #starts block of rows corresponding 
                                                               #to given roi ie row 11 in example above
    roinpixels <- mydat[roistart+1,1]-1 #row immediately after row beginning blcok corresponding to 
                                        #given roi is 1+number pixels in that roi
  }
  else {
    roistart <- which(mydat[,1]==roinumber)[which(diff(which(mydat[,1]==roinumber))==2)][1] 
                                                          #first instance of difference of 2
                                                          #starts block of rows corresponding to given roi
    roinpixels <- mydat[roistart+1,1]-1 #row immediately after row beginning blcok corresponding to 
    #given roi is 1+number pixels in that roi
  }
  pixelsperroi$nPixels[i] <- roinpixels
}

write.table(pixelsperroi,"nPixels_.txt",sep="\t",col.names = F,row.names = F)
