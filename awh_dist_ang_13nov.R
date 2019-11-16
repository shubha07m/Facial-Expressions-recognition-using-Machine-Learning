setwd("C:/Users/sd98d/Documents/PHD STUDIES/CS_ISL/project/pro_GFE")
##########

awh=read.table("a_wh_question_datapoints.txt")
#fix(awh)
face=awh[-1,-1]
tgt=read.table("a_wh_question_targets.txt")
#fix(tgt)

tf=nrow(face)
np=ncol(face)

xmin=min(as.matrix(face[,seq(1,np,3)]))
ymin=min(as.matrix(face[,seq(2,np,3)]))
zmin=min(as.matrix(face[,seq(3,np,3)]))

xmax=max(as.matrix(face[,seq(1,np,3)]))
ymax=max(as.matrix(face[,seq(2,np,3)]))
zmax=max(as.matrix(face[,seq(3,np,3)]))

xlm=c(as.numeric(xmin),as.numeric(xmax))
ylm=c(as.numeric(ymax),as.numeric(ymin))


#############
#library(zoom)
for(k in 1)
{
xc=as.matrix(face[k,seq(1,np,3)])
yc=as.matrix(face[k,seq(2,np,3)])
zc=as.matrix(face[k,seq(3,np,3)])
#png(filename=paste('time_',k,'.png',sep=''))
#plot(cbind(xc[1:7],xc[1]), cbind(yc[1:7],yc[1]), pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k),type="o",xlab="Pixel_X",ylab="Pixel_Y")
#par(new=TRUE)
#plot(cbind(xc[8:15],xc[8]), cbind(yc[8:15],yc[8]), pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k),type="o",xlab="Pixel_X",ylab="Pixel_Y")

plot(xc, yc, asp=1,pch="+",xlim=xlm,ylim=ylm, main=paste("time frame : ", k),xlab="Pixel_X",ylab="Pixel_Y")
text(as.numeric(xc),as.numeric(yc), labels = seq(0,99,1), pos = 4,offset = 0.5,cex = 1, col = "red")
#zm()
#, adj = NULL,pos = NULL, offset = 0.5, vfont = NULL,cex = 1, col = NULL, font = NULL, ...)
#dev.off()      
}

#system("convert -delay 80 *.png a_assertive.gif")

on.off=c(sum(tgt),nrow(tgt)-sum(tgt))
on.off


