setwd("E:/shubh_projects/isl_project")
library(zoom)
##########

awh=read.table("a_wh_question_datapoints.txt")
#fix(awh)
face=awh[-1,-1]
tgt=read.table("a_wh_question_targets.txt")
#fix(tgt)

tf=nrow(face)
np=ncol(face)

for(k in 1)
{
xc=as.matrix(face[k,seq(1,np,3)])
yc=as.matrix(face[k,seq(2,np,3)])
zc=as.matrix(face[k,seq(3,np,3)])

#shubha
xc_new = cbind(as.numeric(xc[3]) , as.numeric(xc[11]) , as.numeric(xc[18]) , as.numeric(xc[28])
               , as.numeric(xc[40]) , as.numeric(xc[45]) , as.numeric(xc[49]) , as.numeric(xc[52])
               , as.numeric(xc[55]) , as.numeric(xc[58]) , as.numeric(xc[90]))

yc_new = cbind(as.numeric(yc[3]) , as.numeric(yc[11]) , as.numeric(yc[18]) , as.numeric(yc[28])
               , as.numeric(yc[40]) , as.numeric(yc[45]) , as.numeric(yc[49]) , as.numeric(yc[52])
               , as.numeric(yc[55]) , as.numeric(yc[58]) , as.numeric(yc[90]))

##new xlim ylim defined

xmin=min(xc_new)
ymin=min(yc_new)

xmax=max(xc_new)
ymax=max(yc_new)

xlm=c(xmin,xmax)
ylm=c(ymax,ymin)

plot(xc_new, yc_new, asp=1,pch="+",xlim=xlm,ylim=ylm, main=paste("time frame : ", k),xlab="Pixel_X",ylab="Pixel_Y")
lines(xc_new, yc_new, asp=1,pch="+",xlim=xlm,ylim=ylm, main=paste("time frame : ", k),xlab="Pixel_X",ylab="Pixel_Y")
text((xc_new),(yc_new), labels = c(2,10,17,27,39,44,48,51,54,57,89), pos = 4,offset = 0.5,cex = 1, col = "red")
}

mydistance = function(i,j) {
  return (((as.numeric(xc[i+1])-as.numeric(xc[j+1]))^2 + (as.numeric(yc[i+1])-as.numeric(yc[j+1]))^2)^.5)
}


d1 = mydistance(2,17)
d2 = mydistance(10,27)
d3 = mydistance(2,89)
d4 = mydistance(10,89)
d5 = mydistance(48,54)
d6 = mydistance(39,89)
d7 = mydistance(44,89)
d8 = mydistance(51,57)
d9 = mydistance(17,27)
d10 = mydistance(39,57)
d11 = mydistance(44,57)


myangle = function(i,j,k) {
  x1 = as.numeric(xc[i+1])
  y1 = as.numeric(yc[i+1])
  x2 = as.numeric(xc[j+1])
  y2 = as.numeric(yc[j+1])
  x3 = as.numeric(xc[k+1])
  y3 = as.numeric(yc[k+1])
  
  a.b = ((x1-x2)*(x3-x2) + (y1-y2)*(y3-y2))
  mod.a = sqrt((x1-x2)^2 + (y1-y2)^2)
  mod.b = sqrt((x3-x2)^2 + (y3-y2)^2)
  c = (a.b)/(mod.a*mod.b)
  rad = acos (c)
  a = rad*180/pi
  return (a)
}

a1 = myangle(48,89,54)
a2 = myangle(51,54,57)
a3 = myangle(51,48,57)
# a4 = myangle(48,89,54)
a5 = myangle(89,48,57)
a6 = myangle(27,4,12)
a7 = myangle(17,12,4)
a8 = myangle(48,89,54)