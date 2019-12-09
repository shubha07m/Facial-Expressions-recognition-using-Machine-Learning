setwd("C:/Users/shubh/Downloads/Shubh_myR_proj/face")

##########

awh=read.table("a_wh_question_datapoints.txt")
#fix(awh)
face=awh[-1,-1]
tgt=read.table("a_wh_question_targets.txt")
#fix(tgt)

############

ayn=read.table("a_yn_question_datapoints.txt")
#fix(ayn)
face=ayn[-1,-1]
tgt=read.table("a_yn_question_targets.txt")
#fix(tgt)

############

adou=read.table("a_doubt_question_datapoints.txt")
#fix(adou)
face=adou[-1,-1]
tgt=read.table("a_doubts_question_targets.txt")
#fix(tgt)

############

atop=read.table("a_topics_datapoints.txt")
#fix(atop)
face=atop[-1,-1]
tgt=read.table("a_topics_targets.txt")
#fix(tgt)


############

aneg=read.table("a_negative_datapoints.txt")
#fix(aneg)
face=aneg[-1,-1]
tgt=read.table("a_negative_targets.txt")
#fix(tgt)


############

aaff=read.table("a_affirmative_datapoints.txt")
#fix(aaff)
face=aaff[-1,-1]

##########

acond=read.table("a_conditional_datapoints.txt")
#fix(acond)
face=acond[-1,-1]

##########


tf=nrow(face)
np=ncol(face)

kk=data.frame()
for(k in 2:tf)
{
  if(tgt[k,1]!=tgt[k-1,1])
  kk=rbind(kk,k)
}

tstart=kk[1,1]
tend=kk[nrow(kk),1]


plot(1:tf,tgt[,1],xlab='time frame',ylab='Silent/Talking',type="h",col='blue')


xmin=min(as.matrix(face[tstart:tend,seq(1,np,3)]))
ymin=min(as.matrix(face[tstart:tend,seq(2,np,3)]))
zmin=min(as.matrix(face[tstart:tend,seq(3,np,3)]))

xmax=max(as.matrix(face[tstart:tend,seq(1,np,3)]))
ymax=max(as.matrix(face[tstart:tend,seq(2,np,3)]))
zmax=max(as.matrix(face[tstart:tend,seq(3,np,3)]))

xlm=c(as.numeric(xmin),as.numeric(xmax))
ylm=c(as.numeric(ymax),as.numeric(ymin))


####### Displaying face movement ######
	
for(k in 1:tf)
{
xc=as.matrix(face[k,seq(1,np,3)])
yc=as.matrix(face[k,seq(2,np,3)])
zc=as.matrix(face[k,seq(3,np,3)])

	if(tgt[k,1]==0)
	{#png(filename=paste('time_',k,'.png',sep=''))
	plot(xc, yc, pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k, " : SILENT"))}
	else
	{#png(filename=paste('time_',k,'.png',sep=''))
	plot(xc, yc, pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k, " : TALKING"))}
}



####### storing .png files for face movement #####	
for(k in 1:tf)
{
xc=as.matrix(face[k,seq(1,np,3)])
yc=as.matrix(face[k,seq(2,np,3)])
zc=as.matrix(face[k,seq(3,np,3)])

	if(tgt[k,1]==0)
	{png(filename=paste('time_',k,'.png',sep=''))
	plot(xc, yc, pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k, " : SILENT"))}
	else
	{png(filename=paste('time_',k,'.png',sep=''))
	plot(xc, yc, pch=20,xlim=xlm,ylim=ylm, main=paste("time frame : ", k, " : TALKING"))}

dev.off()      
}

######################################################################
################### Reduced set of variables(18) from GFE Paper ##################

new.mat=matrix(data=0,nrow=tf,ncol=18,dimnames = list(seq(1:tf),c("d1","d2","d3","d4","d5","d6","d7","d8","d9","d10","d11","a1","a2","a3","a4","a5","a6","a7")))
#fix(new.mat)
for(k in 1:tf)
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
  
  #plot(xc_new, yc_new, asp=1,pch="+",xlim=xlm,ylim=ylm, main=paste("time frame : ", k),xlab="Pixel_X",ylab="Pixel_Y")
  #lines(xc_new, yc_new, asp=1,pch="+",xlim=xlm,ylim=ylm, main=paste("time frame : ", k),xlab="Pixel_X",ylab="Pixel_Y")
  #text((xc_new),(yc_new), labels = c(2,10,17,27,39,44,48,51,54,57,89), pos = 4,offset = 0.5,cex = 1, col = "red")
  
  
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
    
    a.b = ((x2-x1)*(x2-x3) + (y2-y1)*(y2-y3))
    mod.a = sqrt((x2-x1)^2 + (y2-y1)^2)
    mod.b = sqrt((x2-x3)^2 + (y2-y3)^2)
    c = (a.b)/(mod.a*mod.b)
    rad = acos (c)
    a = rad*180/pi
    return (a)
  }
  
  a1 = myangle(48,89,54)
  a2 = myangle(51,54,57)
  a3 = myangle(51,48,57)
  a4 = myangle(89,54,57)
  a5 = myangle(89,48,57)
  a6 = myangle(27,2,10)
  a7 = myangle(17,10,2)
  
  new.mat[k,]=rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,a1,a2,a3,a4,a5,a6,a7)
}

fix(new.mat)
new.mat = cbind(tgt,new.mat)
fix(new.mat)

################### FOR PCA experimentaions##########################

new.mat2=new.mat[kk[1,1]:kk[nrow(kk),1],]
tgt0=as.matrix(which(new.mat2[,1]==0))
tgt1=as.matrix(which(new.mat2[,1]==1))
mat0=new.mat2[tgt0,]
mat1=new.mat2[tgt1,]

rownames(mat0)= paste("S",1:nrow(mat0),sep="")
rownames(mat1)= paste("T",1:nrow(mat1),sep="")
new.mat2=rbind(mat0,mat1)

fix(new.mat2)

pca.mat=new.mat2[,-1]

op = matrix(unlist(pca.mat), ncol = ncol(pca.mat), byrow = TRUE)
op = mapply(op, FUN=as.numeric)
op = matrix(data=op, ncol=ncol(pca.mat), nrow=nrow(pca.mat))

rownames(op)= rownames(pca.mat)
colnames(op)= colnames(pca.mat)

fix(pca.mat)
pca.mat=pca.mat[sample(nrow(pca.mat)),]
fix(pca.mat)



pca = prcomp(op, scale=TRUE)

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])
 

## make a scree plot
pca.var = pca$sdev^2
pca.var.per = round(pca.var/sum(pca.var)*100, 1)
 
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
 
## PCs and variation:
library(ggplot2)
 
pca.data = data.frame(Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_update(plot.title = element_text(hjust = 0.5))+
  theme_update(plot.background = element_rect(colour = "white"))+
  theme_bw()+
  ggtitle("PCA Graph")
  
## get the name of the top 10 predictors (distance or angle) that contribute
## most to pc1.

loading_scores = pca$rotation[,1]
da_scores = abs(loading_scores) ## get the magnitudes
da_score_ranked = sort(da_scores, decreasing=TRUE)
top_10_da = names(da_score_ranked[1:10])

top_10_da 
## show the names of the top 10 predictors with scores (and +/- sign)
pca$rotation[top_10_da,1] 



#########################################

######### Experiement with logistic regression ################

####### Train and test division #########
attach(new.mat)
fix(new.mat)
m=new.mat

train=1:nrow(m)*0.5
m.train = m[train,]
m.test = m[-train,]

myglm1 = glm(V1 ~., family = binomial,data =m.train)
summary(myglm1)
glm.prob = predict(myglm1,m.test, type = "response")
glm.pred = rep(0,nrow(m.test))
glm.pred[glm.prob > .5] = 1
testv1 = m.test$V1
table(glm.pred,testv1)
mean(glm.pred==testv1)

myglm2 = glm(V1 ~ d10+d7+a1+d8+d9+d11+a2+d6+d4+d5, family = binomial,data = m.train)
summary(myglm2)
glm.prob = predict(myglm2,m.test, type = "response")
glm.pred = rep(0,nrow(m.test))
glm.pred[glm.prob > .5] = 1
testv1 = m.test$V1
table(glm.pred,testv1)
mean(glm.pred==testv1)



############### K fold cross validation ##################



library(caret)
attach(new.mat)
#fix(new.mat)
m=new.mat

# Define train control for k fold cross validation
train_control = trainControl(method="cv", number=10)
# Fit Naive Bayes Model
m[,1]=as.factor(m$V1)
#fix(m)
######### Cross Validation with 18 variables ################
model = train(V1 ~., data=m, trControl=train_control, method="glm")
# Summarise Results
print(model)


######### CV with 10 variables chosen by PCA ################
model = train(V1 ~ d10+d7+a1+d8+d9+d11+a2+d6+d4+d5, data=m, trControl=train_control, method="glm")
# Summarise Results
print(model)




###############################################################


########## PCA experiments on 200 X-Y coordinates ##############

new.mat= face[, -seq(3,np,3)]
new.mat = cbind(tgt,new.mat)

new.mat2=new.mat[kk[1,1]:kk[nrow(kk),1],]
tgt0=as.matrix(which(new.mat2[,1]==0))
tgt1=as.matrix(which(new.mat2[,1]==1))
mat0=new.mat2[tgt0,]
mat1=new.mat2[tgt1,]

rownames(mat0)= paste("S",1:nrow(mat0),sep="")
rownames(mat1)= paste("T",1:nrow(mat1),sep="")
new.mat2=rbind(mat0,mat1)

fix(new.mat2)


coln=rep(0,200)
for(i in seq(2,200,2))
{coln[i]=paste("Y",i/2, sep="")
coln[i-1]=c(paste("X",i/2, sep=""))
}

colnames(new.mat2)= c(paste("TS"),coln)
fix(new.mat2)

pca.mat=new.mat2[,-1]
fix(pca.mat)

op = matrix(unlist(pca.mat), ncol = ncol(pca.mat), byrow = TRUE)
op = mapply(op, FUN=as.numeric)
op = matrix(data=op, ncol=ncol(pca.mat), nrow=nrow(pca.mat))

rownames(op)= rownames(pca.mat)
colnames(op)= colnames(pca.mat)
fix(pca.mat)

pca.mat=pca.mat[sample(nrow(pca.mat)),]
fix(pca.mat)


pca = prcomp(op, scale=TRUE)

## plot pc1 and pc2
plot(pca$x[,1], pca$x[,2])
 
## make a scree plot
pca.var = pca$sdev^2
pca.var.per = round(pca.var/sum(pca.var)*100, 1)
 
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
 
## PCs and variation:
library(ggplot2)
 
pca.data = data.frame(Sample=rownames(pca$x),
  X=pca$x[,1],
  Y=pca$x[,2])
pca.data

ggplot(data=pca.data, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_update(plot.title = element_text(hjust = 0.5))+
  theme_update(plot.background = element_rect(colour = "white"))+
  theme_bw()+
  ggtitle("PCA Graph")
  
## get the name of the top 20 predictors (coordinates) that contribute
## most to pc1.

loading_scores = pca$rotation[,1]
coords_scores = abs(loading_scores) ## get the magnitudes
coords_score_ranked = sort(coords_scores, decreasing=TRUE)
top_20_coords = names(coords_score_ranked[1:20])

top_20_coords

## show the names of the top 10 predictors with scores (and +/- sign)
pca$rotation[top_20_coords,1] 

###############################################################



######### Logistic regression ################

####### Validation set approach #########

attach(new.mat2)
# fix(new.mat2)
m=new.mat2[sample(nrow(new.mat2)),]
# fix(m)
attach(m)

train=1:nrow(m)*0.8
m.train = m[train,]
m.test = m[-train,]

myglm1 = glm(TS ~., family = binomial,data =m.train)

summary(myglm1)
glm.prob = predict(myglm1,m.test, type = "response")
glm.pred = rep(0,nrow(m.test))
glm.pred[glm.prob > .5] = 1
testv1 = m.test$V1
table(glm.pred,testv1)
mean(glm.pred==testv1)

myglm2 = glm(V1 ~ d10+d7+a1+d8+d9+d11+a2+d6+d4+d5, family = binomial,data = m.train)
summary(myglm2)
glm.prob = predict(myglm2,m.test, type = "response")
glm.pred = rep(0,nrow(m.test))
glm.pred[glm.prob > .5] = 1
testv1 = m.test$V1
table(glm.pred,testv1)
mean(glm.pred==testv1)





###################################


##########################################

########### Time series experimentation####################

tms=awh[-1,]
fix(tms)
t=tms$V1
tms=cbind(t,new.mat)
fix(tms)
start.time = tms[1,1]


y1 = ts(tms$d10,start = start.time,frequency = 1)
dy1 = diff(y1)

tms1=tms[tgt==1,]
tms0=tms[tgt==0,]

y1 = ts(tms1$d10,start = start.time,frequency = 1)
dy1 = diff(y1)

autoplot(y1) + ggtitle("variable d10 time series when talking") + ylab("Change of d10 when talking")
autoplot(dy1) + ggtitle("variable d10 time series delta when talking") + ylab("Rate of Change for d10 when talking")
 
write.csv(dy1,'dy1_a.csv')
 write.csv(y0,'y0.csv')
 
 par(mfrow=c(2,1))
 hist(y0)
 hist(y1)

 

### Preliminary analysis ###

# pairs((y0),(y1))

# check if stationary
# ggseasonplot(dy)





#### other experiments ###

# abc = read.csv("newface_b_wh.csv")
# y1 = ts(abc[,13],start = 1390384768,frequency = 1)
# autoplot(y1) + ggtitle("variable a1 time series") + ylab("Change of a1")

fix(new.mat)

###################
