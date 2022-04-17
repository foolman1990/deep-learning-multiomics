library("glmnet")
library("survival")
library("survminer")
library("survivalROC")
library("ggsci")
library("tidyverse")
library("cowplot")
library("pheatmap")
library("ggplot2")
library("Hmisc")
library("plyr")
library("foreach")
library("doParallel")
library("caret")
library("h2o")
library("e1071")
h2o.init(nthreads = -1,max_mem_size = "8G")
class(subgroup)
subgroup<-as.data.frame(subgroup)
clin_dd_subgroup.df$group<-as.numeric(as.matrix(subgroup))
samp <- createDataPartition(clin_dd_subgroup.df$group, p = 0.6, list = FALSE) 
clin_dd_subgroup.train<-clin_dd_subgroup.df[samp,]
clin_dd_subgroup.test<-clin_dd_subgroup.df[-samp,]

train<-clin_dd_subgroup.train
test<-clin_dd_subgroup.test

cirlasso<-function(train=train,test=test, varlength= 30 ,nloop=1000, unilogis=T, logiscut=0.05, modelcut=0.7,R2= 0.5,BootStrap=F,lambda="min") {
  train<-as.data.frame(train)
  test<-as.data.frame(test)
  if(unilogis==T){
    logis <- function(z){
      fml<-as.formula(paste0("group~",z))
      fit <-glm(fml, family=binomial, data = train)
      Sumfit <- summary(fit)
      OR <- round(exp(coef(Sumfit)[-1,1]),4)
      LCI <- round(exp(confint.default(fit)[-1,1]),4)
      UCI <- round(exp(confint.default(fit)[-1,2]),4)
      CI95 <- paste(LCI,'-',UCI)
      P_Value <- round(Sumfit$coefficients[-1,4],4)
      Unifit <- data.frame(Characteristics = rownames(coef(Sumfit))[-1],
                           Odds_Ratio = as.character(OR),
                           CI95 = CI95,
                           P_Value = P_Value
      )
      return(Unifit)
    }
    
    UniNames <- setdiff(colnames(train), c("group","times","events"))
    library("parallel")
    cl.cores <- detectCores()
    cl <- makeCluster(cl.cores-3)
    clusterExport(cl,c("train","glm"))
    Uni<-parLapply(cl,UniNames,fun  = logis)
    Uni <- ldply(Uni,.parallel = T)
    stopCluster(cl)
    Uni_pvalue<- na.omit(Uni[Uni$P_Value < logiscut,]) 
    filenanme<-paste0("Uni.","logiscut_",logiscut,".Rda")
    save(Uni_pvalue,file = filenanme)
  }else{
    filenanme<-paste0("Uni.","logiscut_",logiscut,".Rda")
    load(filenanme)
  }
  
  dd1<-train[,colnames(train) %in% Uni_pvalue$Characteristics]
  dd2<-test[,colnames(test) %in% Uni_pvalue$Characteristics]
  Train<-data.frame(group=as.numeric(train$group),dd1)
  Test<-data.frame(times=as.numeric(test$times),events=as.numeric(test$events),dd2)
  FUN<-function(d,t,v,r){
    n<-nrow(d)
    res.tArc   <-c()
    res.tBrc   <-c()
    res.testrc <-c()
    res.trainrc<-c()
    genelist   <-vector(mode="character",length=v)
    testrs     <-c()
    trainrs    <-c()
    coef       <-vector(length=v)
    if(BootStrap==T){
      perm <- createResample(d$group, times = 1, list = FALSE)
    }else{
      perm <- createDataPartition(d$group, p = r, list = FALSE) 
    }
    xA<-data.matrix(d[perm,2:length(colnames(d))])
    yA<-data.matrix(d[perm,]$group)
    xB<-data.matrix(d[-perm,2:length(colnames(d))])
    yB<-data.matrix(d[-perm,]$group)
    cv.fitA<-cv.glmnet(xA,yA,type.measure = "class",family = "binomial")
    if(lambda=="min"){
      coeA<-coef(cv.fitA$glmnet.fit,s=cv.fitA$lambda.min)   
    }
    if(lambda=="1se"){
      coeA<-coef(cv.fitA$glmnet.fit,s=cv.fitA$lambda.1se)  
    }
    
    tA.active.coef<-coeA[which(coeA[,1]!=0)]
    tA.name<-row.names(coeA)[which(coeA[,1]!=0)]
    if(length(tA.name)>=3 & length(tA.name)<=v){
      if(lambda=="1se"){
        tA.riskscore <- predict(cv.fitA$glmnet.fit, xA, s=cv.fitA$lambda.1se, type="class")
        tB.riskscore <- predict(cv.fitA$glmnet.fit, xB, s=cv.fitA$lambda.1se, type="class")
        test.riskscore <- predict(cv.fitA$glmnet.fit, as.matrix(t[,3:ncol(t)]), s=cv.fitA$lambda.1se, type="class")
        train.riskscore <- predict(cv.fitA$glmnet.fit, as.matrix(d[,2:ncol(d)]), s=cv.fitA$lambda.1se, type="class")
      }
      if(lambda=="min"){
        tA.riskscore <- predict(cv.fitA$glmnet.fit, xA, s=cv.fitA$lambda.min, type="class")
        tB.riskscore <- predict(cv.fitA$glmnet.fit, xB, s=cv.fitA$lambda.min, type="class")
        test.riskscore <- predict(cv.fitA$glmnet.fit, as.matrix(t[,3:ncol(t)]), s=cv.fitA$lambda.min, type="class")
        train.riskscore <- predict(cv.fitA$glmnet.fit, as.matrix(d[,2:ncol(d)]), s=cv.fitA$lambda.min, type="class")
      }
      
      if(length(levels(as.factor(test.riskscore)))==2){
      tArc<-confusionMatrix(as.factor(tA.riskscore[,1]), as.factor(as.matrix(d[perm,]$group)[,1]))$overall["Accuracy"]
      tBrc<-confusionMatrix(as.factor(tB.riskscore[,1]), as.factor(as.matrix(d[-perm,]$group)[,1]))$overall["Accuracy"]
      train.rc<-confusionMatrix(as.factor(train.riskscore[,1]), as.factor(as.matrix(d$group)[,1]))$overall["Accuracy"]
      test.rc <- 1-Hmisc::rcorrcens(Surv(t$times, t$events)~ test.riskscore,data = t)[1]
      
      if(is.na(test.rc)==F){
        
      res.tArc<-rbind(res.tArc, tArc) 
      res.tBrc<-rbind(res.tBrc, tBrc) 
      res.testrc<-rbind(res.testrc, test.rc) 
      res.trainrc<-rbind(res.trainrc,train.rc) 
      genelist<-rbind(genelist,tA.name)
      testrs<-cbind(testrs,test.riskscore) 
      coef<-cbind(coef,tA.active.coef) 
      trainrs<-cbind(trainrs,train.riskscore) 
      }
      }
    }
    cirlasso_results<-list(res.tArc=res.tArc,
                           res.tBrc=res.tBrc,
                           res.testrc=res.testrc,
                           res.trainrc=res.trainrc,
                           genelist=genelist,
                           testrs=testrs,
                           coef=coef,
                           trainrs=trainrs)
    
    
  }
 
  cl.cores <- detectCores()
  cl <- makeCluster(cl.cores-3)
  registerDoParallel(cl)
  clusterExport(cl,c("confusionMatrix","Surv","coxph","cv.glmnet","coef","predict","createDataPartition","createResample"))
  mm <- foreach(i=1:nloop, .combine=rbind,.multicombine=F,.verbose=T )%dopar% {
    FUN(d=Train,t=Test,v=varlength,r=R2)
  }
  stopCluster(cl)
  options(warn =1)
  if(nloop==1){
    if(is.null(mm$res.tArc)){
      stop(paste("The nloops with ", nloop, " are too small!", sep=""))
    }else{
      res.tArc<-unlist(mm[1])
      res.tBrc<-unlist(mm[2])
      res.testrc<-unlist(mm[3])
      res.trainrc<-unlist(mm[4])
      genelist<-mm[[5]]
      genelist<-genelist[substr(rownames(genelist),1,7)=="tA.name",]
      genelist<-t(as.matrix(genelist))
      testrs<-mm[[6]]
      coef <-mm[[7]]
      coef<-coef[,!colnames(coef)=="coef"]
      coef<-as.matrix(coef)
      trainrs <-mm[[8]]
    }
  }else{
    if(is.null(do.call("rbind", mm[,1]))){
      stop(paste("The nloops with ", nloop, " are too small!", sep=""))
    }else{
      res.tArc<-do.call("rbind", mm[,1])
      res.tBrc<-do.call("rbind", mm[,2])
      res.testrc<-do.call("rbind", mm[,3])
      res.trainrc<-do.call("rbind", mm[,4])
      genelist<-do.call("rbind", mm[,5])
      genelist<-genelist[substr(rownames(genelist),1,7)=="tA.name",]
      if(is.null(dim(genelist))){
        genelist<-t(genelist) 
      }else{
        genelist<-genelist
      }
      testrs<-do.call("cbind", mm[,6])
      coef<-do.call("cbind", mm[,7])
      coef<-as.matrix(coef[,colnames(coef)=="tA.active.coef"])
      trainrs<- do.call("cbind", mm[,8])
    }
  }
  rcindex<-data.frame(res.tArc,res.tBrc,res.testrc,res.trainrc,row.names = 1:length(res.tArc))
  colnames(rcindex)<-c("res.tArc.acc","res.tBrc.acc","res.testrc.cindex","res.trainrc.acc")
  rcindex$index<-rcindex$res.testrc.cindex^2+rcindex$res.tBrc.acc^2 
  colnames(testrs)<- 1:ncol(testrs)
  colnames(trainrs)<- 1:ncol(trainrs)
  colnames(coef)<- 1:ncol(coef)
  rownames(genelist)<-c(1:nrow(genelist))
  selct.rcindex<-rcindex[rcindex$res.tArc.acc>modelcut & rcindex$res.tBrc.acc>modelcut& rcindex$res.testrc.cindex>modelcut,]#tA和tB，test里c-index均大于0.7
  options(warn =1)
  if (is.na(selct.rcindex[1,1])) { 
    warning(paste0("The modelcut with ", modelcut, " are too high!"," Intermediate results are saved in cirlasso_intermediate_results.rda "))
    save(rcindex,
         selct.rcindex,
         genelist,
         testrs,
         trainrs,
         coef,
         file = "cirlasso_intermediate_results.rda") 
    
    cirlasso_intermediate_results<-list(rcindex=rcindex,
                                        genelist=genelist,
                                        testrs=testrs,
                                        trainrs=trainrs,
                                        coef=coef)
    
    
    
  }else{
    best.rcindex<-selct.rcindex[selct.rcindex$index==max(selct.rcindex$index),] 
    best.vars<-unique(genelist[rownames(best.rcindex),]) 
    best.testrs<-testrs[,rownames(best.rcindex)]
    names(best.testrs)<-rownames(testrs)
    best.coef<-unique(coef[,rownames(best.rcindex)])
    names(best.coef)<-c(best.vars)
    best.trrainrs<-trainrs[,rownames(best.rcindex)]
    names(best.trrainrs)<-rownames(trainrs)
    save(rcindex,
         genelist,
         testrs,
         trainrs,
         best.rcindex,
         best.vars,
         best.testrs,
         best.coef,
         best.trrainrs,
         coef,
         file = "cirlasso_results.rda") 
    cirlasso_results<-list(rcindex=rcindex,
                           genelist=genelist,
                           testrs=testrs,
                           trainrs=trainrs,
                           best.rcindex=best.rcindex,
                           best.vars=best.vars,
                           best.testrs=best.testrs,
                           best.coef=best.coef,
                           best.trrainrs=best.trrainrs,
                           coef=coef)
    return(cirlasso_results)
  }
}


laresults<-cirlasso(train = train,test = test, unilogis=T,varlength= 100 ,nloop= 5000, logiscut=0.001, modelcut=0.8, R2= 0.5,BootStrap=T,lambda = "1se")
summary(laresults)
laresults$rcindex
laresults$best.rcindex
Uni_gene_Fvalue<-laresults$best.vars[-1]
predgroup<-laresults$best.testrs

confusionMatrix(as.factor(test$group),as.factor(laresults$best.testrs))

