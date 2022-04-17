
setwd("C:\\Users\\song\\Desktop\\TCGA_PCa\\new_model")
library(caret)
library(survival)
if(!require("survcomp")) BiocManager::install("survcomp",update = F,ask = F)
bestaemodel<-function(data.hex, aemodel,layer=2,uni,loop){
  modelname<-paste0(getwd(),"/",aemodel)
  ae_model<-h2o.loadModel(modelname)
  ae_reduced<- h2o.deepfeatures(ae_model, 
                                data.hex, 
                                layer = layer)
  ae_reduced_clin.hex <- h2o.cbind(data.hex[,c("times","events")],ae_reduced) # events
  load(paste0(getwd(),"/",uni))
  new_feat<-as.character(Uni$Characteristics)
  ae_reduced_clin.df<-as.data.frame(ae_reduced_clin.hex)
  folds<-createFolds(y=ae_reduced_clin.df$events,k=10)  #events
  clin_dd_subgroup.hex<-data.hex
  c_index<-c()
  group<-c()
  i=1
  for(i in 1:loop){
    print(i)
    x<-c(1:10)
    x<-paste("0",x,sep="")
    x<-substr(x,nchar(x)-1,nchar(x))
    x1<-sample(x,4)
    x2<-x[!x %in% c(x1)]
    fold_validation<-ae_reduced_clin.df[unlist(folds[c(paste0("Fold",x1))]),]
    fold_train<-ae_reduced_clin.df[unlist(folds[c(paste0("Fold",x2))]),]
    ae_reduced_train<-as.h2o(fold_train)[,setdiff(colnames(fold_train), c("events","times"))] #events
    ae_reduced_validation<-as.h2o(fold_validation)[,setdiff(colnames(fold_validation), c("events","times"))] #events
    kmeans<-h2o.kmeans(training_frame=ae_reduced_train, 
                       x=new_feat,  
                       validation_frame = ae_reduced_validation,
                       nfolds = 5, 
                       fold_assignment = "AUTO", 
                       k = 2, 
                       estimate_k = F,
                       init="PlusPlus",
                       standardize = T,
                       seed = -1)
    subgroup<-h2o.predict(kmeans,ae_reduced)
    clin_dd_subgroup.hex$group<-as.factor(subgroup)
    clin_dd_subgroup.df<-as.data.frame(clin_dd_subgroup.hex[,c("times","events","group")]) #events
    Sur2 <- Surv(clin_dd_subgroup.df$times, clin_dd_subgroup.df$events) #events
    fit <- coxph(Sur2 ~ group,data = clin_dd_subgroup.df)
    cindex <- survcomp::concordance.index(x=predict(fit),
                                          surv.time = clin_dd_subgroup.df$times, 
                                          surv.event= clin_dd_subgroup.df$events, #events
                                          method = "noether" )
    print(cindex$c.index)
    c_index<-c(c_index,as.numeric(cindex$c.index))
    group<-h2o.cbind(group, as.numeric(subgroup))
    
  }
  print(paste0("Mean c_index: ",mean(c_index)))
  print(paste0("SD c_index: ",sd(c_index)))
  
}

bestaemodel(aemodel="ae_model_1",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_1_Uni_0.05.Rda",loop=10)
# "Mean c_index: 0.649058135429326"
# "SD c_index: 0.0257776730266032"
bestaemodel(aemodel="ae_model_2",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_2_Uni_0.05.Rda",loop=10)
# "Mean c_index: 0.647149727998994"
# "SD c_index: 0.0343577065142467"
bestaemodel(aemodel="ae_model_3",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_3_Uni_0.05.Rda",loop=10)
# "Mean c_index: 0.767450705755274"
# "SD c_index: 0.0202406751275109"
bestaemodel(aemodel="ae_model_4",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_4_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.705841495281665"
# "SD c_index: 0.055464158809907"
bestaemodel(aemodel="ae_model_5",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_5_Uni_0.05.Rda",loop=10)
# "Mean c_index: 0.748977676955774"
# "SD c_index: 0.0267068651580753"
bestaemodel(aemodel="ae_model_6",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_6_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.668297420853273"
#"SD c_index: 0.0212727792734901"
bestaemodel(aemodel="ae_model_7",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_7_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.723078384022221"
#"SD c_index: 0.0200749941962044"
bestaemodel(aemodel="ae_model_8",layer=2,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_8_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.774581850565699"
#"SD c_index: 0.0155791850939812"
bestaemodel(aemodel="ae_model_9",layer=3,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_9_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.631090133716035"
# "SD c_index: 0.0399452070211053"
bestaemodel(aemodel="ae_model_10",layer=3,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_10_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.757929184107995"
# "SD c_index: 0.029427179077185"
bestaemodel(aemodel="ae_model_11",layer=3,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_11_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.76257673167717"
#"SD c_index: 0.0115434916153549"
bestaemodel(aemodel="ae_model_12",layer=3,data.hex=dfs_m_mi_lnc_meth_cnv.hex,uni="ae_feature_12_Uni_0.05.Rda",loop=10)
#"Mean c_index: 0.760530703739463"
#"SD c_index: 0.0291927391795838"

#ae_model_8 is best
ae_model_8<-h2o.loadModel(paste0(getwd(),"/ae_model_8"))
#ae_model_8 is second
ae_model_3<-h2o.loadModel(paste0(getwd(),"/ae_model_3"))
