
library("h2o")
library("caret")
library("survival")

h2o.init(nthreads = -1,max_mem_size = "64G")
dfs_m_mi_lnc_meth_cnv.hex <- as.h2o(x = dfs_m_mi_lnc_meth_cnv, destination_frame = "dfs_m_mi_lnc_meth_cnv.hex")
setwd("C:/Users/song/Desktop/TCGA_PCa/new_model")
ae_model_8<-h2o.loadModel(paste0(getwd(),"/ae_model_8"))
ae_reduced_8<- h2o.deepfeatures(ae_model_8, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 
ae_reduced_8_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_8)
ae_reduced_clin.df<-as.data.frame(ae_reduced_8_clin.hex)
folds<-caret::createFolds(y=ae_reduced_clin.df$events,k=10)
clin_dd_subgroup.hex<-dfs_m_mi_lnc_meth_cnv.hex
load("ae_feature_8_Uni_0.05.Rda")
new_feat<-as.character(Uni$Characteristics)
c_index<-c()
group<-c()
i=1
for(i in 1:250){
  print(i)
  x<-c(1:10)
  x<-paste("0",x,sep="")
  x<-substr(x,nchar(x)-1,nchar(x))
  x1<-sample(x,4)
  x2<-x[!x %in% c(x1)]
  fold_validation<-ae_reduced_clin.df[unlist(folds[c(paste0("Fold",x1))]),]
  fold_train<-ae_reduced_clin.df[unlist(folds[c(paste0("Fold",x2))]),]
  ae_reduced_train<-as.h2o(fold_train)[,setdiff(colnames(fold_train), c("events","times"))]
  ae_reduced_validation<-as.h2o(fold_validation)[,setdiff(colnames(fold_validation), c("events","times"))]
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
  subgroup<-h2o.predict(kmeans,ae_reduced_8)
  clin_dd_subgroup.hex$group<-as.factor(subgroup)
  clin_dd_subgroup.df<-as.data.frame(clin_dd_subgroup.hex[,c("times","events","group")])
  Sur2 <- Surv(clin_dd_subgroup.df$times, clin_dd_subgroup.df$events)
  fit <- coxph(Sur2 ~ group,data = clin_dd_subgroup.df)
  cindex <- survcomp::concordance.index(x=predict(fit),
                                        surv.time = clin_dd_subgroup.df$times, 
                                        surv.event= clin_dd_subgroup.df$events,
                                        method = "noether" )
  print(cindex$c.index)
  c_index<-c(c_index,as.numeric(cindex$c.index))
  group<-h2o.cbind(group, as.numeric(subgroup))
  
}

mean(na.omit(c_index))
sd(na.omit(c_index))
group.max<-group[,which(c_index==max(na.omit(c_index)))]
subgroup<-group.max
#subgroup<-as.data.frame(subgroup)
#rownames(subgroup)<-rownames(dfs_m_mi_lnc_meth_cnv)
save(subgroup,file = "subgroup_modle8.rda")
