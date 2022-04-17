

ae_reduced_1<- h2o.deepfeatures(ae_model_1, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2)   

ae_reduced_2<- h2o.deepfeatures(ae_model_2, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 
ae_reduced_3<- h2o.deepfeatures(ae_model_3, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 

ae_reduced_4<- h2o.deepfeatures(ae_model_4, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 

ae_reduced_5<- h2o.deepfeatures(ae_model_5, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 

ae_reduced_6<- h2o.deepfeatures(ae_model_6, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 
ae_reduced_7<- h2o.deepfeatures(ae_model_7, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 
ae_reduced_8<- h2o.deepfeatures(ae_model_8, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 2) 

ae_reduced_9<- h2o.deepfeatures(ae_model_9, 
                                dfs_m_mi_lnc_meth_cnv.hex, 
                                layer = 3) 
ae_reduced_10<- h2o.deepfeatures(ae_model_10, 
                                 dfs_m_mi_lnc_meth_cnv.hex, 
                                 layer = 3) 
ae_reduced_11<- h2o.deepfeatures(ae_model_11, 
                                 dfs_m_mi_lnc_meth_cnv.hex, 
                                 layer = 3) 
ae_reduced_12<- h2o.deepfeatures(ae_model_12, 
                                 dfs_m_mi_lnc_meth_cnv.hex, 
                                 layer = 3) 


#2 cbind with tiems and events
ae_reduced_1_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_1)
ae_reduced_2_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_2)
ae_reduced_3_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_3)
ae_reduced_4_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_4)
ae_reduced_5_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_5)
ae_reduced_6_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_6)
ae_reduced_7_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_7)
ae_reduced_8_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_8)
ae_reduced_9_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_9)
ae_reduced_10_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_10)
ae_reduced_11_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_11)
ae_reduced_12_clin.hex <- h2o.cbind(dfs_m_mi_lnc_meth_cnv.hex[,c("times","events")],ae_reduced_12)

#3 unicox function
UniCox<- function(gene,p.cutoff=0.05,data=ae_reduced_25_clin.hex){
  uniRed<-h2o.coxph(x = gene,
                    event_column = "events",
                    stop_column = "times",
                    training_frame = data,
                    max_iterations=1000)
  SuniRed<-summary(uniRed)
  P_Value <- round(SuniRed@summary[["waldtest"]][["pvalue"]],5)
  if(P_Value<=p.cutoff){
    unicox<-data.frame(
      Characteristics = gene,
      P_Value = P_Value)
    return(unicox)
  }
}

features <- colnames(ae_reduced_1) 
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_1_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 12
save(Uni,file = "ae_feature_1_Uni_0.05.Rda")
load("ae_feature_1_Uni_0.05.Rda")

features <- colnames(ae_reduced_2)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_2_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
#8
save(Uni,file = "ae_feature_2_Uni_0.05.Rda")
load("ae_feature_2_Uni_0.05.Rda")

features <- colnames(ae_reduced_3)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_3_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
#21
save(Uni,file = "ae_feature_3_Uni_0.05.Rda")
load("ae_feature_3_Uni_0.05.Rda")

features <- colnames(ae_reduced_4)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_4_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 35
save(Uni,file = "ae_feature_4_Uni_0.05.Rda")
load("ae_feature_4_Uni_0.05.Rda")

features <- colnames(ae_reduced_5)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_5_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 42
save(Uni,file = "ae_feature_5_Uni_0.05.Rda")
load("ae_feature_5_Uni_0.05.Rda")

features <- colnames(ae_reduced_6)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_6_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 57
save(Uni,file = "ae_feature_6_Uni_0.05.Rda")
load("ae_feature_6_Uni_0.05.Rda")

features <- colnames(ae_reduced_7)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_7_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 60
save(Uni,file = "ae_feature_7_Uni_0.05.Rda")
load("ae_feature_7_Uni_0.05.Rda")

features <- colnames(ae_reduced_8)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_8_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 96
save(Uni,file = "ae_feature_8_Uni_0.05.Rda")
load("ae_feature_8_Uni_0.05.Rda")

features <- colnames(ae_reduced_9)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_9_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 13
save(Uni,file = "ae_feature_9_Uni_0.05.Rda")
load("ae_feature_9_Uni_0.05.Rda")

features <- colnames(ae_reduced_10)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_10_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 42
save(Uni,file = "ae_feature_10_Uni_0.05.Rda")
load("ae_feature_10_Uni_0.05.Rda")

features <- colnames(ae_reduced_11)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_11_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 40
save(Uni,file = "ae_feature_11_Uni_0.05.Rda")
load("ae_feature_11_Uni_0.05.Rda")

features <- colnames(ae_reduced_12)
Uni <- lapply(features, p.cutoff=0.05,data=ae_reduced_12_clin.hex, UniCox)
Uni <- plyr::ldply(Uni)
# 69
save(Uni,file = "ae_feature_12_Uni_0.05.Rda")
load("ae_feature_12_Uni_0.05.Rda")

