
hidden <- c(500, 100, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-5 
l2<-0.1
dropout<-c(0.5,0.5,0.5)
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_1 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_1",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

h2o.saveModel(ae_model_1,getwd(),force = TRUE) 

hidden <- c(500, 50, 500) 
activation<-"TanhWithDropout"
epochs<-10 
l1<-1.0E-2 
l2<-1.0E-5 
dropout<-c(0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_2 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_2",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_2,getwd(),force = TRUE) 

#ae_model_3
hidden <- c(500, 150, 500) 
activation<-"TanhWithDropout" 
epochs<-10
l1<-1.0E-3 
l2<-0 
dropout<-c(0.5,0.5,0.5) 

x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_3 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_3",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_3,getwd(),force = TRUE) 

hidden <- c(500, 200, 500) 
activation<-"TanhWithDropout" 
epochs<-10
l1<-1.0E-1 
l2<-1.0E-4 
dropout<-c(0.5,0.5,0.5)
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_4 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_4",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_4,getwd(),force = TRUE) 

#ae_model_5
hidden <- c(500, 250, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-1 
l2<-0 
dropout<-c(0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_5 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_5",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 
setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_5,getwd(),force = TRUE) 


hidden <- c(500, 300, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-2 
l2<-0 
dropout<-c(0.5,0.5,0.5) 

x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_6 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_6",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F)

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_6,getwd(),force = TRUE) 

#ae_model_7
hidden <- c(500, 400, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-5 
l2<-1.0E-3 
dropout<-c(0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_7 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_7",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F)
setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_7,getwd(),force = TRUE)

#ae_model_8
hidden <- c(500, 500, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-0 
l2<-0 
dropout<-c(0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_8 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_8",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_8,getwd(),force = TRUE) 


#ae_model_9
hidden <- c(500, 500,100,500, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-3 
l2<-0 
dropout<-c(0.5,0.5,0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_9 <- h2o.deeplearning(x = x, 
                               training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                               model_id ="ae_model_9",
                               ignore_const_cols = F,
                               activation = activation, 
                               hidden = hidden,
                               autoencoder = TRUE,
                               epochs=epochs,
                               l1=l1,
                               l2=l2,
                               input_dropout_ratio=0,
                               hidden_dropout_ratios= dropout,
                               stopping_metric="AUTO",
                               reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_9,getwd(),force = TRUE) 

#ae_model_10
hidden <- c(500,500,200,500, 500)
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-4  
l2<-1.0E-5 
dropout<-c(0.5,0.5,0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_10 <- h2o.deeplearning(x = x, 
                                training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                                model_id ="ae_model_10",
                                ignore_const_cols = F,
                                activation = activation, 
                                hidden = hidden,
                                autoencoder = TRUE,
                                epochs=epochs,
                                l1=l1,
                                l2=l2,
                                input_dropout_ratio=0,
                                hidden_dropout_ratios= dropout,
                                stopping_metric="AUTO",
                                reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_10,getwd(),force = TRUE) 

#ae_model_11
hidden <- c(500, 500,300,500, 500) 
activation<-"TanhWithDropout" 
epochs<-10 
l1<-1.0E-4  
l2<-1.0E-1 
dropout<-c(0.5,0.5,0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_11 <- h2o.deeplearning(x = x, 
                                training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                                model_id ="ae_model_11",
                                ignore_const_cols = F,
                                activation = activation, 
                                hidden = hidden,
                                autoencoder = TRUE,
                                epochs=epochs,
                                l1=l1,
                                l2=l2,
                                input_dropout_ratio=0,
                                hidden_dropout_ratios= dropout,
                                stopping_metric="AUTO",
                                reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_11,getwd(),force = TRUE) 

#ae_model_12
hidden <- c(500, 500,400,500, 500) 
activation<-"TanhWithDropout" 
epochs<-10
l1<-1.0E-5 
l2<-0 
dropout<-c(0.5,0.5,0.5,0.5,0.5) 
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))
ae_model_12 <- h2o.deeplearning(x = x, 
                                training_frame = dfs_m_mi_lnc_meth_cnv.hex,
                                model_id ="ae_model_12",
                                ignore_const_cols = F,
                                activation = activation, 
                                hidden = hidden,
                                autoencoder = TRUE,
                                epochs=epochs,
                                l1=l1,
                                l2=l2,
                                input_dropout_ratio=0,
                                hidden_dropout_ratios= dropout,
                                stopping_metric="AUTO",
                                reproducible=F) 

setwd("C:/Users/Administrator/Documents/")
h2o.saveModel(ae_model_12,getwd(),force = TRUE) 