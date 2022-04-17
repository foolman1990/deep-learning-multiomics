
rm(list = ls())
library("h2o")
h2o.init(nthreads = -1,max_mem_size = "100G")
dfs_m_mi_lnc_meth_cnv.hex <- as.h2o(x = dfs_m_mi_lnc_meth_cnv, destination_frame = "dfs_m_mi_lnc_meth_cnv.hex")
head(dfs_m_mi_lnc_meth_cnv.hex)[1:3,1:3]
x <- setdiff(names(dfs_m_mi_lnc_meth_cnv.hex), c("event","times"))

l1 <- 1e-4 
l2 <- 1e-3 
hidden <- list( c(500, 25, 500), c(500, 50, 500), c(500, 100, 500), c(500, 150, 500))
epochs<-c(10,100)
dropout<-c(0.5,0.5,0.5)
hyper_params <- list(hidden = hidden,
                     epochs=epochs)

search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 10)

splits <- h2o.splitFrame(data=dfs_m_mi_lnc_meth_cnv.hex, ratios=c(0.6), seed = 1)
clin_dd.hex_train<-splits[[1]]
clin_dd.hex_validation<-splits[[2]]

ae_grid <- h2o.grid("deeplearning", 
                    x = x,
                    grid_id = "ae_grid1",
                    training_frame = clin_dd.hex_train,
                    validation_frame = clin_dd.hex_validation,
                    seed = 1,
                    l1=l1,
                    l2=l2,
                    hidden_dropout_ratios= dropout,
                    keep_cross_validation_models = TRUE,
                    hyper_params = hyper_params,
                    search_criteria = search_criteria,
                    ignore_const_cols = F,
                    activation = "TanhWithDropout", 
                    autoencoder = TRUE,
                    input_dropout_ratio=0,
                    stopping_metric="AUTO")


dl_gridperf <- h2o.getGrid(grid_id = "ae_grid1", 
                           sort_by = "rmse", 
                           decreasing = F)

ae_grid1_model_8 <- dl_gridperf@model_ids[[1]]
ae_grid1_model_8 <- h2o.getModel(ae_grid1_model_8)

ae_grid1_model_3 <- dl_gridperf@model_ids[[3]]
ae_grid1_model_3 <- h2o.getModel(ae_grid1_model_3)

ae_grid1_model_6 <- dl_gridperf@model_ids[[6]]
ae_grid1_model_6 <- h2o.getModel(ae_grid1_model_6)

ae_grid1_model_2 <- dl_gridperf@model_ids[[8]]
ae_grid1_model_2 <- h2o.getModel(ae_grid1_model_2)

h2o.saveModel(ae_grid1_model_8,getwd(),force = TRUE) #
h2o.saveModel(ae_grid1_model_3,getwd(),force = TRUE)
h2o.saveModel(ae_grid1_model_6,getwd(),force = TRUE)
h2o.saveModel(ae_grid1_model_2,getwd(),force = TRUE)

l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
hidden <- list( c(250, 100, 250), c(500, 100, 500), c(1000, 100, 1000), c(5000, 100, 5000))
dropout<-list(c(0.1,0.1,0.1),c(0.3,0.3,0.3),c(0.5,0.5,0.5),c(0,0,0))
hyper_params <- list(hidden = hidden,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     hidden_dropout_ratios=dropout)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 15)
splits <- h2o.splitFrame(data=dfs_m_mi_lnc_meth_cnv.hex, ratios=c(0.6), seed = 1)
clin_dd.hex_train<-splits[[1]]
clin_dd.hex_validation<-splits[[2]]
ae_grid2 <- h2o.grid("deeplearning", 
                     x = x,
                     grid_id = "ae_grid2",
                     training_frame = clin_dd.hex_train,
                     validation_frame = clin_dd.hex_validation,
                     seed = 1,
                     epochs=10,
                     keep_cross_validation_models = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria,
                     ignore_const_cols = F,
                     activation = "TanhWithDropout", 
                     autoencoder = TRUE,
                     input_dropout_ratio=0,
                     stopping_metric="AUTO")
dl_gridperf <- h2o.getGrid(grid_id = "ae_grid2", 
                           sort_by = "rmse", 
                           decreasing = F)

# grid search round 3
l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
hidden <- list( c(500, 50, 500), c(500, 100, 500), c(500, 150, 500), c(500, 200, 500))
dropout<-list(c(0.3,0.3,0.3),c(0.5,0.5,0.5))
hyper_params <- list(hidden = hidden,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     hidden_dropout_ratios=dropout)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 20)
splits <- h2o.splitFrame(data=dfs_m_mi_lnc_meth_cnv.hex, ratios=c(0.6), seed = 1)
clin_dd.hex_train<-splits[[1]]
clin_dd.hex_validation<-splits[[2]]

ae_grid3 <- h2o.grid("deeplearning", 
                     x = x,
                     grid_id = "ae_grid3",
                     training_frame = clin_dd.hex_train,
                     validation_frame = clin_dd.hex_validation,
                     seed = 1,
                     epochs=10,
                     keep_cross_validation_models = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria,
                     ignore_const_cols = F,
                     activation = "TanhWithDropout", 
                     autoencoder = TRUE,
                     input_dropout_ratio=0,
                     stopping_metric="AUTO")


dl_gridperf <- h2o.getGrid(grid_id = "ae_grid3", 
                           sort_by = "rmse", 
                           decreasing = F)

ae_grid3_model_12  <- dl_gridperf@model_ids[[1]] # 50  0.001 1.0E-5
ae_grid3_model_12  <- h2o.getModel(ae_grid3_model_12)

ae_grid3_model_16  <- dl_gridperf@model_ids[[2]] # 100 1.0E-5   0.1
ae_grid3_model_16  <- h2o.getModel(ae_grid3_model_16 )

ae_grid3_model_3  <- dl_gridperf@model_ids[[3]] #200 0.1 1.0E-4
ae_grid3_model_3  <- h2o.getModel(ae_grid3_model_3 )

ae_grid3_model_15  <- dl_gridperf@model_ids[[5]] #150 0.001    0.0
ae_grid3_model_15  <- h2o.getModel(ae_grid3_model_15)

h2o.saveModel(ae_grid3_model_12,getwd(),force = TRUE) #
h2o.saveModel(ae_grid3_model_16,getwd(),force = TRUE)
h2o.saveModel(ae_grid3_model_3,getwd(),force = TRUE)
h2o.saveModel(ae_grid3_model_15,getwd(),force = TRUE)

l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
hidden <- list( c(500, 250, 500), c(500, 300, 500), c(500, 400, 500), c(500, 500, 500))
dropout<-list(c(0.3,0.3,0.3),c(0.5,0.5,0.5))
hyper_params <- list(hidden = hidden,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     hidden_dropout_ratios=dropout)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 20)

splits <- h2o.splitFrame(data=dfs_m_mi_lnc_meth_cnv.hex, ratios=c(0.6), seed = 1)
clin_dd.hex_train<-splits[[1]]
clin_dd.hex_validation<-splits[[2]]
ae_grid4 <- h2o.grid("deeplearning", 
                     x = x,
                     grid_id = "ae_grid4",
                     training_frame = clin_dd.hex_train,
                     validation_frame = clin_dd.hex_validation,
                     seed = 1,
                     epochs=10,
                     keep_cross_validation_models = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria,
                     ignore_const_cols = F,
                     activation = "TanhWithDropout", 
                     autoencoder = TRUE,
                     input_dropout_ratio=0,
                     stopping_metric="AUTO")

dl_gridperf <- h2o.getGrid(grid_id = "ae_grid4", 
                           sort_by = "rmse", 
                           decreasing = F)

ae_grid4_model_10   <- dl_gridperf@model_ids[[1]] # 250  0.1 0
ae_grid4_model_10   <- h2o.getModel(ae_grid4_model_10 )

ae_grid4_model_4    <- dl_gridperf@model_ids[[2]] # 300 1.0E-2   0
ae_grid4_model_4    <- h2o.getModel(ae_grid4_model_4   )

ae_grid4_model_1   <- dl_gridperf@model_ids[[4]] #400 1.0E-5  0.001
ae_grid4_model_1   <- h2o.getModel(ae_grid4_model_1  )

ae_grid4_model_14  <- dl_gridperf@model_ids[[5]] #500 0   0.0
ae_grid4_model_14  <- h2o.getModel(ae_grid4_model_14)

h2o.saveModel(ae_grid4_model_10,getwd(),force = TRUE) #
h2o.saveModel(ae_grid4_model_4,getwd(),force = TRUE)
h2o.saveModel(ae_grid4_model_1,getwd(),force = TRUE)
h2o.saveModel(ae_grid4_model_14,getwd(),force = TRUE)


l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
hidden <- list( c(500,500, 100, 500,500), c(500,500, 200, 500,500), c(500,500, 300, 500,500),c(500,500, 400, 500,500),c(500,500, 500, 500,500))
dropout<-list(c(0.3,0.3,0.3,0.3,0.3),c(0.5,0.5,0.5,0.5,0.5))
hyper_params <- list(hidden = hidden,
                     l1 = l1_opt,
                     l2 = l2_opt,
                     hidden_dropout_ratios=dropout)
search_criteria <- list(strategy = "RandomDiscrete", 
                        max_models = 30)
splits <- h2o.splitFrame(data=dfs_m_mi_lnc_meth_cnv.hex, ratios=c(0.6), seed = 1)
clin_dd.hex_train<-splits[[1]]
clin_dd.hex_validation<-splits[[2]]
ae_grid5 <- h2o.grid("deeplearning", 
                     x = x,
                     grid_id = "ae_grid5",
                     training_frame = clin_dd.hex_train,
                     validation_frame = clin_dd.hex_validation,
                     seed = 1,
                     epochs=10,
                     keep_cross_validation_models = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria,
                     ignore_const_cols = F,
                     activation = "TanhWithDropout", 
                     autoencoder = TRUE,
                     input_dropout_ratio=0,
                     stopping_metric="AUTO")


dl_gridperf <- h2o.getGrid(grid_id = "ae_grid5", 
                           sort_by = "rmse", 
                           decreasing = F)

ae_grid5_model_23     <- dl_gridperf@model_ids[[1]] # 200 
ae_grid5_model_23     <- h2o.getModel(ae_grid5_model_23 )

ae_grid5_model_9      <- dl_gridperf@model_ids[[2]] # 400 
ae_grid5_model_9      <- h2o.getModel(ae_grid5_model_9 )

ae_grid5_model_18       <- dl_gridperf@model_ids[[4]] # 100 
ae_grid5_model_18       <- h2o.getModel(ae_grid5_model_18 )

ae_grid5_model_3        <- dl_gridperf@model_ids[[5]] # 300 
ae_grid5_model_3        <- h2o.getModel(ae_grid5_model_3)

h2o.saveModel(ae_grid5_model_23,getwd(),force = TRUE) #
h2o.saveModel(ae_grid5_model_9,getwd(),force = TRUE)
h2o.saveModel(ae_grid5_model_18,getwd(),force = TRUE)
h2o.saveModel(ae_grid5_model_3,getwd(),force = TRUE)






