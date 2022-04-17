#survival curve of the new best subgroup
clin_dd_subgroup.hex<-dfs_m_mi_lnc_meth_cnv.hex
clin_dd_subgroup.hex$group<-as.factor(subgroup[,1])
h2o.table(clin_dd_subgroup.hex$group,clin_dd_subgroup.hex$events)
head(clin_dd_subgroup.hex)[1:3,1:3]
clin_dd_subgroup.df<-as.data.frame(clin_dd_subgroup.hex[,c("times","events","group")])
rownames(clin_dd_subgroup.df)<-rownames(dfs_m_mi_lnc_meth_cnv)
save(clin_dd_subgroup.df,file = "clin_dd_subgroup_model8.df.rda")
if(!require("survival")) BiocManager::install("survival",update = F,ask = F)
if(!require("survivalROC")) BiocManager::install("survivalROC",update = F,ask = F)
if(!require("survminer")) BiocManager::install("survminer",update = F,ask = F)
Sur <- Surv(clin_dd_subgroup.df$times/365,clin_dd_subgroup.df$events)
sfit <- survfit(Sur ~ group,data=clin_dd_subgroup.df) 
pvalue<-surv_pvalue(sfit, data = clin_dd_subgroup.df, method = "survdiff")$pval
ggsurvplot(sfit, 
           conf.int=F,
           #fun="pct",
           pval=paste0("log-rank p-value: ",round(pvalue,9)),
           palette = "jco",
           pval.method = T,
           risk.table =T, 
           ncensor.plot = T,
           surv.median.line="hv",
           legend.labs=c("S1","S2"))+labs(x = "Year")



