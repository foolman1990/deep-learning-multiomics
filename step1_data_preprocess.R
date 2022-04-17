setwd("~/Desktop/song/pca_project")

mrna<-read.table("TCGA_PCa_mRNA_cancer_count.txt",header = T)
mrna <- mrna[!duplicated(mrna[,1]), ]
rownames(mrna)<-mrna$Tags
mrna<-mrna[,-(1)]
save(mrna,file = "pca_mrna_cancer_count.rda")
dim(mrna)
load("pca_mrna_cancer_count.rda")
boxplot(log2(mrna[,1:10]+0.1))
keep <- rowSums(mrna) >= 10
mrna <- mrna[keep,]
mrna<-as.matrix(mrna)
mrna<-DESeq2::varianceStabilizingTransformation(round(mrna))
save(mrna,file = "pca_mrna_cancer_count_vst.rda")

load("TCGA-PRAD_mrna_FPKM.rda") 
dim(count_matrix)
head(count_matrix)[1:3,1:5]
colnames(count_matrix)<-substr(colnames(count_matrix),1,16)
colnames(count_matrix)<-gsub("-",".",colnames(count_matrix))
rna_fpkm<-count_matrix
save(rna_fpkm,file="pca_rna_cancer_fpkm.rda")

load("pca_rna_cancer_fpkm.rda")
load("Homo_sapiens.GRCh38.96.chr_df.Rda")
rna_fpkm<-as.data.frame(rna_fpkm)
head(rna_fpkm)[1:3,1:3]
library(dplyr)
library(tidyr)
rna_fpkm$gene_id<-rownames(rna_fpkm)
# 1.2.1 mRNA
mRNA_exprSet <- gtf_df %>% 
  dplyr::filter(type=="gene",gene_biotype=="protein_coding") %>% #筛???gene,和编码指???
  dplyr::select(c(gene_name,gene_id)) %>% 
  dplyr::inner_join(rna_fpkm,by ="gene_id")
dim(mRNA_exprSet)
# 19638   553
head(mRNA_exprSet)[1:3,1:3]
mrna_fpkm<-mRNA_exprSet
mrna_fpkm<-mrna_fpkm[!duplicated(mrna_fpkm[,1]), ]
rownames(mrna_fpkm)<-mrna_fpkm$gene_name
mrna_fpkm<-mrna_fpkm[,-c(1,2)]
dim(mrna_fpkm)
#19634   551
head(mrna_fpkm)[1:3,1:3]
ff<-as.numeric(substr(colnames(mrna_fpkm),14,15)) %in% c(1:9)
mrna_fpkm<-mrna_fpkm[,ff]
dim(mrna_fpkm)
#19634   499
low_count_mask <- rowSums(mrna_fpkm!=0)/ncol(mrna_fpkm) <=0.2 # 过滤掉在20%的样本中表达为零的基???
mrna_fpkm <- mrna_fpkm[!low_count_mask,]
dim(mrna_fpkm)
#18223   499
save(mrna_fpkm,file = "pca_mrna_cancer_fpkm.rda")

ncRNA <- c("sense_overlapping","lincRNA","3prime_overlapping_ncRNA",
           "processed_transcript","sense_intronic",
           "bidirectional_promoter_lncRNA","non_coding",
           "antisense_RNA")

LncRNA_exprSet <- gtf_df %>% 
  dplyr::filter(type=="transcript",transcript_biotype %in% ncRNA) %>% #transcript_biotype
  dplyr::select(c(gene_name,gene_id)) %>% 
  dplyr::distinct() %>% 
  dplyr::inner_join(rna_fpkm,by ="gene_id") 

dim(LncRNA_exprSet)
# 19822  553
head(LncRNA_exprSet)[1:3,1:3]
lncrna_fpkm<-LncRNA_exprSet
lncrna_fpkm$gene_name<-gsub("-","_",lncrna_fpkm$gene_name)
lncrna_fpkm<-lncrna_fpkm[!duplicated(lncrna_fpkm[,1]), ]
dim(lncrna_fpkm)
#19818   553
rownames(lncrna_fpkm)<-lncrna_fpkm$gene_name
lncrna_fpkm<-lncrna_fpkm[,-c(1,2)]
dim(lncrna_fpkm)
#19818   551
head(lncrna_fpkm)[1:3,1:3]
ff<-as.numeric(substr(colnames(lncrna_fpkm),14,15)) %in% c(1:9)
lncrna_fpkm<-lncrna_fpkm[,ff]
dim(lncrna_fpkm)
#19818   499
low_count_mask <- rowSums(lncrna_fpkm!=0)/ncol(lncrna_fpkm) <=0.2 # 过滤掉在20%的样本中表达为零的基???
lncrna_fpkm <- lncrna_fpkm[!low_count_mask,]
dim(lncrna_fpkm)
#15912  499
save(lncrna_fpkm,file = "pca_lncrna_cancer_fpkm.rda")

lncrna<-read.table("TCGA_PCa_lncRNA_cancer_count.txt",header = T)
head(lncrna)[1:3,1:3]
lncrna <- lncrna[!duplicated(lncrna[,1]), ]
rownames(lncrna)<-lncrna$SYMBOL
lncrna<-lncrna[,-(1:2)]
save(lncrna,file = "pca_lncrna_cancer_count.rda")
dim(lncrna)
load("pca_lncrna_cancer_count.rda")
boxplot(log2(lncrna[,1:10]+0.1))
keep <- rowSums(lncrna) >= 10
lncrna <- lncrna[keep,]
lncrna<-as.matrix(lncrna)
lncrna<-DESeq2::varianceStabilizingTransformation(round(lncrna))
save(lncrna,file = "pca_lncrna_cancer_count_vst.rda")

mirna<-read.table("TCGA_PCa_microRNA_count.txt",header = T,row.names = 1)
head(mirna)[1:3,1:3]
rownames(mirna)<-gsub("-","_",rownames(mirna))
colnames(mirna)<-substr(colnames(mirna),1,15)
save(mirna,file = "pca_mirna_cancer_count.rda")
dim(mirna)
load("pca_mirna_cancer_count.rda")
boxplot(log2(mirna[,1:10]+0.1))
keep <- rowSums(mirna) >= 10
mirna <- mirna[keep,]
mirna<-as.matrix(mirna)
mirna<-DESeq2::varianceStabilizingTransformation(round(mirna))
save(mirna,file = "pca_mirna_cancer_count_vst.rda")

load("TCGA-PRAD_bcgsc_miRNA.rda")
head(expdat)[1:3,1:8]
rownames(expdat)<-expdat$miRNA_ID
expdat<-expdat[,-1]
mirna_map<-expdat[,substr(colnames(expdat),1,31)=="reads_per_million_miRNA_mapped_"]
colnames(mirna_map)<-substr(colnames(mirna_map),32,59)
dim(mirna_map)
#1881 551
head(mirna_map)[1:3,1:5]
rownames(mirna_map)<-gsub("-","_",rownames(mirna_map))
colnames(mirna_map)<-substr(colnames(mirna_map),1,16)
colnames(mirna_map)<-gsub("-",".",colnames(mirna_map))
ff<-as.numeric(substr(colnames(mirna_map),14,15)) %in% c(1:9)
mirna_map<-mirna_map[,ff]
dim(mirna_map)
#1881  499
low_count_mask <- rowSums(mirna_map!=0)/ncol(mirna_map) <=0.2 # 过滤掉在20%的样本中表达为零的基???
mirna_map <- mirna_map[!low_count_mask,]
dim(mirna_map)
#673  499
save(mirna_map,file = "pca_mirna_map_cancer_rpm.rda")

load("TCGA-PRAD_Copy_Number_Variation_Gene_Level_Copy_Number_Scores.rda")
head(expdat)[1:3,1:4]
dim(expdat)
cnv_biolinks<-expdat
cnv_biolinks<-as.data.frame(cnv_biolinks)
head(cnv_biolinks)[1:3,1:4]
dim(cnv_biolinks)
#19729 505
cnv_biolinks<-cnv_biolinks[!duplicated(cnv_biolinks[,1]), ]
rownames(cnv_biolinks)<-cnv_biolinks[,1]
cnv_biolinks<-cnv_biolinks[,-c(1,2,3)]
dim(cnv_biolinks)
#19729 502
colnames(cnv_biolinks)<-substr(colnames(cnv_biolinks),1,16)
colnames(cnv_biolinks)<-gsub("-",".",colnames(cnv_biolinks))
ff<-as.numeric(substr(colnames(cnv_biolinks),14,15)) %in% c(1:9)
cnv_biolinks<-cnv_biolinks[,ff]
dim(cnv_biolinks)
# 19729   304
low_count_mask <- rowSums(cnv_biolinks!=0)/ncol(cnv_biolinks) <=0.2 
cnv_biolinks <- cnv_biolinks[!low_count_mask,]
dim(cnv_biolinks)
#268   304
head(cnv_biolinks)[1:3,1:3]
save(cnv_biolinks,file ="pca_cnv_cancer_biolinks_scores.rda" )
RNA <- c("sense_overlapping","lincRNA","3prime_overlapping_ncRNA",
         "processed_transcript","sense_intronic",
         "bidirectional_promoter_lncRNA","non_coding",
         "antisense_RNA","protein_coding")
cnv_biolinks$gene_id<-substr(rownames(cnv_biolinks),1,15)
cnv_biolinks <- gtf_df %>% 
  dplyr::filter(type=="transcript",gene_biotype %in% RNA) %>% #gene_biotype
  dplyr::select(c(gene_name,gene_id)) %>% 
  dplyr::distinct() %>% 
  dplyr::inner_join(cnv_biolinks,by ="gene_id") 
dim(cnv_biolinks)
boxplot(cnv_biolinks[1:200,])
cnv<-read.table("all_thresholded.by_genes.txt",header = T,sep = "\t")
head(cnv)[1:5,1:6]
dim(cnv)
#25988  1032
cnv<-cnv[!duplicated(cnv[,1]), ]
rownames(cnv)<-cnv$Gene.Symbol
cnv<-cnv[,-c(1,2,3)]
dim(cnv)
#25988  1029
ff<-as.numeric(substr(colnames(cnv),14,15)) %in% c(1:9)
cnv<-cnv[,ff]
dim(cnv)
# 25988   498
low_count_mask <- rowSums(cnv!=0)/ncol(cnv) <=0.2 # 过滤掉在20%的样本中无扩增或删失的基???
cnv <- cnv[!low_count_mask,]
dim(cnv)
#5028   498
head(cnv)[1:3,1:3]
save(cnv,file = "pca_cnv_cancer_gistic_scores.rda")

clin<-read.csv("pcaclintcgabiolinks.csv",header = T)
head(clin)
clin<-clin[1:427,]
dim(clin)
rownames(clin)<-clin$bcr_patient_barcode
rownames(clin)<-gsub("-",".", rownames(clin))
clin$events<-ifelse(clin$biochemical_recurrence=="YES",1,0)
clin$times<-ifelse(clin$biochemical_recurrence=="YES",clin$days_to_first_biochemical_recurrence,clin$days_to_last_followup)
save(clin,file = "pca_clin.rda")                
dim(clin)
load("pca_clin.rda")

meth<-read.table("Illumina Human Methylation 450 TCGA PCa cancer.txt",header = T)
dim(meth)
head(meth)[1:3,1:3]
rownames(meth)<-meth$Tags
col<-setdiff(colnames(meth),"Tags")
meth<-meth[,col]
meth<-na.omit(meth)
save(meth,file = "pca_meth_cancer_betavalue.rda")  
load("pca_meth_cancer_betavalue.rda")

library(data.table)
nameList <- list.files()
expr_df <- fread(nameList[1])
a<-Hmisc::substring.location(nameList[1], 'TCGA')$first
b<-Hmisc::substring.location(nameList[1], '.gdc')$first
TCGA_id<-substr(nameList[1],a,b-1)
expr_df <- dplyr::select(expr_df,c("Composite Element REF",Chromosome,Start,End,Gene_Symbol,Beta_value)) 
names(expr_df)[6] <- TCGA_id
for (i in 2:length(nameList)){
  print(i)
  expr_new <- fread(nameList[i])
  a<-Hmisc::substring.location(nameList[i], 'TCGA')$first
  b<-Hmisc::substring.location(nameList[i], '.gdc')$first
  TCGA_id<-substr(nameList[i],a,b-1)
  expr_df$add <- expr_new$Beta_value
  names(expr_df)[i+5] <- TCGA_id
}
save(expr_df,file = "pca_6_methylation_data_merged_raw.Rda")
dim(expr_df)
library(dplyr)
library(tidyr)
names(expr_df)[5] <- "id"
expr_df[is.na(expr_df)] =0
expr_df<-as.data.frame(expr_df)
head(expr_df)[1:4,1:6]
expr_df<-expr_df[,-c(1:4)]
expr_df<-expr_df[expr_df$id!=".",]
expr_df$id<-limma::strsplit2(expr_df$id,split = "\\;")[,1]
#geneMethy<-aggregate(.~id,expr_df,mean)
#head(geneMethy)[1:3,1:5]
geneMethy <- expr_df %>% 
  group_by(id) %>% 
  summarise_all(mean) 
geneMethy<-as.data.frame(geneMethy)
save(geneMethy,file = "pca_geneMethy.Rda")
rownames(geneMethy)<-geneMethy$id
geneMethy<-geneMethy[,-1]
dim(geneMethy)
colnames(geneMethy)<-substr(colnames(geneMethy),1,16)
colnames(geneMethy)<-gsub("-",".",colnames(geneMethy))
rownames(geneMethy)<-gsub("-","_",rownames(geneMethy))
ff<-as.numeric(substr(colnames(geneMethy),14,15)) %in% c(1:9)
geneMethy<-geneMethy[,ff]
dim(geneMethy)
#29005 502
save(geneMethy,file = "pca_cancer_geneMethy.Rda")

load("pca_cancer_geneMethy.Rda")
dim(geneMethy)
#29005 502
head(geneMethy)[1:3,1:3]
load("pca_mrna_cancer_fpkm.rda")
load("pca_lncrna_cancer_fpkm.rda")
load("pca_mirna_map_cancer_rpm.rda")
load("pca_cnv_cancer_gistic_scores.rda")
load("pca_cancer_geneMethy.Rda")
load("pca_clin.rda")
#Unit scale normalization
scalar1 <- function(x) {x / sqrt(sum(x^2))}
head(mrna_fpkm)[1:3,1:3]
mrna<-scalar1(as.data.frame(t(mrna_fpkm)))
head(mirna_map)[1:3,1:3]
mirna<-scalar1(as.data.frame(t(mirna_map)))
head(lncrna_fpkm)[1:3,1:3]
lncrna<-scalar1(as.data.frame(t(lncrna_fpkm)))
head(cnv)[1:3,1:3]
cnv<-as.data.frame(t(cnv))
head(geneMethy)[1:3,1:3]
meth<-scalar1(as.data.frame(t(geneMethy)))
head(clin)[1:3,1:3]
dfs<-clin[,c("times","events")]
dim(dfs)
dim(clin)
substr(rownames(mrna),1,12) 
rownames(dfs)
substr(rownames(mirna),1,12) 
substr(rownames(lncrna),1,12) 
substr(rownames(meth),1,12) 
substr(rownames(cnv),1,12) 
sample<-intersect(intersect(intersect(substr(rownames(mrna),1,12) ,rownames(dfs)),
                            intersect(substr(rownames(mirna),1,12), substr(rownames(lncrna),1,12))),substr(rownames(meth),1,12))

sample<-intersect(sample, substr(rownames(cnv),1,12))

dfs_m_mi_lnc_meth_cnv<-data.frame(dfs[sample,],mrna[paste0(sample,".01A"),],mirna[paste0(sample,".01A"),],meth[paste0(sample,".01A"),],cnv[paste0(sample,".01"),])
dim(dfs_m_mi_lnc_meth_cnv)
#417 52931
save(dfs_m_mi_lnc_meth_cnv,file = "pca_cancer_dfs_m_mi_lnc_meth_cnv.rda")
