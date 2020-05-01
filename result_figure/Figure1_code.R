load("ICB_figure_code.rda")
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(ggplot2)
#TMB landscanpe
clinical_info=read.table("/home/miaoyr/ICB_project/data/all_data_clinical_info.txt",header=T,row.names=1,sep="\t")

sample_TMB=read.table("phase123_sample_TMB",header=T,row.names=1,sep="\t")
sample_id=intersect(row.names(sample_TMB),row.names(clinical_info))
clinical_info_new=cbind(sample_id,clinical_info[sample_id,])
clinical_info_new=clinical_info_new[-grep(TRUE,is.na(clinical_info_new$Anti_target)),]
Anti=c("anti-CTLA4","anti-PD1")
res=c("PR","CR","R","long-survival","long-term-benefit")
nres=c("PD","SD","NR","minimal-or-no-benefit")


clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% res | Second_Response_standard %in% res)%>%
  dplyr::filter(Bioproject == "PRJNA307199")%>%
  dplyr::filter(Anti_target == "anti-PD1")->melanoma_metadata_pd1_R
clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% nres | Second_Response_standard %in% nres)%>%
  dplyr::filter(Bioproject == "PRJNA307199")%>%
  dplyr::filter(Anti_target == "anti-PD1")->melanoma_metadata_pd1_NR


clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% res | Second_Response_standard %in% res)%>%
  dplyr::filter(Bioproject == "PRJNA343789")%>%
  dplyr::filter(Anti_target == "anti-PD1")->melanoma_metadata_pd1_R1
clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% nres | Second_Response_standard %in% nres)%>%
  dplyr::filter(Bioproject == "PRJNA343789")%>%
  dplyr::filter(Anti_target == "anti-PD1")->melanoma_metadata_pd1_NR1



clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% res | Second_Response_standard %in% res)%>%
  dplyr::filter(Bioproject == "PRJNA306070")%>%
  dplyr::filter(Anti_target == "anti-CTLA4")->melanoma_metadata_CTLA4_PRJNA306070_R
clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% nres | Second_Response_standard %in% nres)%>%
  dplyr::filter(Bioproject == "PRJNA306070")%>%
  dplyr::filter(Anti_target == "anti-CTLA4")->melanoma_metadata_CTLA4_PRJNA306070_NR


clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% res | Second_Response_standard %in% res)%>%
  dplyr::filter(Bioproject == "PRJNA82747")%>%
  dplyr::filter(Anti_target == "anti-CTLA4")->melanoma_metadata_CTLA4_PRJNA82747_R
clinical_info_new %>% 
  dplyr::filter(Cancer == "melanoma")%>%
  dplyr::filter( Response %in% nres | Second_Response_standard %in% nres)%>%
  dplyr::filter(Bioproject == "PRJNA82747")%>%
  dplyr::filter(Anti_target == "anti-CTLA4")->melanoma_metadata_CTLA4_PRJNA82747_NR


clinical_info_new %>%
  dplyr::filter(Cancer == "lung cancer")%>%
  dplyr::filter( (Response %in% res) | (Second_Response_standard %in% res))%>%
  dplyr::filter(Anti_target == "anti-PD1")->lung_cancer_metadata_pd1_R

clinical_info_new %>%
  dplyr::filter(Cancer == "lung cancer")%>%
  dplyr::filter( (Response %in% nres) | (Second_Response_standard %in% nres))%>%
  dplyr::filter(Anti_target == "anti-PD1")->lung_cancer_metadata_pd1_NR


clinical_info_new %>%
  dplyr::filter(Cancer == "lung cancer")%>%
  dplyr::filter( (Response %in% res) | (Second_Response_standard %in% res))%>%
  dplyr::filter(Anti_target == "anti-PD1+anti-CTLA4")->lung_cancer_metadata_pd1_ctla4_R

clinical_info_new %>%
  dplyr::filter(Cancer == "lung cancer")%>%
  dplyr::filter( (Response %in% nres) | (Second_Response_standard %in% nres))%>%
  dplyr::filter(Anti_target == "anti-PD1+anti-CTLA4")->lung_cancer_metadata_pd1_ctla4_NR

for(sam in intersect(melanoma_metadata_CTLA4_PRJNA82747_R$sample_id,melanoma_metadata_CTLA4_PRJNA82747_NR$sample_id)){
  melanoma_metadata_CTLA4_PRJNA82747_NR=melanoma_metadata_CTLA4_PRJNA82747_NR[-which(melanoma_metadata_CTLA4_PRJNA82747_NR$sample_id==sam),]
}

for(sam in intersect(lung_cancer_metadata_pd1_R$sample_id,lung_cancer_metadata_pd1_NR$sample_id)){
  lung_cancer_metadata_pd1_NR=lung_cancer_metadata_pd1_NR[-which(lung_cancer_metadata_pd1_NR$sample_id==sam),]
}

all_sample=c(melanoma_metadata_pd1_R$sample_id,melanoma_metadata_pd1_NR$sample_id,
             melanoma_metadata_pd1_R1$sample_id,melanoma_metadata_pd1_NR1$sample_id,
             melanoma_metadata_CTLA4_PRJNA306070_R$sample_id,melanoma_metadata_CTLA4_PRJNA306070_NR$sample_id,
             melanoma_metadata_CTLA4_PRJNA82747_R$sample_id,melanoma_metadata_CTLA4_PRJNA82747_NR$sample_id,
             lung_cancer_metadata_pd1_R$sample_id,lung_cancer_metadata_pd1_NR$sample_id,
             lung_cancer_metadata_pd1_ctla4_R$sample_id,lung_cancer_metadata_pd1_ctla4_NR$sample_id)
mela_pd1_R=nrow(melanoma_metadata_pd1_R)
mela_pd1_nr=nrow(melanoma_metadata_pd1_NR)
mela_pd1_count=mela_pd1_R+mela_pd1_nr

mela_pd1_R1=nrow(melanoma_metadata_pd1_R1)
mela_pd1_nr1=nrow(melanoma_metadata_pd1_NR1)
mela_pd1_count1=mela_pd1_R1+mela_pd1_nr1

mela_ctla4_R1=nrow(melanoma_metadata_CTLA4_PRJNA306070_R)
mela_ctla4_NR1=nrow(melanoma_metadata_CTLA4_PRJNA306070_NR)
mela_ctla4_all1=mela_ctla4_R1+mela_ctla4_NR1

mela_ctla4_R2=nrow(melanoma_metadata_CTLA4_PRJNA82747_R)
mela_ctla4_NR2=nrow(melanoma_metadata_CTLA4_PRJNA82747_NR)
mela_ctla4_all2=mela_ctla4_R2+mela_ctla4_NR2

lung_pd1_r=nrow(lung_cancer_metadata_pd1_R)
lung_pd1_nr=nrow(lung_cancer_metadata_pd1_NR)
lung_pd1_all=lung_pd1_r+lung_pd1_nr

lung_ctla4_r=nrow(lung_cancer_metadata_pd1_ctla4_R)
lung_ctla4_nr=nrow(lung_cancer_metadata_pd1_ctla4_NR)
lung_ctla4_all=lung_ctla4_r+lung_ctla4_nr


TMB_df=data.frame(sample=all_sample,target=c(rep("Anti-PD1",mela_pd1_count),rep("Anti-PD1_1",mela_pd1_count1),rep("Anti-CTLA4",mela_ctla4_all1), rep("Anti-CTLA4_1",mela_ctla4_all2),
                                             rep("Anti-PD1",lung_pd1_all),rep("Anti-PD1+CTLA4",lung_ctla4_all)),
                  Cancer=c(rep("Melanoma",mela_pd1_count+mela_pd1_count1+mela_ctla4_all1+mela_ctla4_all2),rep("NSCLC",lung_pd1_all+lung_ctla4_all)),
                  Response=c(rep("R",mela_pd1_R),rep("NR",mela_pd1_nr),
                             rep("R",mela_pd1_R1),rep("NR",mela_pd1_nr1),
                             rep("R",mela_ctla4_R1),rep("NR",mela_ctla4_NR1),
                             rep("R",mela_ctla4_R2),rep("NR",mela_ctla4_NR2),
                             rep("R",lung_pd1_r),rep("NR",lung_pd1_nr),
                             rep("R",lung_ctla4_r),rep("NR",lung_ctla4_nr)),
                  TMB=sample_TMB[all_sample,]/30)
TMB_df=TMB_df[-which(TMB_df$TMB>500),]

ggplot(TMB_df, aes(x=factor(interaction(Cancer,target),levels=c("Melanoma.Anti-PD1","Melanoma.Anti-PD1_1","Melanoma.Anti-CTLA4","Melanoma.Anti-CTLA4_1","NSCLC.Anti-PD1","NSCLC.Anti-PD1+CTLA4")), y=TMB,fill=as.factor(interaction(Cancer,target)))) + 
  geom_violin(aes(alpha = 0.5,color=as.factor(interaction(Cancer,target))))+ geom_jitter(width =0.05,size=1,aes(colour=as.factor(interaction(Cancer,target)))) +scale_fill_manual(values = c("#64BD9C","#0c3866","#26A6C4","#6063AA","#F5B317","#EA6067"))+
  scale_color_manual(values = c("#64BD9C","#0c3866","#26A6C4","#6063AA","#F5B317","#EA6067"))+theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+guides(fill=FALSE,color=FALSE,alpha=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))

#density figure
y_peak <- which.max(density(TMB_df$TMB)$y)#找y值最大的
x_peak <- density(TMB_df$TMB)$x[y_peak]#找出最大的y所对应的x   
ggplot(TMB_df,aes(x=TMB,fill="Blue",color="Blue",alpha=0.8))+
  geom_density()+theme_bw() + scale_fill_manual(values="#3681B7")+
  geom_vline( aes(xintercept = x_peak,color="black"),linetype="dashed")+scale_color_manual(values=c("#FF6666","#3681B7"))+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+guides(fill=FALSE,color=FALSE,alpha=FALSE)

#TMB_predict_panel

#ICB_mut_ls
#sample_id=read.table("sample_id",header=F)
library(dplyr)
library(plyr)
lung_vcf=grep("vcf",all_sample)
mut_ls=list()
# all_gene=c()
# for(sample in all_sample[-lung_vcf]){
#   mut=read.table(paste("/home/miaoyr/ICB_project/data/WXS/mutation/TMB/sigmut/",sample,"_convert_vcf.txt.filter",sep=""),header=F,sep="\t")
#   tmp=data.frame(table(mut$V6))
# 
#   all_gene=unique(c(all_gene,as.vector(unlist(tmp$Var1))))
# }
mut_matrix_df=matrix(rep(0,length(all_gene)*length(all_sample)), nrow=length(all_sample), byrow=TRUE, dimnames=list(all_sample, all_gene))

for(sample in all_sample[-lung_vcf]){
  mut=read.table(paste("/home/miaoyr/ICB_project/data/WXS/mutation/TMB/sigmut/",sample,"_convert_vcf.txt.filter",sep=""),header=F,sep="\t")
  tmp=data.frame(table(mut$V6))
  mut_matrix_df[sample,tmp$Var1]=as.vector(unlist(tmp$Freq))
}

#lung_vcf
lung_vcf_sample=read.table("/home/miaoyr/ICB_project/data/WXS/mutation/TMB/lung_pd1_vcf_ls/vcf_file_ls",header=F)
for(sample in lung_vcf_sample$V1){
  mut=read.table(paste("/home/miaoyr/ICB_project/data/WXS/mutation/TMB/lung_pd1_vcf_ls/",sample,sep=""),header = F,sep="\t")
  tmp=data.frame(table(mut$V6))
  sample_name=paste(as.vector(unlist(strsplit(sample,"_")))[1],"_indel.vcf.gz",sep="")
  if(sample_name %in% row.names(mut_matrix_df)){
    mut_matrix_df[sample_name,tmp$Var1]=as.vector(unlist(mut_matrix_df[sample_name,tmp$Var1]))+as.vector(unlist(tmp$Freq))

  }
  #all_gene=unique(c(all_gene,as.vector(unlist(tmp$Var1))))
}

cancer_ls=c("coad","read","luad","stad","skcm")
mut_mat_ls=list()
TMB_ls=list()
mut_fre_ls=list()
for(cancer in cancer_ls){
  mt=read.table(paste("data/WXS/mutation/TCGA_mut/",cancer,"_mutation_matrix",sep=""),header=T,row.names = 1,sep="\t")
  mut_mat_ls[[cancer]]=mt
  tmb=apply(mt,1,sum)/30
  fre=apply(mt,2,function(x) sum(x)/nrow(mt))
  TMB_ls[[cancer]]=tmb
  mut_fre_ls[[cancer]]=fre
}
#lasso model
feature1=mut_fre_ls[["skcm"]][which(mut_fre_ls[["skcm"]]>0.1)]

#feature2=mut_fre_ls[["coad"]][which(mut_fre_ls[["coad"]]>0.04)]

feature3=mut_fre_ls[["luad"]][which(mut_fre_ls[["luad"]]>0.1)]

#feature4=mut_fre_ls[["read"]][which(mut_fre_ls[["read"]]>0.04)]

#feature5=mut_fre_ls[["stad"]][which(mut_fre_ls[["stad"]]>0.04)]
a=intersect(names(feature1),names(feature2))
b=intersect(a,names(feature3))
c=intersect(b,names(feature4))
d=intersect(c,names(feature5))
feature1=intersect(names(feature1),names(feature3))
#feature1=feature1[-which(names(feature1)=="Hugo_Symbol")]
#feature1=feature1[-which(names(feature1)=="Unknown")]
# feature1=feature1[-which(names(feature1)=="RP11.478B9.1")]
feature1=feature1[-which(feature1=="Hugo_Symbol")]
#feature1=feature1[-which(feature1=="Unknown")]
#feature1=feature1[-which(feature1=="Unknown")])
skcm_tmb=TMB_ls[["skcm"]]
skcm_mut_mt=mut_mat_ls[["skcm"]]
feature_p_value=c()
for(gene in feature1){
  mut=skcm_mut_mt[,gene]
  names(mut)=row.names(skcm_mut_mt)
  group<-ifelse(mut==0,1,2)
  norm=names(which(group==1))
  mutant=names(which(group==2))
  ratio=median(skcm_tmb[mutant])/median(skcm_tmb[norm])
  #tt=fisher.test(rbind(table(skcm_mut_mt[high,gene]),table(skcm_mut_mt[low,gene])))
  if(ratio>2.5){
    feature_p_value=c(feature_p_value,gene)
  }
}
#lasso
#feature_p_value=b
#lasso.mod = glmnet(as.matrix(skcm_mut_mt[,feature_p_value]),as.numeric(skcm_tmb), family = "gaussian", alpha = 1, nlambda = 50)
#ridge
library(MASS)
select(lm.ridge(as.numeric(skcm_tmb)~as.matrix(skcm_mut_mt[,feature_p_value]),lambda=seq(0,0.5,0.001)))
TMB=as.numeric(skcm_tmb)
df=data.frame(cbind(TMB,as.matrix(skcm_mut_mt[,feature_p_value])))
ridge_model=lm.ridge(TMB~.,df,lambda = 81)
for(cancer in cancer_ls){
  mut_mt=mut_mat_ls[[cancer]]
  diff_gene=setdiff(feature_p_value,colnames(mut_mt))
  if(length(diff_gene)>0){
    diff_mat=matrix(rep(0,nrow(mut_mt)*length(diff_gene)),nrow=nrow(mut_mt))
    colnames(diff_mat)=diff_gene
    row.names(diff_mat)=row.names(mut_mt)
    pre_mat=cbind(mut_mt,diff_mat)
  }else{
    pre_mat=mut_mt
  }
  #pre_result=predict(lasso.mod, as.matrix(pre_mat[,feature_p_value]),s=0.001 , type = "response")
  pre_result <- as.matrix(pre_mat[,feature_p_value]) %*% coef(ridge_model)[-1]+coef(ridge_model)[1]
  pre_result[which(pre_result<0)]=0
  cor_va=cor.test(pre_result[names(TMB_ls[[cancer]]),],TMB_ls[[cancer]])
  print(cancer)
  df=data.frame(TMB=TMB_ls[[cancer]],predict=pre_result[names(TMB_ls[[cancer]]),])
  p=round(cor_va$estimate,3)
  ggplot(df, aes(x=as.numeric(predict), y=as.numeric(TMB))) +
    geom_point(color="#F26522") +    # Use hollow circles
    geom_smooth(method=lm,   
                se=FALSE,color="gray")+ theme_bw()+theme(panel.border = element_blank(),panel.grid.major = element_blank(),
                                                         panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
    guides(fill=FALSE,color=FALSE,alpha=FALSE)+
    annotate("text", label =p, x = max(df$predict)-20, y =10, size = 5, colour = "black")+
    annotate("text", label =cancer, x = max(df$predict)/2, y =max(df$TMB)+10, size = 5, colour = "black")
  print(cor_va$estimate)
}
#ICB_data
#diff_gene=setdiff(feature_p_value,colnames(mut_matrix_df))

sample_item=list(mela_pd1=c(melanoma_metadata_pd1_R$sample_id,melanoma_metadata_pd1_NR$sample_id),
                 mela_pd1_1=c(melanoma_metadata_pd1_R1$sample_id,melanoma_metadata_pd1_NR1$sample_id),
                 mela_ctla41=c(melanoma_metadata_CTLA4_PRJNA306070_R$sample_id,melanoma_metadata_CTLA4_PRJNA306070_NR$sample_id),
                 mela_ctla42=c(melanoma_metadata_CTLA4_PRJNA82747_R$sample_id,melanoma_metadata_CTLA4_PRJNA82747_NR$sample_id),
                 lung_pd1=c(lung_cancer_metadata_pd1_R$sample_id,lung_cancer_metadata_pd1_NR$sample_id),
                 lung_ctla4=c(lung_cancer_metadata_pd1_ctla4_R$sample_id,lung_cancer_metadata_pd1_ctla4_NR$sample_id))
pre_result <- as.matrix(mut_matrix_df[,feature_p_value]) %*% coef(ridge_model)[-1]+coef(ridge_model)[1]
#pre_result[which(pre_result<0)]=0
TMB_df=data.frame(sample=all_sample,target=c(rep("Anti-PD1",mela_pd1_count),rep("Anti-PD1_1",mela_pd1_count1),rep("Anti-CTLA4",mela_ctla4_all1), rep("Anti-CTLA4_1",mela_ctla4_all2),
                                             rep("Anti-PD1",lung_pd1_all),rep("Anti-PD1+CTLA4",lung_ctla4_all)),
                  Cancer=c(rep("Melanoma",mela_pd1_count+mela_pd1_count1+mela_ctla4_all1+mela_ctla4_all2),rep("NSCLC",lung_pd1_all+lung_ctla4_all)),
                  Response=c(rep("R",mela_pd1_R),rep("NR",mela_pd1_nr),
                             rep("R",mela_pd1_R1),rep("NR",mela_pd1_nr1),
                             rep("R",mela_ctla4_R1),rep("NR",mela_ctla4_NR1),
                             rep("R",mela_ctla4_R2),rep("NR",mela_ctla4_NR2),
                             rep("R",lung_pd1_r),rep("NR",lung_pd1_nr),
                             rep("R",lung_ctla4_r),rep("NR",lung_ctla4_nr)),
                  TMB=sample_TMB[all_sample,]/30)
#TMB_df_new=TMB_df[-c(248,266),]
TMB_df_new=TMB_df
row.names(TMB_df_new)=TMB_df_new$sample
for(item in names(sample_item)){
  sam_ls=sample_item[[item]]
  cor_va=cor.test(pre_result[sam_ls,],TMB_df_new[sam_ls,"TMB"])
  df=data.frame(TMB=TMB_df_new[sam_ls,"TMB"],predict=pre_result[sam_ls,])
  p=round(cor_va$estimate,3)
  ggplot(df, aes(x=as.numeric(predict), y=as.numeric(TMB))) +
    geom_point(color="#F26522") +    # Use hollow circles
    geom_smooth(method=lm,   
                se=FALSE,color="gray")+ theme_bw()+theme(panel.border = element_blank(),panel.grid.major = element_blank(),
                                 panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
    guides(fill=FALSE,color=FALSE,alpha=FALSE)+
    annotate("text", label =p, x = max(df$predict)-20, y =10, size = 5, colour = "black")+
    annotate("text", label =item, x = max(df$predict)/2, y =max(df$TMB)+10, size = 5, colour = "black")
  print(cor_va$estimate)
}


#R/NR boxplot
library(ggpubr)
my_comparisons=list(c("Melanoma.Anti-PD1.R","Melanoma.Anti-PD1.NR"),c( "Melanoma.Anti-PD1_1.R","Melanoma.Anti-PD1_1.NR"),
                   c("Melanoma.Anti-CTLA4.R","Melanoma.Anti-CTLA4.NR"),c("Melanoma.Anti-CTLA4_1.R","Melanoma.Anti-CTLA4_1.NR"),
                   c("NSCLC.Anti-PD1.R","NSCLC.Anti-PD1.NR"),c( "NSCLC.Anti-PD1+CTLA4.R","NSCLC.Anti-PD1+CTLA4.NR"))
TMB_df_new$predicted_value=pre_result[row.names(TMB_df_new),]
TMB_df_new=TMB_df_new[-which(TMB_df_new$TMB>400),]
ggplot(TMB_df_new, aes(x=factor(interaction(interaction(Cancer,target),Response),levels = c("Melanoma.Anti-PD1.R","Melanoma.Anti-PD1.NR",
                                                                                            "Melanoma.Anti-PD1_1.R","Melanoma.Anti-PD1_1.NR",
                                                                                            "Melanoma.Anti-CTLA4.R","Melanoma.Anti-CTLA4.NR",
                                                                                            "Melanoma.Anti-CTLA4_1.R","Melanoma.Anti-CTLA4_1.NR",
                                                                                            "NSCLC.Anti-PD1.R","NSCLC.Anti-PD1.NR",
                                                                                            "NSCLC.Anti-PD1+CTLA4.R","NSCLC.Anti-PD1+CTLA4.NR")), y=TMB,fill=as.factor(Response))) + 
  geom_boxplot()+scale_fill_manual(values = c("red","black"))+theme_bw() + stat_compare_means(comparisons = my_comparisons,label.y = 200)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+guides(fill=FALSE,color=FALSE,alpha=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))



ggplot(TMB_df_new, aes(x=factor(interaction(interaction(Cancer,target),Response),levels = c("Melanoma.Anti-PD1.R","Melanoma.Anti-PD1.NR",
                                                                                            "Melanoma.Anti-PD1_1.R","Melanoma.Anti-PD1_1.NR",
                                                                                            "Melanoma.Anti-CTLA4.R","Melanoma.Anti-CTLA4.NR",
                                                                                            "Melanoma.Anti-CTLA4_1.R","Melanoma.Anti-CTLA4_1.NR",
                                                                                            "NSCLC.Anti-PD1.R","NSCLC.Anti-PD1.NR",
                                                                                            "NSCLC.Anti-PD1+CTLA4.R","NSCLC.Anti-PD1+CTLA4.NR")), y=predicted_value,fill=as.factor(Response))) + 
  geom_boxplot()+scale_fill_manual(values = c("red","black"))+theme_bw() + stat_compare_means(comparisons = my_comparisons,label.y = 200)+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+guides(fill=FALSE,color=FALSE,alpha=FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))



#neoantigen
#neoantigen_fs_mt=matrix(rep(0,length(all_gene)*length(all_sample)), nrow=length(all_sample), byrow=TRUE, dimnames=list(all_sample, all_gene))
mut_type_ls=c()
lung_vcf=grep("vcf",all_sample)
for(sample in all_sample[-lung_vcf]){
  mut=read.table(paste("sigmut/",sample,"_convert_vcf.txt.filter",sep=""),header=F,sep="\t")
  tmp=cbind(rep(sample,nrow(mut)),mut$V6,mut$V12)
  mut_type_ls=rbind(mut_type_ls,tmp)
}
colnames(mut_type_ls)=c("Sample","Gene","type")
mut_type_ls=data.frame(mut_type_ls)
#neo_ls=c()
neo_file=read.table("/home/miaoyr/ICB_project/data/WXS/neoantigen/phase1_phase2_phase3_neoantigen_result/all_neoantigen_SB_WB_final_new_title",header=T,sep="\t")
neo_file_df=data.frame(neo_file)
neo_file%>%filter(sample %in% row.names(TMB_df_new))->neo_file_filter
neoantigen=rep(0,nrow(TMB_df))
FS=rep(0,nrow(TMB_df))
missense=rep(0,nrow(TMB_df))
inframe_ins=rep(0,nrow(TMB_df))
inframe_del=rep(0,nrow(TMB_df))
neo_TMB_df=cbind(TMB_df,neoantigen,FS,missense,inframe_ins,inframe_del)
row.names(neo_TMB_df)=neo_TMB_df$sample
neo_TMB_df=as.matrix(neo_TMB_df)

for(i in seq(1,nrow(neo_file_filter))){
  line=neo_file_filter[i,]
  sam=line$sample
  gene=line$Gene
  mut_type_ls %>% 
    filter(Gene == gene)%>%
    filter( Sample %in% sam )-> mut_info
  mut_type=unique(mut_info[,3])
  neo_TMB_df[sam,"neoantigen"]=as.numeric(neo_TMB_df[sam,"neoantigen"])+1
  neo_TMB_df[sam,mut_type]= as.numeric(neo_TMB_df[sam,mut_type])+1
}


#pheatmap

neo_TMB_df_new=data.frame(neo_TMB_df)
neo_TMB_df_new$pre_TMB=pre_result[neo_TMB_df_new$sample,]
neo_TMB_df_new=neo_TMB_df_new[,c(1,2,3,4,5,11,6,7,8,9,10)]
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="Melanoma")%>%
  dplyr::filter(target=="Anti-PD1")->mela_pd1 
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="Melanoma")%>%
  dplyr::filter(target=="Anti-PD1_1")->mela_pd1_1
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="Melanoma")%>%
  dplyr::filter(target=="Anti-CTLA4")->mela_ctla4
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="Melanoma")%>%
  dplyr::filter(target=="Anti-CTLA4_1")->mela_ctla4_1
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="NSCLC")%>%
  dplyr::filter(target=="Anti-PD1")->nsclc_pd1
neo_TMB_df_new%>%
  dplyr::filter(Cancer=="NSCLC")%>%
  dplyr::filter(target=="Anti-PD1+CTLA4")->nsclc_pd1_ctla4
data_ls=list(mela1=mela_pd1,mela2=mela_pd1_1,mela3=mela_ctla4,mela4=mela_ctla4_1,nsclc1=nsclc_pd1,nsclc2=nsclc_pd1_ctla4)
cor_mt=c()
for(tag in names(data_ls)){
  data=data_ls[[tag]]
  r_index=which(data$Response=="R")
  nr_index=which(data$Response=="NR")
  p_ls<-c()
  for(n in colnames(data)[5:11]){
    r=as.numeric(data[r_index,n])
    nr=as.numeric(data[nr_index,n])
    a=wilcox.test(r,nr)
    p_ls<-c(p_ls,a$p.value)
  }
  # print(p_ls)
  # print(tag)
  cor_mt<-rbind(cor_mt,p_ls)
}
row.names(cor_mt)=c("Melanoma.aPD1","Melanoma.aPD1_1","Melanoma.aCTLA4","Melanoma.aCTLA4_1","NSCLC.aPD1","NSCLC.aPD1+aCTLA4")
colnames(cor_mt)=c("TMB","TMB(predicted)","Neoantigen","Neoantigen(FS)","Neoantigen(missen)","Neoantigen(inframe_ins)","Neoantigen(inframe_del)")
cor_mt[cor_mt>0.1]<-0.5
pheatmap::pheatmap(cor_mt,color = colorRampPalette(c("#E95F5C", "gray", "#F3F4F7"))(50),display_numbers=TRUE,number_format="%.4f",cluster_rows = F,cluster_cols = F)



save.image(file="ICB_figure_code.rda",ascii=FALSE,compress=TRUE)




