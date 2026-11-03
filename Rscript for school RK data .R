
#Clear memory and plots==================
if(!is.null(dev.list())) dev.off()
rm(list=ls())
cat("\014")

#Libraries==========

#Data management and graphs
library(tcltk)
library(viridis)
library(haven)
library(tidyverse)
library(gridExtra)
library(rmarkdown)
library(pdftools)
library(labelled)
library(viridis)
library(readxl)
library(janitor)
library(car)
library(rcompanion)

#For data analysis
library(lme4)
library(ordinal)
library(rstan)
library(rstantools)
library(brms)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

#wd==========
new_wd<-tk_choose.dir()
setwd(new_wd)



#custom unctions========================


at_ref=function(x,k=1){ rep(as.factor(levels(as.factor(x))[1]),k)}
at_median=function(x,k=1){ rep(median(x),k)}
at_mean=function(x,k=1){ rep(mean(x),k)}
at_value=function(x,k=1){ seq(min(x),max(x),length.out=5)}
at_facet=function(x,k=1){rep(as.factor(levels(x)),each=k)}


scaled_vec<-function(y){
  z<-as.vector(scale((as.numeric(y))))
}



theme_set(theme_bw())



#Load Data=======================
df<-read_spss(tk_choose.files())

#Data preperation=======================

df$schoolid<-df$Skolkod


df$energydrink<-as.factor(5-df$EnergiDryck)
df$energydrink<-factor(df$energydrink,labels = c("Never","Once or twice per week","Several time per week","Everyday"))
df$energydrink<-as.ordered(df$energydrink)


df$schoolstatus<-11-df$StatusSkola
df$familystatus<-11-df$StatusFam

df<-df%>%group_by(schoolid)%>%mutate(SCHOOL_schoolstatus=mean(schoolstatus,na.rm=T))%>%ungroup()
df<-df%>%group_by(schoolid)%>%mutate(SCHOOL_familystatus=mean(familystatus,na.rm=T))%>%ungroup()

df$z_schoolstatus<-scaled_vec(df$schoolstatus)
df$z_familystatus<-scaled_vec(df$familystatus)
df$z_SCHOOL_familystatus<-scaled_vec(df$SCHOOL_familystatus)
df$z_SCHOOL_schoolstatus<-scaled_vec(df$SCHOOL_schoolstatus)


df$genderid<-ifelse(df$Kön==1,"boy","girl")
df$migrationid<-as.factor(ifelse(df$UrsprungSjälv==1,"native","migrant"))
df$friends<-as.factor(df$VännerAntal)
lab=c("none","one","two","more than two")
df$friends<-factor(df$friends,labels = lab)
df$household<-NA
df$household[df$Boende==1]<-"BothParents"
df$household[df$Boende==2 |df$Boende==3|df$Boende==4]<-"SingleParents"
df$household[df$Boende==5 |df$Boende==6]<-"Other"
df$household<-as.factor(df$household)
df$familyeconomic<-as.factor(6-df$FamEkKöpa)
df$familyeconomic<-factor(df$familyeconomic,labels = c("Never","Seldom","Sometimes", "often","Always"))


#Descriptive plots======================
gdat<-as.data.frame(with(df,proportions(table(energydrink))))
g0<-ggplot(gdat,aes(energydrink,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Outcome")+xlab("Energy drink \n consumption")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(household))))
g1<-ggplot(gdat,aes(household,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Explantory")+xlab("Household")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(familyeconomic))))
g2<-ggplot(gdat,aes(familyeconomic,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Adjusting")+xlab("Family Economic")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(friends))))
g3<-ggplot(gdat,aes(friends,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Adjusting")+xlab("Friends ")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(genderid))))
g4<-ggplot(gdat,aes(genderid,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Adjusting")+xlab("Gender(Sex identification)")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(migrationid))))
g5<-ggplot(gdat,aes(migrationid,Freq,ymin=0,ymax=Freq))+geom_bar(stat = "identity")+geom_point()+
  labs(captions="Adjusting")+xlab("Migration status")+ylab("Proportions")+coord_flip()+theme_bw()


gdat<-as.data.frame(with(df,proportions(table(familystatus))))
g6<-ggplot(gdat,aes(familystatus,Freq,group=1,ymin=0,ymax=Freq))+geom_pointrange()+#geom_line()
  labs(captions="Explantory")+xlab("Self-rated Family Status")+ylab("Proportions")+coord_flip()+theme_bw()
gdat<-as.data.frame(with(df,proportions(table(schoolstatus))))
g7<-ggplot(gdat,aes(schoolstatus,Freq,group=1,ymin=0,ymax=Freq))+geom_pointrange()+#geom_line()
  labs(captions="Explantory")+xlab("Self-rated School Community  Status")+ylab("Proportions")+coord_flip()+theme_bw()

g7

plotdat<-df%>%group_by(schoolid)%>%summarise(familystatus=mean(familystatus,na.rm=T),
                                             schoolstatus=mean(schoolstatus,na.rm=T),
                                             energydrink=mean(as.numeric(energydrink),na.rm=T)
)


g8<-ggplot(plotdat,aes(scale(energydrink),scale(schoolstatus)))+geom_point()+geom_smooth(method="lm",se=F,col="black")+
  theme_bw()+xlab("Self-rated School Community Status")+ylab("Energy Drink Consumption")+labs(captions="Aggregated Regression by Schools") +
  geom_vline(xintercept=0,linetype=2)+geom_hline(yintercept=0,linetype=2) 

g9<-ggplot(plotdat,aes(scale(energydrink),scale(familystatus)))+geom_point()+
  geom_smooth(method="lm",se=F,,col="black")+theme_bw()+xlab("Self-rated Family Status")+ylab("Energy Drink Consumption")+labs(captions="Aggregated Regression by Schools")+
  geom_vline(xintercept=0,linetype=2)+geom_hline(yintercept=0,linetype=2) 

g10<-ggplot(plotdat,aes(x=reorder(x=schoolid,schoolstatus),y=schoolstatus,ymin=0,ymax=schoolstatus))+
  geom_point()+geom_pointrange()+xlab("Schools")+ylab("School Average School Community Status")+labs(caption="School Averages (ordered)")+theme_bw()+theme(
    axis.text.x=element_blank()  )
g11<-ggplot(plotdat,aes(x=reorder(x=schoolid,familystatus),y=familystatus,ymin=0,ymax=familystatus))+
  geom_point()+geom_pointrange()+xlab("Schools")+ylab("School Average Family Status")+labs(caption="School Averages (ordered)")+theme_bw()+theme(
    axis.text.x=element_blank()
  )


gridExtra::grid.arrange(g0,g1,g2,g3,g4,g5)
gridExtra::grid.arrange(g6,g7,g10,g11)
gridExtra::grid.arrange(g8,g9)
pdf("descplotsenegery.pdf")
gridExtra::grid.arrange(g0,g1,g2,g3,g4,g5)
dev.off()
pdf("descplotsenegery_stat.pdf")
gridExtra::grid.arrange(g6,g7,g10,g11)
dev.off()
pdf("descplotsenegery_aggregate.pdf")
gridExtra::grid.arrange(g8,g9)
dev.off()




pdf_convert(pdf ="descplotsenegery.pdf",format = "jpeg",dpi = 300)
pdf_convert(pdf ="descplotsenegery_aggregate.pdf",format = "jpeg",dpi = 300)
pdf_convert(pdf ="descplotsenegery_stat.pdf",format = "jpeg",dpi = 300)

#Fit models=================






summary(brmsfit1<-brm(bf(energydrink~migrationid+genderid+household+friends+familyeconomic++z_schoolstatus+z_familystatus+(1|schoolid),family = cumulative()),data=df))
summary(brmsfit2<-brm(bf(energydrink~migrationid+genderid+household+friends+familyeconomic+z_schoolstatus+z_familystatus+z_SCHOOL_schoolstatus+z_SCHOOL_familystatus+(1|schoolid),family = cumulative()),data=df))




save(brmsfit1,file="brmsenergy1")
save(brmsfit2,file="brmsenergy2")




load(file="brmsenergy1")
load(file="brmsenergy2")

#make predictive plots======================

newdat=with(brmsfit2$data,data.frame(
  migrationid=at_ref(migrationid),
  genderid=at_ref( genderid),
  household=at_ref(household),
  friends=at_ref(friends),
  familyeconomic=at_ref(familyeconomic),
  z_schoolstatus=at_mean(z_schoolstatus),
  z_familystatus=at_mean(z_familystatus),
  z_SCHOOL_schoolstatus=at_mean(z_SCHOOL_schoolstatus),
  z_SCHOOL_familystatus=-2:2,
  schoolid=at_ref(schoolid)
)
)

pred_fam=predict(brmsfit2,newdat)
pred_fam<-reshape2::melt(pred_fam)

pred_fam<-pred_fam%>% rename(Predictor=Var1,Outcome=Var2,Value=value)
pred_fam$Predictor<-pred_fam$Predictor-3
gp1=ggplot(pred_fam,aes(Predictor,Value, linetype=Outcome,shape=Outcome))+geom_line()+geom_point()+
  xlab("School average family status (z-score)")+
  ylab("Estimated predicted probability for \n energy drink consumption")+
  theme_bw()+geom_vline(xintercept = 0,alpha=.4,col="red",linetype=8)+labs(caption="Estimated from Model 2")


newdat=with(brmsfit2$data,data.frame(
  migrationid=at_ref(migrationid),
  genderid=at_ref( genderid),
  household=at_ref(household),
  friends=at_ref(friends),
  familyeconomic=at_ref(familyeconomic),
  z_schoolstatus=at_mean(z_schoolstatus),
  z_familystatus=at_mean(z_familystatus),
  z_SCHOOL_schoolstatus=-2:2,
  z_SCHOOL_familystatus=at_mean(z_SCHOOL_familystatus),
  schoolid=at_ref(schoolid)
)
)

pred_fam=predict(brmsfit2,newdat)
pred_fam<-reshape2::melt(pred_fam)

pred_fam<-pred_fam%>% rename(Predictor=Var1,Outcome=Var2,Value=value)
pred_fam$Predictor<-pred_fam$Predictor-3
gp2=ggplot(pred_fam,aes(Predictor,Value, linetype=Outcome,shape=Outcome))+geom_line()+geom_point()+
  xlab("School average school community status (z-score)")+
  ylab("Estimated predicted probability for \n energy drink consumption")+
  theme_bw()+geom_vline(xintercept = 0,alpha=.4,col="red",linetype=8)+labs(caption="Estimated from Model 2")


grid.arrange(gp1,gp2)

pdf("estpredpolts.pdf")
grid.arrange(gp1,gp2)
dev.off()
pdf_convert(pdf ="estpredpolts.pdf",format = "jpeg",dpi = 300)






#Make coef plots=========================
coefplots=data.frame(coef=summary(brmsfit1)$fixed[,1],
                     ymin=summary(brmsfit1)$fixed[,3],
                     ymax=summary(brmsfit1)$fixed[,4],
                      predictors=row.names(summary(brmsfit1)$fixed))
coefplots$ModelID<-"Model1"
coefplots<-coefplots[-c(1,2,3),]

predlab=c(
  "Migration: Native",
  "Gender: Girl",
  "Household: Other",
  "Household: Single Parents",
  "Friends:One",
  "Friends: Two",
  "Friends: More than two",
  "Family Economic: Seldom",
  "Family Economic: Sometimes",
  "Family Economi: Often",
  "Family Economic: Always",
  "Community Status (z-standardized)",
  "Family Status (z-standardized)"


)

coefplots$predictors<-as.factor(coefplots$predictors)
coefplots$predictors<-factor(coefplots$predictors,labels = predlab)


coefplots2=data.frame(coef=summary(brmsfit2)$fixed[,1],
                      ymin=summary(brmsfit2)$fixed[,3],
                      ymax=summary(brmsfit2)$fixed[,4],
                      predictors=row.names(summary(brmsfit2)$fixed))
coefplots2$ModelID<-"Model2"
coefplots2<-coefplots2[-c(1,2,3),]

predlab=c(
  "Migration: Native",
  "Gender: Girl",
  "Household: Other",
  "Household: Single Parents",
  "Friends:One",
  "Friends: Two",
  "Friends: More than two",
  "Family Economic: Seldom",
  "Family Economic: Sometimes",
  "Family Economi: Often",
  "Family Economic: Always",
  "Community Status (z-standardized)",
  "Family Status (z-standardized)",
  "School Average School Community Status (z-standardized)",
  "School Average  Family Status (z-standardized)"
  
)


coefplots2$predictors<-as.factor(coefplots2$predictors)
coefplots2$predictors<-factor(coefplots2$predictors,labels = predlab)
ggcoefplot<-full_join(coefplots,coefplots2,by=c("coef","ymin","ymax","predictors","ModelID"))



            

levels(ggcoefplot$predictors)

Modellab=c("Model 1", "Model 2")
ggcoefplot$ModelID<-factor(ggcoefplot$ModelID,labels = Modellab)

ggcoefplot$Level<-ifelse(ggcoefplot$predictors== "School Average School Community Status (z-standardized)"|                                             
                           ggcoefplot$predictors=="School Average  Family Status (z-standardized)",
                         "Level 2:School","Level 1: Student")

rand<-c(sd1=summary(brmsfit1)$rand$schoolid[c(1)],sd2=summary(brmsfit2)$rand$schoolid[c(1)])

ggcoef1<-ggplot(ggcoefplot,aes(predictors,coef,ymin=ymin,ymax=ymax,group=ModelID,col=Level))+geom_pointrange()+
  coord_flip()+theme_bw()+geom_hline(yintercept=0,linetype=2)+facet_grid(.~ModelID)+scale_color_manual(
    values = c( "grey","black")  )+
  ylab("Estimated Coefficients (Outcome: Energy Drink Consumption)")+xlab("Predictors")+labs(caption=c(paste0( "N Students=",Students, "\n N Schools=",Schools,
                                                                                                               "\n Model 1 SD(Intercept)=",round(rand$sd1.Estimate,digits=2),
                                                                                                               "\n Model 2 SD(Intercept)=",round(rand$sd2.Estimate,digits=2)))
                                                                   ,title="Multilevel ordinal logit regression",subtitle="Model 1:  Student Level\n Model 2: School Level (Averages)"
                                                                   )+theme(axis.text.y = element_text( size = 5),
                                                                           axis.text.x = element_text( size = 5),
                                                                           axis.title.y = element_text( size = 8),
                                                                           axis.title.x = element_text( size = 8),
                                                                           legend.text=element_text(size=8),
                                                                           plot.title = element_text( size = 12),
                                                                           plot.subtitle = element_text( size = 9))

   

ggcoef1

pdf("coefplotschools.pdf")
ggcoef1
dev.off()


pdf_convert(pdf ="coefplotschools.pdf",format = "jpeg",dpi = 300)


#make diagnostics plots======================

gpp1<-pp_check(brmsfit1,ndraw=50)
gpp2<-pp_check(brmsfit2,ndraw=50)



loo1<-loo(brmsfit1)
loo2<-loo(brmsfit2)
loo_compare(loo1,loo2)

loocom<-loo_compare(loo1,loo2)
fitdat=data.frame(Models=c("Model 1", "Model 2"),
                  fitstat=c(loo1$estimates[3,1],loo2$estimates[3,1]))

ggloo<-ggplot(fitdat,aes(Models,fitstat,group=1))+geom_point()+theme_bw()+
  geom_line()+scale_color_viridis_d()+xlab("Models")+ylab("LOO Information Criterion")

pdf("schooldiagnostics.pdf")
gridExtra::grid.arrange(gpp1,gpp2,ggloo,nrow=2,
                        top = "Model fit: Posterior predictive checks and LOO Information Criterion"
                       )
dev.off()
pdf_convert(pdf ="schooldiagnostics.pdf",format = "jpeg",dpi = 300)
