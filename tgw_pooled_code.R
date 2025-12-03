library(dplyr)
library(questionr)
library(spatstat)
setwd("C:/Users/sweta/OneDrive - Emory University/Emory 2023-2024/CDC/TGW Pooled Analysis/CRANE3_IBBS_TGW_DATA/Weighted Datasets")
arua<-read.csv("CRANE3_IBBS_TGW_ARUA_03122024.csv")
arua<-mutate_all(arua,as.character)
fp<-read.csv("CRANE3_IBBS_TGW_FORTPORTAL_03122024.csv")
fp<-mutate_all(fp,as.character)
gulu<-read.csv("CRANE3_IBBS_TGW_GULU_09192023.csv")
gulu<-mutate_all(gulu,as.character)
jinj<-read.csv("CRANE3_IBBS_TGW_JINJA_03112024.csv")
jinj<-mutate_all(jinj,as.character)
kamp<-read.csv("CRANE3_IBBS_TGW_KAMPALA_09042023.csv")
kamp<-mutate_all(kamp,as.character)
lira<-read.csv("CRANE3_IBBS_TGW_LIRA_03132024.csv")
lira<-mutate_all(lira,as.character)
masa<-read.csv("CRANE3_IBBS_TGW_MASAKA_03112024.csv")
masa<-mutate_all(masa,as.character)
mbar<-read.csv("CRANE3_IBBS_TGW_MBARARA_03112024.csv")
mbar<-mutate_all(mbar,as.character)
all<-bind_rows(arua,fp,gulu,jinj,kamp,lira,masa,mbar)
all$weights<-as.numeric(all$weights)

#DEMOGRAPHICS

#Ugandan National
all$weights<-as.numeric(all$weights)
prop.table(wtd.table(all$tutnat, weights = all$weights))
table(all$tutnat)

#Has financial dependents
prop.table(wtd.table(all$dmdep, weights = all$weights))
table(all$dmdep)

#Currently married
prop.table(wtd.table(all$dmmr, weights = all$weights))
table(all$dmmr)

#Lives with spouse/partner
prop.table(wtd.table(all$dmpart, weights = all$weights))
table(all$dmpart)

#Ever been homeless
prop.table(wtd.table(all$tuthln, weights = all$weights))
table(all$tuthln)

#Finished secondary school
prop.table(wtd.table(all$tutedu, weights = all$weights))
table(all$tutedu)

#Age 
prop.table(wtd.table(all$elage, weights = all$weights))
table(all$elage)

#IQR age
weighted.quantile(all$elage,all$weights, probs=seq(0,1,0.25), na.rm = TRUE)

#TGW CHARACTERISTICS
#Used or uses hormones
tghmuse_nona<-filter(all,tghmuse==1|tghmuse==2|tghmuse==3)
prop.table(wtd.table(tghmuse_nona$tghmuse, weights = tghmuse_nona$weights))
table(tghmuse_nona$tghmuse)

#Shared needles
tghminj_nona<-filter(all,tghminj==1|tghminj==2|tghminj==3)
prop.table(wtd.table(tghminj_nona$tghminj, weights = tghminj_nona$weights))
table(tghminj_nona$tghminj)

#Uses birth control for hormones
tghmbc_nona<-filter(all,tghmbc==1|tghmbc==2|tghmbc==3)
prop.table(wtd.table(tghmbc_nona$tghmbc, weights = tghmbc_nona$weights))
table(tghmbc_nona$tghmbc)

#Enhancement/transition procedure
tgsrg_nona<-filter(all,tgsrg==1|tgsrg==2)
prop.table(wtd.table(tgsrg_nona$tgsrg, weights = tgsrg_nona$weights))
table(tgsrg_nona$tgsrg)

#Surgically constructed vagina
tgvag_nona<-filter(all,tgvagina==1|tgvagina==2)
prop.table(wtd.table(tgvag_nona$tgvagina, weights = tgvag_nona$weights))
table(tgvag_nona$tgvagina)

#TYPES OF HORMONES USED
#Injection
inj_nona<-filter(all,tghmtyxa==0|tghmtyxa==1)
prop.table(wtd.table(inj_nona$tghmtyxa, weights = inj_nona$weights))
table(inj_nona$tghmtyxa)

#Gel
gel_nona<-filter(all,tghmtyxb==0|tghmtyxb==1)
prop.table(wtd.table(gel_nona$tghmtyxb, weights = gel_nona$weights))
table(gel_nona$tghmtyxb)

#Pills
pill_nona<-filter(all,tghmtyxc==0|tghmtyxc==1)
prop.table(wtd.table(pill_nona$tghmtyxc, weights = pill_nona$weights))
table(pill_nona$tghmtyxc)

#Patch
pat_nona<-filter(all,tghmtyxd==0|tghmtyxd==1)
prop.table(wtd.table(pat_nona$tghmtyxd, weights = pat_nona$weights))
table(pat_nona$tghmtyxd)

#Other
oth_nona<-filter(all,tghmtyxe==0|tghmtyxe==1)
prop.table(wtd.table(oth_nona$tghmtyxe, weights = oth_nona$weights))
table(oth_nona$tghmtyxe)

#TYPE OF GENDER TRANSITION PROCEDURE
tg_nona<-filter(all,tgsrgty==1|tgsrgty==2|tgsrgty==3|tgsrgty==4|tgsrgty==5)
prop.table(wtd.table(tg_nona$tgsrgty, weights = tg_nona$weights))
table(tg_nona$tgsrgty)

#ALCOHOL, DRUG USE, DEPRESSION
#Alcohol misuse
prop.table(wtd.table(all$alcscr2, weights = all$weights))
table(all$alcscr2)

#Drug use ever
prop.table(wtd.table(all$drev, weights = all$weights))
table(all$drev)

#Injected Drugs ever
driev_nona<-filter(all,driev==1|driev==2|driev==3)
prop.table(wtd.table(driev_nona$driev, weights = driev_nona$weights))
table(driev_nona$driev)

#ENACTED STIGMA AGAINST TGW

#Healthcare discrimination
hcw<-filter(all,tgsthc1==1|tgsthc2==1|tgsthc3==1)
hcw.n<-select(hcw,tgsthc1,tgsthc2,tgsthc3,weights)
hcw.n
hcw.d<-filter(all,tgsthc1!="NA"|tgsthc2!="NA",tgsthc3!="NA")
hcw.d<-select(hcw.d,tgsthc1,tgsthc2,tgsthc3,weights)
hcw.d
(sum(hcw.n$weights))/(sum(hcw.d$weights))

#Discrimination by family or friends
fam.n<-filter(all,tgstfam1==1|tgstfam2==1|tgstfam3==1)
fam.n<-select(fam.n,tgstfam1,tgstfam2,tgstfam3,weights)
fam.n
fam.d<-filter(all,tgstfam1!="NA"|tgstfam2!="NA",tgstfam3!="NA")
fam.d<-select(fam.d,tgstfam1,tgstfam2,tgstfam3,weights)
fam.d
(sum(fam.n$weights))/(sum(fam.d$weights))

#Discrimination by police
pol.n<-filter(all,tgstuni1==1|tgstuni2==1|tgstuni3==1)
pol.n<-select(pol.n,tgstuni1,tgstuni2,tgstuni3,weights)
pol.n
pol.d<-filter(all,tgstuni1!="NA"|tgstuni2!="NA",tgstuni3!="NA")
pol.d<-select(pol.d,tgstuni1,tgstuni2,tgstuni3,weights)
pol.d
(sum(pol.n$weights))/(sum(pol.d$weights))

# ANTICIPATED TGW STIGMA

#Afraid seeking healthcare, tgst1 =1
prop.table(wtd.table(all$tgst1, weights = all$weights))
table(all$tgst1)

#Not seeking healthcare, tgst2 =1
prop.table(wtd.table(all$tgst2, weights = all$weights))
table(all$tgst2)

#Avoid carrying condoms, tgst3 =1
prop.table(wtd.table(all$tgst3, weights = all$weights))
table(all$tgst3)

#Avoid public places, tgst4 =1
prop.table(wtd.table(all$tgst4, weights = all$weights))
table(all$tgst4)

#Been blackmailed, tgst =5
prop.table(wtd.table(all$tgst5, weights = all$weights))
table(all$tgst5)

#INTERNALIZED TG STIGMA

#Cannot face family and friends, tgstin1 = 1 and 2 (strongly agree and agree)
prop.table(wtd.table(all$tgstin1, weights = all$weights))
table(all$tgstin1)

#Feel ashamed of being a TGW, tgstin2 = 1 and 2 (strongly agree and agree)
prop.table(wtd.table(all$tgstin2, weights = all$weights))
table(all$tgstin2)

#Feel guilty for identifying as TGW, tgstin3 = 1 and 2
prop.table(wtd.table(all$tgstin3, weights = all$weights))
table(all$tgstin3)

#Blame myself for identifying as TGW, tgstin4 = 1 and 2
prop.table(wtd.table(all$tgstin4, weights = all$weights))
table(all$tgstin4)

#No self-worth, tgstin5 = 1 and 2
prop.table(wtd.table(all$tgstin5, weights = all$weights))
table(all$tgstin5)

#Suicidal ideation, tgstin6 = 1 and 2
prop.table(wtd.table(all$tgstin6, weights = all$weights))
table(all$tgstin6)

#Deserve bad things that happen because TGW, tgstin7 = 1 and 2
prop.table(wtd.table(all$tgstin7, weights = all$weights))
table(all$tgstin7)

#MORTALITY AMONG PEERS
mort<-all%>%
  filter(rcdgcod1!=9&rcdgcod1!=998)%>%
  select(rcdgcod1,weights)
prop.table(wtd.table(mort$rcdgcod1,weights=mort$weights))
table(mort$rcdgcod1)

#OUTREACH SERVICES
#last 6 mos, >6 mos
prop.table(wtd.table(all$outexp, weights = all$weights))
table(all$outexp)

#Condoms
osa_nona<-filter(all,outserva!=998)
table(osa_nona$outserva)
prop.table(wtd.table(osa_nona$outserva,weights=osa_nona$weights))

#Lubricants
osb_nona<-filter(all,outservb!=998)
table(osb_nona$outservb)
prop.table(wtd.table(osb_nona$outservb,weights=osb_nona$weights))

#HIV Testing
osc_nona<-filter(all,outservc!=998)
table(osc_nona$outservc)
prop.table(wtd.table(osc_nona$outservc,weights=osc_nona$weights))

#HIV Information
osd_nona<-filter(all,outservd!=998)
table(osd_nona$outservd)
prop.table(wtd.table(osd_nona$outservd,weights=osd_nona$weights))

#Other services
ose_nona<-filter(all,outserve!=998)
table(ose_nona$outserve)
prop.table(wtd.table(ose_nona$outserve,weights=ose_nona$weights))

#HIV TESTING

#Ever tested
prop.table(wtd.table(all$tsever, weights = all$weights))
table(all$tsever)
