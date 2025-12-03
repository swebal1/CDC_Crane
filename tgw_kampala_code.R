#data
setwd("C:/Users/sweta/OneDrive - Emory University/CDC Crane/Fall 2023 - Spring 2024")
tgw_k<-read.csv("CRANE3_IBBS_TGW_KAMPALA_09042023.csv")
pop<-read.csv("Age-adjusting HIV prevalence.csv")
library(dplyr)
library(questionr)
library(spatstat)

#DEMOGRAPHICS
#Ugandan national
prop.table(wtd.table(tgw_k$tutnat, weights = tgw_k$weights))
table(tgw_k$tutnat)

#Has (financial) dependents
prop.table(wtd.table(tgw_k$dmdep, weights = tgw_k$weights))
table(tgw_k$dmdep)

#Currently married
prop.table(wtd.table(tgw_k$dmmr, weights = tgw_k$weights))
table(tgw_k$dmmr)

#Lives with spouse/partner
prop.table(wtd.table(tgw_k$dmpart, weights = tgw_k$weights))
table(tgw_k$dmpart)

#Ever been homeless
prop.table(wtd.table(tgw_k$tuthln, weights = tgw_k$weights))
0.3469185+0.1795721 #values obtained from table
table(tgw_k$tuthln)
171+89 #obtained from table

#Completed secondary school
prop.table(wtd.table(tgw_k$tutedu, weights = tgw_k$weights))
table(tgw_k$tutedu)

#Age 18-24

#percent weighted
x<-data.frame(prop.table(wtd.table(tgw_k$elage, weights = tgw_k$weights)))
x
x$Var1<-as.numeric(x$Var1)
y<-filter(x,Var1>=1,Var1<=7)
sum(y$Freq)

#raw value
df<-data.frame(tgw_k$elage)
filter(df,tgw_k.elage>=18,tgw_k.elage<=24)

#Age 25-44
#percent weighted
z2<-filter(x,Var1>=8)
sum(z2$Freq)

#raw value
df<-data.frame(tgw_k$elage)
filter(df,tgw_k.elage>=25)

#Kampala median age
weighted.median(tgw_k$elage, tgw_k$weights, na.rm = TRUE)

#IQR Kampala age
weighted.quantile(tgw_k$elage,tgw_k$weights, probs=seq(0,1,0.25), na.rm = TRUE)

#HIV PREVALENCE AND UNSUPPRESSED HIV PREVALENCE
#Overall - all who tested HIV-positive in the survey
prop.table(wtd.table(tgw_k$bmhiv, weights = tgw_k$weights))
table(tgw_k$bmhiv)

#Age 18-24 and HIV positive
x.denom<-filter(tgw_k,elage>=18,elage<=24)
prop.table(wtd.table(x.denom$bmhiv,weights=x.denom$weights))
table(x.denom$bmhiv)

#Age 25-34 and HIV positive
y.denom<-filter(tgw_k,elage>=25)
prop.table(wtd.table(y.denom$bmhiv,weights=y.denom$weights))
table(y.denom$bmhiv)

#Overall - All who tested HIV-positive and have a VL of 1000+
hiv.d<-select(tgw_k,bmhiv,weights)
hiv.n<-tgw_k%>%
  filter(bmhiv=="Positive",bmvl2>1000)%>%
  select(bmhiv,bmvl2,weights)
(sum(hiv.n$weights))/(sum(hiv.d$weights))

#18-24, All who tested HIV pos and have a VL of 1000+
hiv.d<-tgw_k%>%
  filter(elage>=18,elage<=24)%>%
  select(bmhiv,weights)
hiv.n<-tgw_k%>%
  filter(bmhiv=="Positive",bmvl2>1000,elage>=18,elage<=24)%>%
  select(bmhiv,bmvl2,weights)
(sum(hiv.n$weights))/(sum(hiv.d$weights))
hiv.n

#25+, all who tested HIV pos and have a VL of 1000+
hiv.d<-tgw_k%>%
  filter(elage>=25)%>%
  select(bmhiv,weights)
hiv.n<-tgw_k%>%
  filter(bmhiv=="Positive",bmvl2>1000,elage>=25)%>%
  select(bmhiv,bmvl2,weights)
(sum(hiv.n$weights))/(sum(hiv.d$weights))
hiv.n$bmhiv

hiv.n$bmhiv

##95-95-95 TARGETS - Conditional

#Among all HIV positive, aware of status
denom<-filter(tgw_k,bmhiv=="Positive")
num<-filter(denom,tstat==1|bmvl2<1000,bmvl2!="NA",bmhiv=="Positive")
num<-select(num,tstat,bmvl2,weights,bmhiv,artnow)
num
(sum(num$weights))/(sum(denom$weights))

#Among status aware, on ART
num2<-filter(num,artnow==1|bmvl2<1000)
num2
(sum(num2$weights))/(sum(num$weights))

#Among treated, virally suppressed
num3<-filter(num2,bmvl2<1000)
num3

##95-95-95 TARGETS - Unconditional

#Among all HIV positive, aware of status
denom<-filter(tgw_k,bmhiv=="Positive")
num<-filter(denom,tstat==1|bmvl2<1000,bmvl2!="NA")
num<-select(num,tstat,bmvl2,weights,bmhiv,artnow)
num
(sum(num$weights))/(sum(denom$weights))

#CD4 COUNT & LOW-LEVEL VIREMIA DISTRIBUTION

#<100
cd4<-filter(tgw_k,bmcd4!="NA",bmhiv!="Inconclusive")
cd4$bmcd4
filter(cd4,bmcd4<100)$bmcd4

#100-249
#percent weighted
cd4.n1<-filter(cd4,bmcd4>=100,bmcd4<=249)
(sum(cd4.n1$weights))/(sum(cd4$weights))
select(cd4.n1,bmcd4)

#250-499
cd4.n2<-filter(cd4,bmcd4>=250,bmcd4<=499)
(sum(cd4.n2$weights))/(sum(cd4$weights))
select(cd4.n2,bmcd4)

#500+
#percent weighted
cd4.n3<-filter(cd4,bmcd4>=500)
(sum(cd4.n3$weights))/(sum(cd4$weights))
select(cd4.n3,bmcd4)

#LLV 
llv<-filter(tgw_k,bmvl2!="NA",bmvl2<=999)
table(llv$bmvl2)
prop.table(wtd.table(llv$bmvl2,weights=llv$weights))

#CD4 count median
cd4<-filter(tgw_k,bmcd4!="NA",bmhiv!="Inconclusive")
weighted.median(cd4$bmcd4, cd4$weights, na.rm = TRUE)
weighted.quantile(cd4$bmcd4, cd4$weights, probs=seq(0,1,0.25), na.rm = TRUE)

#LLV median, recode as 25 to calculate median/IQR
weighted.median(llv$bmvl2, llv$weights)
weighted.quantile(llv$bmvl2, llv$weights, probs=seq(0,1,0.25), na.rm = TRUE)

#ACTIVE SYPHILIS PREVALENCE
#Overall
o<-filter(tgw_k,bmsyp!="",bmsyp!="Past Syphilis")
prop.table(wtd.table(o$bmsyp, weights = o$weights))
table(o$bmsyp)

#Among HIV-pos
ahp<-filter(tgw_k,bmhiv=="Positive",bmsyp!="",bmsyp!="Past Syphilis")
prop.table(wtd.table(ahp$bmsyp, weights = ahp$weights))
table(ahp$bmsyp)

#Among HIV-neg
ahn<-filter(tgw_k,bmhiv=="Negative",bmsyp!="",bmsyp!="Past Syphilis")
prop.table(wtd.table(ahn$bmsyp, weights = ahn$weights))
table(ahn$bmsyp)

#Age 18-24 with active syphilis
ya<-filter(tgw_k,elage>=18,elage<=24,bmsyp!="",bmsyp!="Past Syphilis")
prop.table(wtd.table(ya$bmsyp, weights = ya$weights))
table(ya$bmsyp)

#Age 25-34 with active syphilis
ma<-filter(tgw_k,elage>=25,bmsyp!="",bmsyp!="Past Syphilis")
prop.table(wtd.table(ma$bmsyp, weights = ma$weights))
table(ma$bmsyp)

#tgw_k CHARACTERISTICS

#had gender enhancement or transition
prop.table(wtd.table(tgw_k$tgsrg, weights = tgw_k$weights))
table(tgw_k$tgsrg)

#surgically constructed vagina
prop.table(wtd.table(tgw_k$tgvagina, weights = tgw_k$weights))
table(tgw_k$tgvagina)

#hormone therapy
prop.table(wtd.table(tgw_k$tghmuse, weights = tgw_k$weights))
0.07558996+0.12082657 #from table
table(tgw_k$tghmuse)
37+48 #from table

#used birth control as hormone therapy
prop.table(wtd.table(tgw_k$tghmbc, weights = tgw_k$weights))
0.03149766 +0.06526783 #from table
table(tgw_k$tghmbc)
19+28 #from table

#TYPES OF HORMONES USED
hormone<-filter(tgw_k,tghmuse!=3)
#Injection
prop.table(wtd.table(hormone$tghmtyxa, weights = hormone$weights))
table(hormone$tghmtyxa)

#Gel
prop.table(wtd.table(hormone$tghmtyxb, weights = hormone$weights))
table(hormone$tghmtyxb)

#Pills
prop.table(wtd.table(hormone$tghmtyxc, weights = hormone$weights))
table(hormone$tghmtyxc)

#Patch
prop.table(wtd.table(tgw_k$tghmtyxd, weights = tgw_k$weights))
table(tgw_k$tghmtyxd)

#Other
prop.table(wtd.table(hormone$tghmtyxe, weights = hormone$weights))
table(hormone$tghmtyxe)

#TYPES OF GENDER TRANSITION PROCEDURE
table(tgw_k$tgsrgty)
prop.table(wtd.table(tgw_k$tgsrgty, weights = tgw_k$weights))

#ALCOHOL, DRUG USE, DEPRESSION
#Alcohol misuse
prop.table(wtd.table(tgw_k$alcscr2, weights = tgw_k$weights))
table(tgw_k$alcscr2)

#Drug use ever
prop.table(wtd.table(tgw_k$drev, weights = tgw_k$weights))
0.24925424+0.08512117 #from table
table(tgw_k$drev)
122+39 #from table

#Drug last 6 months
prop.table(wtd.table(tgw_k$drev, weights = tgw_k$weights))
table(tgw_k$drev)

#Injected drugs ever
prop.table(wtd.table(tgw_k$driev, weights = tgw_k$weights))
0.12564009 +0.04287159  #from table
table(tgw_k$driev)
25+7 #from table

#Injected drugs last 6 months
prop.table(wtd.table(tgw_k$driev, weights = tgw_k$weights))
table(tgw_k$driev)

#PHQ DEPRESSION SCREENING

#Depression level,
table(all$phqscre)
prop.table(wtd.table(all$phqscre, weights = all$weights))

#Trouble Sleeping, PHQ3
table(tgw_k$phq3)
prop.table(wtd.table(tgw_k$phq3, weights = tgw_k$weights))

#Feeling tired, PHQ4
table(tgw_k$phq4)
prop.table(wtd.table(tgw_k$phq4, weights = tgw_k$weights))

#Poor/excessive appetite, PHQ5
table(tgw_k$phq5)
prop.table(wtd.table(tgw_k$phq5, weights = tgw_k$weights))

#Feeling bad about oneself, PHQ6
table(tgw_k$phq6)
prop.table(wtd.table(tgw_k$phq6, weights = tgw_k$weights))

#Trouble concentrating, PHQ7
table(tgw_k$phq7)
prop.table(wtd.table(tgw_k$phq7, weights = tgw_k$weights))

#Moving/speaking slowly or excessively, PHQ8
table(tgw_k$phq8)
prop.table(wtd.table(tgw_k$phq8, weights = tgw_k$weights))

#Suicidal ideation, PHQ9
table(tgw_k$phq9)
prop.table(wtd.table(tgw_k$phq9, weights = tgw_k$weights))

#MENTALLY DETRIMENTAL EXPERIENCE OR DIAGNOSIS

#depression
table(tgw_k$mhdxxa)
prop.table(wtd.table(tgw_k$mhdxxa, weights = tgw_k$weights))

#anxiety
table(tgw_k$mhdxxb)
prop.table(wtd.table(tgw_k$mhdxxb, weights = tgw_k$weights))

#stigma
table(tgw_k$mhdxxc)
prop.table(wtd.table(tgw_k$mhdxxc, weights = tgw_k$weights))

#feeling of rejection
table(tgw_k$mhdxxd)
prop.table(wtd.table(tgw_k$mhdxxd, weights = tgw_k$weights))

#loss of self esteem
table(tgw_k$mhdxxe)
prop.table(wtd.table(tgw_k$mhdxxe, weights = tgw_k$weights))

#ENACTED STIGMA AGAINST tgw_k

#Healthcare discrimination
hcw<-filter(tgw_k,tgsthc1==1|tgsthc2==1|tgsthc3==1)
hcw.n<-select(hcw,tgsthc1,tgsthc2,tgsthc3,weights)
hcw.n
hcw.d<-filter(tgw_k,tgsthc1!="NA"|tgsthc2!="NA",tgsthc3!="NA")
hcw.d<-select(hcw.d,tgsthc1,tgsthc2,tgsthc3,weights)
hcw.d
(sum(hcw.n$weights))/(sum(hcw.d$weights))

#Discrimination by family or friends
fam.n<-filter(tgw_k,tgstfam1==1|tgstfam2==1|tgstfam3==1)
fam.n<-select(fam.n,tgstfam1,tgstfam2,tgstfam3,weights)
fam.n
fam.d<-filter(tgw_k,tgstfam1!="NA"|tgstfam2!="NA",tgstfam3!="NA")
fam.d<-select(fam.d,tgstfam1,tgstfam2,tgstfam3,weights)
fam.d
(sum(fam.n$weights))/(sum(fam.d$weights))

#Discrimination by police
pol.n<-filter(tgw_k,tgstuni1==1|tgstuni2==1|tgstuni3==1)
pol.n<-select(pol.n,tgstuni1,tgstuni2,tgstuni3,weights)
pol.n
pol.d<-filter(tgw_k,tgstuni1!="NA"|tgstuni2!="NA",tgstuni3!="NA")
pol.d<-select(pol.d,tgstuni1,tgstuni2,tgstuni3,weights)
pol.d
(sum(pol.n$weights))/(sum(pol.d$weights))

# ANTICIPATED tgw_k STIGMA

#Afraid seeking healthcare, tgst1 =1
prop.table(wtd.table(tgw_k$tgst1, weights = tgw_k$weights))
table(tgw_k$tgst1)

#Not seeking healthcare, tgst2 =1
prop.table(wtd.table(tgw_k$tgst2, weights = tgw_k$weights))
table(tgw_k$tgst2)

#Avoid carrying condoms, tgst3 =1
prop.table(wtd.table(tgw_k$tgst3, weights = tgw_k$weights))
table(tgw_k$tgst3)

#Avoid public places, tgst4 =1
prop.table(wtd.table(tgw_k$tgst4, weights = tgw_k$weights))
table(tgw_k$tgst4)

#Been blackmailed, tgst =5
prop.table(wtd.table(tgw_k$tgst5, weights = tgw_k$weights))
table(tgw_k$tgst5)

#INTERNALIZED TG STIGMA

#Cannot face family and friends, tgstin1 = 1 and 2 (strongly agree and agree)
prop.table(wtd.table(tgw_k$tgstin1, weights = tgw_k$weights))
table(tgw_k$tgstin1)

#Feel ashamed of being a tgw_k, tgstin2 = 1 and 2 (strongly agree and agree)
prop.table(wtd.table(tgw_k$tgstin2, weights = tgw_k$weights))
table(tgw_k$tgstin2)

#Feel guilty for identifying as tgw_k, tgstin3 = 1 and 2
prop.table(wtd.table(tgw_k$tgstin3, weights = tgw_k$weights))
table(tgw_k$tgstin3)

#Blame myself for identifying as tgw_k, tgstin4 = 1 and 2
prop.table(wtd.table(tgw_k$tgstin4, weights = tgw_k$weights))
table(tgw_k$tgstin4)

#No self-worth, tgstin5 = 1 and 2
prop.table(wtd.table(tgw_k$tgstin5, weights = tgw_k$weights))
table(tgw_k$tgstin5)

#Suicidal ideation, tgstin6 = 1 and 2
prop.table(wtd.table(tgw_k$tgstin6, weights = tgw_k$weights))
table(tgw_k$tgstin6)

#Deserve bad things that happen because tgw_k, tgstin7 = 1 and 2
prop.table(wtd.table(tgw_k$tgstin7, weights = tgw_k$weights))
table(tgw_k$tgstin7)

#MORTALITY AMONG PEERS
mort<-tgw_k%>%
  filter(rcdgcod1!=9)%>%
  select(rcdgcod1,weights)
prop.table(wtd.table(mort$rcdgcod1,weights=mort$weights))
table(mort$rcdgcod1)

#OUTREACH SERVICES
#last 6 mos, >6 mos
prop.table(wtd.table(tgw_k$outexp, weights = tgw_k$weights))
table(tgw_k$outexp)

#Condoms
table(tgw_k$outserva)
prop.table(wtd.table(tgw_k$outserva,weights=tgw_k$weights))

#Lubricants
table(tgw_k$outservb)
prop.table(wtd.table(tgw_k$outservb,weights=tgw_k$weights))

#HIV Testing
table(tgw_k$outservc)
prop.table(wtd.table(tgw_k$outservc,weights=tgw_k$weights))

#HIV Information
table(tgw_k$outservd)
prop.table(wtd.table(tgw_k$outservd,weights=tgw_k$weights))

#Other services
table(tgw_k$outserve)
prop.table(wtd.table(tgw_k$outserve,weights=tgw_k$weights))

#HIV TESTING

#Ever tested
prop.table(wtd.table(tgw_k$tsever, weights = tgw_k$weights))
table(tgw_k$tsever)

#<=12 mos, 1-2 years, >2 years
#percent weighted
data.frame(prop.table(wtd.table(tgw_k$tswhen, weights = tgw_k$weights)))
test<-data.frame(prop.table(wtd.table(tgw_k$tswhen, weights = tgw_k$weights)))
test$Var1<-as.numeric(test$Var1)

#raw value
data.frame(table(tgw_k$tswhen))
test2<-data.frame(table(tgw_k$tswhen))
test2$Var1<-as.numeric(test2$Var1)

#<=12 mos 
#percent weighted
mos.12<-filter(test,Var1>=1,Var1<=7)
sum(mos.12$Freq)

#raw value
mos.12.2<-filter(test2,Var1>=1,Var1<=7)
sum(mos.12.2$Freq)

#1-2 years
#percent weighted
data.frame(prop.table(wtd.table(tgw_k$tswhen, weights = tgw_k$weights)))
#only 1 value for 24 mos

#raw value
data.frame(table(tgw_k$tswhen))
#only 1 value for 24 mos

#>2 years 
#percent weighted
years<-filter(test,Var1>=9)
sum(years$Freq)

#raw value
years<-filter(test2,Var1>=9)
sum(years$Freq)

#REASONS FOR NOT TESTING FOR HIV

#Not tested in the last 12 months
table(tgw_k$ts1yrno)
prop.table(wtd.table(tgw_k$ts1yrno,weights=tgw_k$weights))

#Never tested
prop.table(wtd.table(tgw_k$tsevno,weights=tgw_k$weights))
table(tgw_k$tsevno)

#PREP

#Heard of PrEP
pr<-filter(tgw_k,bmhiv=="Negative",prhear!="NA")
table(pr$prhear)
prop.table(wtd.table(pr$prhear,weights=pr$weights))

#Was offered PrEP
off<-filter(tgw_k,prhear==1)
prop.table(wtd.table(off$proffer,weights=off$weights))
table(off$proffer)

#Know where to get prep
prop.table(wtd.table(off$prget,weights=off$weights))
table(off$prget)

#Want to use PrEP
pr<-filter(tgw_k,bmhiv=="Negative",prhear!="NA")
prop.table(wtd.table(pr$prwant,weights=pr$weights))
table(pr$prwant)

#Taken PrEP
pr2<-filter(tgw_k,prhear==1)
table(pr2$prtake)
prop.table(wtd.table(pr2$prtake,weights=pr2$weights))


#REASONS FOR NOT TAKING PREP IN THE LAST 6 MONTHS
p<-filter(tgw_k,prhear==1,prtake >2,bmhiv=="Negative")
table(p$prnowhy)
prop.table(wtd.table(p$prnowhy,weights=p$weights))

#MISSED ARV DOSE

#Missed in last 3 months
table(tgw_k$arvmss)
prop.table(wtd.table(tgw_k$arvmss,weights=tgw_k$weights))

#Reason for missing taking ARVs
table(tgw_k$arvmssw)
prop.table(wtd.table(tgw_k$arvmssw,weights=tgw_k$weights))


#MISSED ARV REFILL

#Missed in the last 12 months
table(tgw_k$artmiss)
prop.table(wtd.table(tgw_k$artmiss,weights=tgw_k$weights))

#Reason for missing ARV refil
arv<-filter(tgw_k,artmiss!=2)
table(arv$artmssw1)
prop.table(wtd.table(arv$artmssw1,weights=arv$weights))

#SELECT ART CHARACTERISTICS

#No. of ART Providers since starting ART
art<-filter(tgw_k,artevr==1|artnow==1)
table(art$artplc)
prop.table(wtd.table(art$artplc,weights=art$weights))

#Current ART Provider
table(art$artprov)
prop.table(wtd.table(art$artprov,weights=art$weights))


#TUBERCULOSIS AMONG HIV POSITIVE tgw_k 

#Screened for TB symptoms, HIV pos
tb.screened<-filter(tgw_k,bmhiv=="Positive")
table(tb.screened$tbscrnp)
prop.table(wtd.table(tb.screened$tbscrnp, weights=tb.screened$weights))

#Screened for TB symptoms, HIV neg
tb.screened<-filter(tgw_k,bmhiv=="Negative")
table(tb.screened$tbscrnn)
prop.table(wtd.table(tb.screened$tbscrnn, weights=tb.screened$weights))

#Informed of TB diagnosis
tb.informed<-filter(tgw_k,bmhiv=="Positive")
table(tb.informed$tbdx)
prop.table(wtd.table(tb.informed$tbdx, weights=tb.informed$weights))

#Population size estimate
kam<-filter(pop,area_name=="Kampala",sex=="female")
select(kam,age_group,population)
