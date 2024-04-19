## Technical validation of GCoM datasets

library(readxl)
library(dplyr)
library(readr)
library(data.table)
library(cluster)
library(NbClust)
library(clusterCrit)
library(solitude)
library(dbscan)
library(tidyr)

## set the path
setwd(".../GCoM_validation_emissions_policies/")

## Part I. Energy
## Signatories
orgsig = read_excel("df1.xlsx", col_types = c(rep("text",13),rep("numeric",3),rep("text",2),rep("numeric",2),rep("text",21),"numeric",rep("text",3))) 
orgsig$organisation_id = as.factor(orgsig$organisation_id)

## B/MEIs
emisa = read_excel("dfEI_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",3),rep("numeric",2),rep("text",5),"numeric"))
emisr = read_excel("dfEI_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",3),rep("numeric",2),rep("text",5),"numeric"))
emis = rbind(emisa,emisr)
emis$organisation_id = as.factor(emis$organisation_id)

## countries from MyCovenant
covcountry = read.csv("country_.csv", header=TRUE, encoding = "UTF-8")
covcountry2 = covcountry[c("code","area_id","english_name")]
colnames(covcountry2) = c("country_code","area_id","country")

## Greater table
## Signatories with countries
orgsig_con = left_join(orgsig, covcountry2, by = c("country_code"))
MC = left_join(emis, orgsig_con, by = c("organisation_id","organisation_name"))

## join with the three-character country code
code2 = read_excel("country_codes.xlsx")
code2$codeL2 = tolower(code2$code2)
code2$codeL3 = tolower(code2$code3)
code3 = code2[c("codeL2","codeL3")]
colnames(code3) = c("country_code","IEA_code")
MC3 = left_join(MC, code3, by = c("country_code"))

# Export plans (everything) for validation
MC3 = MC3 %>% rename(population=population_adhesion)
MC_v = MC3

## South-East IEA countries
## population by year and country
pop_se = read.csv("WPP2019_TotalPopulation.csv", header=TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)
pop_se = pop_se[,c("Location","Time","PopTotal")]
colnames(pop_se) = c("country","year","pops")
pop_se = subset(pop_se, (pop_se$year<2022 & pop_se$year>1989))
pop_se$country = ifelse(pop_se$country=="State of Palestine","Memo: Palestinian Authority",pop_se$country)

## consumption
se00 = read_excel("IEA-com south-east_1990-2018.xlsx")[,1:11]
## Other countries: Switzerland, Mexico, Tajikistan, Kazakhstan, Palestine
others = read.csv("ConsumptionOtherCountries.csv", header=TRUE, encoding = "UTF-8", stringsAsFactors = FALSE)[,1:11]
colnames(others)=c("MEASURE...1","MEASURE...2","COUNTRY...3","COUNTRY...4","PRODUCT...5","PRODUCT...6","FLOW...7","FLOW...8","TIME","Time","Value")
se0 = rbind(se00,others)

ktoe = subset(se0, (se0$MEASURE...1=="KTOE" & se0$Value!="NA"))
ktoe= ktoe[,c("COUNTRY...3","COUNTRY...4","PRODUCT...5","PRODUCT...6","FLOW...7","Time","Value")]
colnames(ktoe) = c("IEA_code","country","carrierM","carrier","flow","year","KTOE")
ktoe$mwh = ktoe$KTOE * 11.63
ktoe$IEA_code = tolower(ktoe$IEA_code)
se = subset(ktoe, ktoe$flow=="TFC")
## consumption per capita
se_pop = left_join(se, pop_se, by= c("country","year"))
se_pop$elecap = se_pop$mwh/se_pop$pops

## data set with all carriers
SE = se_pop[,c("IEA_code","country","carrierM","carrier",
               "flow","year","elecap")]

# Electricity
SE_elec = subset(SE, SE$carrier=="Electricity")
SE_elec$IEA_code = ifelse(SE_elec$IEA_code=="mpalestine","pse",SE_elec$IEA_code)

# Thermal
SE_thermal0 = subset(SE, SE$carrier!="Electricity")
SE_thermal0$IEA_code = ifelse(SE_thermal0$IEA_code=="mpalestine","pse",SE_thermal0$IEA_code)

## Load the national consumption for EU-ESTAT: Electricity per capita excl. Industry 
elec = read.csv("electricity.csv", skip = 1, header=TRUE)
elec$country_code = tolower(elec$X)
elec$country_code = ifelse(elec$country_code=="uk","gb",elec$country_code )
elec$country_code = ifelse(elec$country_code=="el","gr",elec$country_code )
elec$country = trimws(elec$X.1, which = c("both"))
elec2 = subset(elec, elec$X2019!="NA")

## Validate MyCovenant with averages of the B/MEI's country
MC_e0 = subset(MC_v, MC_v$energy_consumption_carrier_id==1)
MC_e = subset(MC_e0,!MC_e0$item_text %in% "Industry") # Electricity excluding Industry

## Aggregate MyCovenant's B/MEIs by carrier through all sectors
elecdata = setDT(MC_e)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,action_plan_id,
                                                            emission_inventory_id), 
                       .SDcols=c("energy_measure")]
## Join the B/MEIs' electricity consumption with other Covenant variables
pop_inv = unique(MC_e[,c("organisation_id","organisation_name","action_plan_id","bei_flag","emission_inventory_id",
                         "inventory_year","population_in_the_inventory_year",
                         "area","country_code","population","country",
                         "IEA_code")])
elecdata2 = left_join(pop_inv, elecdata, by = c("organisation_id","action_plan_id","emission_inventory_id"))
## Join the B/MEIs with EU-STAT and SE-IEA national consumption/capita
elecs0 = left_join(elecdata2, elec2, by = c("country_code"))
## new variable year in elecs0 to merge by year in the IEA-SE
elecs0$year = ifelse(elecs0$inventory_year >= 2019, 2018, elecs0$inventory_year)
elecs = left_join(elecs0, SE_elec, by = c("IEA_code","year")) 

## Compute electricity per capita for B/MEIs
## Get the maximum between population declared in the inventory or the adhesion year
elecs$pope = apply(elecs[, c("population_in_the_inventory_year","population")], 1, max, na.rm=TRUE)

## EU-ESTAT national consumption
## Control de year
yearESTAT = 2019
jj = which( colnames(elecs)==paste("X",yearESTAT,sep=""))

n = dim(elecs)[1]
for (i in 1:n){
  years = yearESTAT-elecs$inventory_year[i]
  k = jj-years
  if(years>0 & years<(yearESTAT-1990)){
    elecs$nat_elec[i]=elecs[i,..k]
  }else{elecs$nat_elec[i]=elecs[i,..jj]} 
  if(is.na(elecs$nat_elec[i])){elecs$nat_elec[i]=elecs$elecap[i]}
}
elecs$nat_elec = as.numeric(elecs$nat_elec)
## compute B/MEIs' declared consumption per capita
elecs$dec_elec = elecs$energy_measure/elecs$pope
xx = subset(elecs,is.na(dec_elec))

## Add the extra variable saying if BEI=FALSE, is there a follow-up MEI?
elecs = elecs[with(elecs, order(elecs$organisation_id,
                                elecs$inventory_year)),]

elecs$FU = 0
for(i in 1:n){
  org_aux = elecs$organisation_id[i]
  if(elecs$bei_flag[i]==FALSE){
    if(elecs$bei_flag[i+1]==FALSE & elecs$organisation_id[i+1]==org_aux){
      elecs$FU[i] = 1
    }
  }
}

## Detect anomalies by carrier for the available information
elecs_study = subset(elecs, !is.na(elecs$nat_elec))
elecs_noval = subset(elecs, is.na(elecs$nat_elec))

## Group by countries with similar averages MWh/capita
## Search for a good partition
mat=matrix(nrow=0,ncol=5)
ee=as.matrix(na.omit(elecs_study$nat_elec))
for(i in 2:5){
  k_clus = pam(ee, i)
  a=intCriteria(as.matrix(ee), k_clus$cluster, "Calinski_Harabasz") #max
  b=intCriteria(as.matrix(ee), k_clus$cluster, "C_index")#min
  c=intCriteria(as.matrix(ee), k_clus$cluster, "Davies_Bouldin")#min1
  d=intCriteria(as.matrix(ee), k_clus$cluster, "Dunn")#max2
  xx = cbind(toString(i),toString(a),toString(b),toString(c),toString(d))
  mat = rbind(mat,xx)
}

## three clusters are a reasonable partition
clus2 = pam(ee, 3)
elecss = elecs_study[,c("organisation_id","organisation_name","action_plan_id","bei_flag","FU","emission_inventory_id",
                        "inventory_year","population_in_the_inventory_year",
                        "area","country_code","population","country.x","pope","nat_elec","dec_elec")]
oclus = data.frame(Cluster = clus2$cluster, elecss)
maxi = aggregate(oclus$nat_elec, list(oclus$Cluster), max)
colnames(maxi) = c("Cluster","max")
mini = aggregate(oclus$nat_elec, list(oclus$Cluster), min)
colnames(mini) = c("Cluster","min")
elecs_clus0 = left_join(oclus,maxi,by="Cluster")
elecs_clus = left_join(elecs_clus0,mini,by="Cluster")

elecs_clus = elecs_clus[with(elecs_clus, order(elecs_clus$organisation_id,
                                               elecs_clus$inventory_year)),]

## statistical outliers
apply(elecs_clus, 2, function(x) any(is.na(x)))
nax = subset(elecs_clus,is.na(dec_elec))
ec2 = na.omit(elecs_clus)
ec3 = ec2[duplicated(ec2[,c("action_plan_id","inventory_year","dec_elec")])|duplicated(ec2[,c("action_plan_id","inventory_year","dec_elec")], fromLast=TRUE),]
ec4 = ec2[with(ec2, order(action_plan_id,inventory_year)), ]
ec5 = setDT(ec4)[, lapply(.SD, first), by=.(action_plan_id,inventory_year,country.x,area,population_in_the_inventory_year,dec_elec)]

## Isoforest 
## initiate an isolation forest
set.seed(1)
index = sample(ceiling(nrow(ec5)))
t1 = isolationForest$new(sample_size = length(index))
t1$fit(dataset = ec5[index, c("dec_elec","population_in_the_inventory_year")])
outs = t1$predict(ec5[, c("dec_elec","population_in_the_inventory_year")])
ec5$stat_outlier_iso = outs$anomaly_score
outs_iso = ec5[with(ec5, order(-stat_outlier_iso)), ]

## LOF
lof_o = lof(as.matrix(ec5[, c("dec_elec","population_in_the_inventory_year")]), minPts = 3)
ec5$stat_outlier_lof = lof_o
outs_lof = ec5[with(ec5, order(-stat_outlier_lof)), ]

## statistical outliers 
write.table(outs_lof, file = "stat_electricOutliers.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


## Electricity anomalies 
elecs_clus$thresholdR = (elecs_clus$max)
elecs_clus$thresholdL = 0.01
elecs_clus$elec_outlier1 = as.factor(ifelse(elecs_clus$dec_elec > elecs_clus$thresholdR , "outlier", "normal"))
elecs_clus$elec_outlier2 = as.factor(ifelse(elecs_clus$dec_elec > 5*elecs_clus$thresholdR , "outlier", "normal"))
elecs_clus$elec_outlier0 = as.factor(ifelse(elecs_clus$dec_elec < elecs_clus$thresholdL, "outlier", "normal"))
outlier_electricity1 = subset(elecs_clus,elecs_clus$elec_outlier1=="outlier") 
outlier_electricity2 = subset(elecs_clus,elecs_clus$elec_outlier2=="outlier")
outlier_electricity0 = subset(elecs_clus,elecs_clus$elec_outlier0=="outlier") 

## save result
## all the data
elecs_clus = with(elecs_clus, elecs_clus[order(elecs_clus$elec_outlier2,elecs_clus$elec_outlier0, decreasing = c(TRUE), method = "radix"),])
write.table(elecs_clus, file = "electricFuneral.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## only outliers by inflated declared electricity
write.table(outlier_electricity2, file = "electricOutliers.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## only outliers by super-low declared electricity
write.table(outlier_electricity0, file = "electricOutliers0.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## missing countries for Electricity validation
elecs_novel = elecs_noval[,c("organisation_id","organisation_name","action_plan_id","emission_inventory_id",
                             "bei_flag","FU","inventory_year","population_in_the_inventory_year",
                             "area","country_code","population","country.x",
                             "IEA_code","energy_measure","dec_elec")]
write.table(elecs_novel, file = "BMEIs_missing_electricityValidation.txt", sep = "\t",row.names = FALSE, col.names = TRUE)




## Aggregate all Thermal carriers from IEA
SE_thermal = setDT(SE_thermal0)[, lapply(.SD, sum, na.rm=TRUE), by=.(IEA_code,country,year), 
                                .SDcols=c("elecap")]

## Load the national consumption for EU-ESTAT: Thermal per capita excl. Industry 
heat = read.csv("heat.csv", skip = 1, header=TRUE)[,1:32]
natgas = read.csv("natgas.csv", skip = 1, header=TRUE)[,1:32]
diesel = read.csv("diesel.csv", skip = 1, header=TRUE)[,1:32]
gasoline = read.csv("gasoline.csv", skip = 1, header=TRUE)[,1:32]
gpl = read.csv("gpl.csv", skip = 1, header=TRUE)[,1:32]
coal = read.csv("coal.csv", skip = 1, header=TRUE)[,1:32]
lignite = read.csv("lignite.csv", skip = 1, header=TRUE)[,1:32]
hoil = read.csv("hoildiesel.csv", skip = 1, header=TRUE)[,1:32]
obio = read.csv("otherbiomass.csv", skip = 1, header=TRUE)[,1:32]
solar = read.csv("solarthermal.csv", skip = 1, header=TRUE)[,1:32]
geo = read.csv("geothermal.csv", skip = 1, header=TRUE)[,1:32]
nelec = rbind(heat,natgas,diesel,gasoline,gpl,coal,lignite,hoil,obio,solar,geo)
nelec2 = subset(nelec, nelec$X2019!="NA")

nelec2$country_code = tolower(nelec2$X)
nelec2$country_code = ifelse(nelec2$country_code=="uk","gb",nelec2$country_code )
nelec2$country_code = ifelse(nelec2$country_code=="el","gr",nelec2$country_code )
nelec2$country = trimws(nelec2$X.1, which = c("both"))#, "left", "right"))#, whitespace = "[ \t\r\n]")

## Aggregate carriers from ESTAT
nelecT = setDT(nelec2)[, lapply(.SD, sum, na.rm=TRUE), by=.(country,country_code), 
                       .SDcols=c("X1990","X1991","X1992","X1993","X1994","X1995","X1996","X1997","X1998","X1999",
                                 "X2000","X2001","X2002","X2003","X2004","X2005","X2006","X2007","X2008","X2009",
                                 "X2010","X2011","X2012","X2013","X2014","X2015","X2016","X2017","X2018","X2019")]

## Validate MyCovenant with averages of the B/MEI's country
MC_e0 = subset(MC_v, MC_v$energy_consumption_carrier_id!=1)
MC_e = subset(MC_e0,!MC_e0$item_text %in% "Industry") # Excluding Industry

## Aggregate MyCovenant's B/MEIs by carrier through all sectors
nelecdata = setDT(MC_e)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,action_plan_id,emission_inventory_id), 
                        .SDcols=c("energy_measure")]
## Join the B/MEIs' electricity consumption with other Covenant variables
pop_inv = unique(MC_e[,c("organisation_id","organisation_name","action_plan_id","bei_flag","emission_inventory_id",
                         "inventory_year","population_in_the_inventory_year",
                         "area","country_code","population","country","IEA_code")])
nelecdata2 = left_join(pop_inv, nelecdata, by = c("organisation_id","action_plan_id","emission_inventory_id"))
## Join the B/MEIs with EU-STAT and SE-IEA national consumption/capita
nelecs0 = left_join(nelecdata2, nelecT, by = c("country_code"))
## new variable year in elecs0 to merge by year in the IEA-SE
nelecs0$year = ifelse(nelecs0$inventory_year >= 2019, 2018, nelecs0$inventory_year)
nelecs = left_join(nelecs0, SE_thermal, by = c("IEA_code","year")) 

## Compute electricity per capita for B/MEIs
## Get the maximum between population declared in the inventory or the adhesion year
nelecs$pope = apply(nelecs[, c("population_in_the_inventory_year","population")], 1, max)
## get EU-ESTAT national consumption

## Control de year
yearESTAT = 2019
jj = which( colnames(nelecs)==paste("X",yearESTAT,sep=""))

n = dim(nelecs)[1]
for (i in 1:n){
  years = yearESTAT-nelecs$inventory_year[i]
  k = jj-years
  if(years>0 & years<(yearESTAT-1990)){
    nelecs$nat_elec[i]=nelecs[i,..k]
  }else{nelecs$nat_elec[i]=nelecs[i,..jj]} 
  if(is.na(nelecs$nat_elec[i])){nelecs$nat_elec[i]=nelecs$elecap[i]}
}
nelecs$nat_elec = as.numeric(nelecs$nat_elec)
## compute B/MEIs' declared consumption per capita
nelecs$dec_elec = nelecs$energy_measure/nelecs$pope

## Add the extra variable saying if BEI=FALSE, is there a follow-up MEI?
nelecs = nelecs[with(nelecs, order(nelecs$organisation_id,
                                   nelecs$inventory_year)),]

nelecs$FU = 0
for(i in 1:n){
  org_aux = nelecs$organisation_id[i]
  if(nelecs$bei[i]==FALSE){
    if(nelecs$bei[i+1]==FALSE & nelecs$organisation_id[i+1]==org_aux){
      nelecs$FU[i] = 1
    }
  }
}

## Detect anomalies by carrier for the available information
nelecs_study = subset(nelecs, !is.na(nelecs$nat_elec))
nelecs_noval = subset(nelecs, is.na(nelecs$nat_elec))


## Group by countries with similar averages MWh/capita
## Search for a good partition
mat=matrix(nrow=0,ncol=5)
ee=as.matrix(na.omit(nelecs_study$nat_elec))
for(i in 2:5){
  k_clus = pam(ee, i)
  a=intCriteria(as.matrix(ee), k_clus$cluster, "Calinski_Harabasz")#max
  b=intCriteria(as.matrix(ee), k_clus$cluster, "C_index")#min
  c=intCriteria(as.matrix(ee), k_clus$cluster, "Davies_Bouldin")#min
  d=intCriteria(as.matrix(ee), k_clus$cluster, "Dunn")#max
  xx = cbind(toString(i),toString(a),toString(b),toString(c),toString(d))
  mat = rbind(mat,xx)
}

## By consensus, three clusters seem to be a reasonable partition
clus2 = pam(ee, 3)
nelecss = nelecs_study[,c("organisation_id","organisation_name","action_plan_id","bei_flag","FU","emission_inventory_id",
                          "inventory_year","population_in_the_inventory_year",
                          "area","country_code","population","country.x","pope","nat_elec","dec_elec")]
oclus = data.frame(Cluster = clus2$cluster, nelecss)
maxi = aggregate(oclus$nat_elec, list(oclus$Cluster), max)
colnames(maxi) = c("Cluster","max")
mini = aggregate(oclus$nat_elec, list(oclus$Cluster), min)
colnames(mini) = c("Cluster","min")
nelecs_clus0 = left_join(oclus,maxi,by="Cluster")
nelecs_clus = left_join(nelecs_clus0,mini,by="Cluster")

nelecs_clus = nelecs_clus[with(nelecs_clus, order(nelecs_clus$organisation_id,
                                                  nelecs_clus$inventory_year)),]


## statistical outliers
apply(nelecs_clus, 2, function(x) any(is.na(x)))
ec2 = na.omit(elecs_clus)
ec3 = ec2[duplicated(ec2[,c("action_plan_id","inventory_year","dec_elec")])|duplicated(ec2[,c("action_plan_id","inventory_year","dec_elec")], fromLast=TRUE),]
ec4 = ec2[with(ec2, order(action_plan_id,inventory_year)), ]
ec5 = setDT(ec4)[, lapply(.SD, first), by=.(action_plan_id,inventory_year,country.x,area,population_in_the_inventory_year,dec_elec)]

# ### Isoforest
## initiate an isolation forest
set.seed(1)
index = sample(ceiling(nrow(ec5)))# * 0.5))
t1 = isolationForest$new(sample_size = length(index))
t1$fit(dataset = ec5[index, c("dec_elec","population_in_the_inventory_year")])
outs = t1$predict(ec5[, c("dec_elec","population_in_the_inventory_year")])
ec5$stat_outlier_iso_th = outs$anomaly_score
outs_iso = ec5[with(ec5, order(-stat_outlier_iso_th)), ]

## LOF
lof_o = lof(as.matrix(ec5[, c("dec_elec","population_in_the_inventory_year")]), minPts = 3)
ec5$stat_outlier_lof_th = lof_o
outs_lof = ec5[with(ec5, order(-stat_outlier_lof_th)), ]

## statistical outliers 
write.table(outs_lof, file = "stat_thermalOutliers.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Thermal anomalies 
nelecs_clus$thresholdR = (2*nelecs_clus$max)
nelecs_clus$thresholdL = 0.01
nelecs_clus$thermal_outlier1 = as.factor(ifelse(nelecs_clus$dec_elec > nelecs_clus$thresholdR, "outlier", "normal"))
nelecs_clus$thermal_outlier2 = as.factor(ifelse(nelecs_clus$dec_elec > 3*nelecs_clus$thresholdR, "outlier", "normal"))
nelecs_clus$thermal_outlier0 = as.factor(ifelse(nelecs_clus$dec_elec <= nelecs_clus$thresholdL, "outlier", "normal"))
outlier_thermal1 = subset(nelecs_clus,nelecs_clus$thermal_outlier1=="outlier") 
outlier_thermal2 = subset(nelecs_clus,nelecs_clus$thermal_outlier2=="outlier")
outlier_thermal0 = subset(nelecs_clus,nelecs_clus$thermal_outlier0=="outlier") 

## save result
## all the data
nelecs_clus = with(nelecs_clus, nelecs_clus[order(nelecs_clus$thermal_outlier2,
                                                  nelecs_clus$thermal_outlier0, decreasing = c(TRUE), method = "radix"),])
write.table(nelecs_clus, file = "thermalFuneral.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## only outliers to the right
write.table(outlier_thermal1, file = "thermalOutliers.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## only outliers to the left
write.table(outlier_thermal0, file = "thermalOutliers0.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## missing countries for Thermal validation
nelecs_novel = nelecs_noval[,c("organisation_id","organisation_name","action_plan_id","emission_inventory_id",
                               "bei_flag","FU","inventory_year","population_in_the_inventory_year",
                               "area","country_code","population","country.x",
                               "IEA_code","energy_measure","dec_elec")]
write.table(nelecs_novel, file = "BMEIs_missing_thermalValidation.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Part II. Emissions
## EFs
facts0a = read_excel("dfEF_AP.xlsx",col_types = c(rep("text",8),rep("numeric",1),rep("text",2),rep("numeric",1),rep("text",3)))
facts0r = read_excel("dfEF_MR.xlsx",col_types = c(rep("text",8),rep("numeric",1),rep("text",2),rep("numeric",1),rep("text",3)))
facts0=rbind(facts0a,facts0r)
facts0$organisation_id = as.factor(facts0$organisation_id)
facts = subset(facts0, !is.na(facts0$emission_inventory_year))

## Greater table
MC = left_join(facts, orgsig_con, by = c("organisation_id","organisation_name"))
MC3 = left_join(MC, code3, by = c("country_code"))

# Export data for validation
MC3 = MC3 %>% rename(population=population_adhesion)
MC_f = MC3[,c("organisation_id","organisation_name","action_plan_id","emission_inventory_id",
              "emission_inventory_year","emission_reporting_unit","emission_factor_type_",
              "emission_factor_value","energy_consumption_carrier_id","energy_consumption_carrier",
              "emission_factor_origin","area","country_code","population",
              "country","IEA_code")]
MC_fb = subset(MC_f,organisation_name!="Albolote")
MC_fa = subset(MC_f,organisation_name=="Albolote")
MC_fa$emission_factor_type_="IPCC"
MC_fa$emission_reporting_unit ="tonnes CO2 equivalent"
MC_f = rbind(MC_fb,MC_fa)


## Identification of Outlier Emission factors 
## Import Emission Factors National Electricity
## Table 1: European  Emission Factors for Electricity Consumption: tCO2/MWh
ipcc_eu_elec_co2 = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=2,skip=1)
## Table 2: European Emission Factors for Electricity Consumption: tCO2 eq/MWh
ipcc_eu_elec_ghg = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=3,skip=1)
## Table 3: European Emission Factors for Electricity Consumption: tCO2 eq/MWh
lca_eu_elec_ghg = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=4,skip=1)

## Table 4: SouthEast Emission Factors for Electricity Consumption: tCO2/MWh
ipcc_se_elec_co2 = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=5,skip=1)[-c(8:10),]
## Table 5: SouthEast Emission Factors for Electricity Consumption: tCO2 eq/MWh
ipcc_se_elec_ghg = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=6,skip=2)[-c(8:11),]
## Table 6: SouthEast Emission Factors for Electricity Consumption: tCO2 eq/MWh
lca_se_elec_ghg = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=7,skip=1)[-c(8:11),]

## Table 7: RestOfTheWorld Emission Factors for Electricity Consumption: tCO2/MWh
ipcc_row_elec_co2 = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=8,skip=1)
## Table 8: RestOfTheWorld Emission Factors for Electricity Consumption: tCO2 eq/MWh
ipcc_row_elec_ghg_far = read_excel("JRC-COM-NEEFE_1990-2018.xlsx",sheet=9,skip=1)

## ROW lacking 2016:2018 years 
x = ipcc_row_elec_co2[,-1]
x2 = ipcc_row_elec_ghg_far[,-1]
nx = dim(x)[1]
ffs = matrix(0,nx,3)
ffs2 = matrix(0,nx,3)
ffs[,]=as.matrix(x[,26])
ffs2[,]=as.matrix(x2[,26])
colnames(ffs) = c("2016","2017","2018")
colnames(ffs2) = c("2016","2017","2018")

## Gather factors by ipcc_co2/ghg and lca
row_co2_f = cbind(ipcc_row_elec_co2,ffs)
ipcc_co2_0 = rbind(ipcc_eu_elec_co2,ipcc_se_elec_co2,row_co2_f[-51,])
maxs_ipcc_co2 = as.matrix(as.numeric(apply(ipcc_co2_0[,-1], 1, function(x) max(x[x != "NaN"]))))
ipcc_co2_0 = cbind(ipcc_co2_0,maxs_ipcc_co2)

row_ghg_f = cbind(ipcc_row_elec_ghg_far,ffs2)
ipcc_ghg0 = rbind(ipcc_eu_elec_ghg,ipcc_se_elec_ghg,row_ghg_f[-51,])
maxs_ipcc_ghg = as.matrix(as.numeric(apply(ipcc_ghg0[,-1], 1, function(x) max(x[x != "NaN"]))))
ipcc_ghg0 = cbind(ipcc_ghg0,maxs_ipcc_ghg)

## Imputation of NaNs
nn = dim(ipcc_co2_0)[1]
for(i in 1:nn){
  ## Take the first non-NaN value for the country
  index_aux1 = min(which(ipcc_co2_0[i,-1]!="NaN"))
  index_aux2 = min(which(ipcc_ghg0[i,-1]!="NaN"))
  if(index_aux1>1){
    ipcc_co2_0[i,2:(index_aux1)]=as.character(ipcc_co2_0[i,(index_aux1+1)])
  }
  if(index_aux2>1){
    ipcc_ghg0[i,2:(index_aux2)]=as.character(ipcc_ghg0[i,(index_aux2+1)])
  }
}

lca_ghg0 = rbind(lca_eu_elec_ghg,lca_se_elec_ghg)
maxs_lca_ghg = as.matrix(as.numeric(apply(lca_ghg0[,-1], 1, function(x) max(x[x != "NaN"]))))
lca_ghg0 = cbind(lca_ghg0,maxs_lca_ghg)

nr = dim(lca_ghg0)[1]
for(i in 1:nr){
  # Take the first non-NaN value for the country
  index_aux = min(which(lca_ghg0[i,-1]!="NaN"))
  if(index_aux>1){
    lca_ghg0[i,2:(index_aux)]=as.character(lca_ghg0[i,(index_aux+1)])
  }
}

ipcc_co2 = ipcc_co2_0 %>% gather(year, ipcc_co2, -...1, -maxs_ipcc_co2)
ipcc_ghg = ipcc_ghg0 %>% gather(year, ipcc_ghg, -...1, -maxs_ipcc_ghg)
lca_ghg = lca_ghg0 %>% gather(year, lca_ghg, -...1, -maxs_lca_ghg)

## All together, country emission factors
con_facts0 = left_join(ipcc_co2,ipcc_ghg, by = c("...1","year")) 
con_facts = left_join(con_facts0,lca_ghg, by = c("...1","year")) 
con_facts = con_facts %>% rename( country = ...1)

## Add codes to the countries of the emission factors
con_facts$country = ifelse(con_facts$country=="Moldova","Moldova, Republic Of",con_facts$country)
con_facts$country = ifelse(con_facts$country=="Slovak Republic","Slovakia",con_facts$country)
con_facts$country = ifelse(con_facts$country=="Bosnia and Herzegovina","Bosnia-herzegovina",con_facts$country)
con_facts$country = ifelse(con_facts$country=="Czech Republic","Czechia",con_facts$country)

ccodes = unique(MC_f[,c("country_code","country")])
ccon_facts = left_join(con_facts,ccodes, by=("country"))
ccon_facts$energy_consumption_carrier_id = as.integer(1)

## Merge MyCovenant factors with the national factors
MC_f$year = as.integer(ifelse(MC_f$emission_inventory_year<2019,MC_f$emission_inventory_year,2018))
MC_f$energy_consumption_carrier_id = as.integer(MC_f$energy_consumption_carrier_id)
ccon_facts$year = as.integer(ccon_facts$year)

MyCovF = left_join(MC_f, ccon_facts, by = c("country", "country_code","year","energy_consumption_carrier_id")) 
MyCovF$ipcc_co2 = as.numeric(MyCovF$ipcc_co2)
MyCovF$ipcc_ghg = as.numeric(MyCovF$ipcc_ghg)
MyCovF$lca_ghg = as.numeric(MyCovF$lca_ghg)

## Import Emission Factors Fossil Fuels 
fossil = read.csv("fossil_fuel_emission_factor_G.csv", header=TRUE, encoding = "UTF-8")
colnames(fossil) = c("year","energy_consumption_carrier_id","ipcc_co2_foss"
                     ,"ipcc_ghg_foss","lca_ghg_foss")
## Imputation of values for carriers missing factor (thermal non-fossil)
fos_extra = matrix(0,7,5)
## missing carriers
fos_extra[,1] = as.integer(1990)
fos_extra[,2] = as.integer(c(2,11:15,22))
fos_extra[,3:5] = as.numeric(c(0.569,0.287,0.255,0.403,0,0,0.197))
fos_extra[2:4,4] = as.numeric(c(0.302,0.256,0.41))
fos_extra[1:7,5] = as.numeric(c(0.672,0.484,0.462,0.42,0.04,0.05,0.284))
colnames(fos_extra) = c("year","energy_consumption_carrier_id","ipcc_co2_foss"
                        ,"ipcc_ghg_foss","lca_ghg_foss")
## missing years
fos_extra2 = matrix(0,1,5)
fos_extra2[,1:2] = as.integer(c(1990,3))
fos_extra2[,3:5] = as.numeric(fossil[1,3:5])
colnames(fos_extra2) = c("year","energy_consumption_carrier_id","ipcc_co2_foss"
                         ,"ipcc_ghg_foss","lca_ghg_foss")
## All together
fossil = rbind(fossil,fos_extra,fos_extra2)
## Order in MyCode!
fossil = with(fossil,fossil[order(fossil$energy_consumption_carrier_id,fossil$year, 
                                  decreasing = c(FALSE), method = "radix"),])

## Fillout the missing years by carrier with the last known factor
ff = as.matrix(unique(fossil[,c("energy_consumption_carrier_id")]))
m = length(ff)
for(i in 1:m){
  k=ff[i,1]
  ss = subset(fossil,fossil$energy_consumption_carrier_id==as.character(k))
  mr = 2021-1990-dim(ss)[1]+1
  if(mr>0){
    fossex=t(matrix(rep(ss[dim(ss)[1],],mr),5,mr))
    fossex[,1] = seq((2021-mr+1),2021,1)
    colnames(fossex) = c("year","energy_consumption_carrier_id","ipcc_co2_foss",
                         "ipcc_ghg_foss","lca_ghg_foss")
    ssn = rbind(ss,fossex)
    ssn$max_ipcc_co2_foss = max(as.numeric(ssn$ipcc_co2_foss))
    ssn$max_ipcc_ghg_foss = max(as.numeric(ssn$ipcc_ghg_foss))
    ssn$max_lca_foss = max(as.numeric(ssn$lca_ghg_foss))  
  }
  if(i==1){
    fossil2 = ssn
  }else{
    fossil2 = rbind(fossil2,ssn)
  }
}

## Merge MyCovenant factors with the Fossil and thermal factors
fossil2$year = as.integer(fossil2$year)
fossil2$energy_consumption_carrier_id = as.integer(fossil2$energy_consumption_carrier_id)
fossil2$ipcc_co2_foss = as.numeric(fossil2$ipcc_co2_foss)
fossil2$ipcc_ghg_foss = as.numeric(fossil2$ipcc_ghg_foss)
fossil2$lca_ghg_foss = as.numeric(fossil2$lca_ghg_foss)

MCF = left_join(MyCovF, fossil2, by = c("year", "energy_consumption_carrier_id")) 

## Carriers included and missing in the study
carriers = unique(MCF[,c("energy_consumption_carrier_id","energy_consumption_carrier")])
carriers_no_factor = subset(carriers, !carriers$energy_consumption_carrier_id %in% fossil$energy_consumption_carrier_id)#Exclude electricity
carriers_wfactor = subset(carriers, carriers$energy_consumption_carrier_id %in% fossil$energy_consumption_carrier_id)
uu=unique(MCF[c("emission_factor_type_","emission_reporting_unit")])

## Compute the reference factor for direct comparison
n = dim(MCF)[1]
# By default we put LCA CO2-eq for fossil fuels
MCF$refact = MCF$lca_ghg_foss
MCF$maxfact = MCF$max_lca_foss
for (i in 1:n){
  if(MCF$energy_consumption_carrier_id[i]==1){
    if(MCF$emission_factor_type_[i]=="LCA" & !is.na(MCF$lca_ghg[i])){
      MCF$refact[i] = MCF$lca_ghg[i]
      MCF$maxfact[i] = MCF$maxs_lca_ghg[i]
    }
    else if(MCF$emission_factor_type_[i]=="LCA" & is.na(MCF$lca_ghg[i])){
      MCF$refact[i] = MCF$ipcc_ghg[i]
      MCF$maxfact[i] = MCF$maxs_ipcc_ghg[i] 
    }
    else{
      MCF$refact[i] = ifelse(MCF$emission_reporting_unit[i]=="tonnes CO2",MCF$ipcc_co2[i],MCF$ipcc_ghg[i])
      MCF$maxfact[i] = ifelse(MCF$emission_reporting_unit[i]=="tonnes CO2",MCF$maxs_ipcc_co2[i],MCF$maxs_ipcc_ghg[i])
    } 
  }
  else{
    if(MCF$emission_factor_type_[i]!="LCA"){
      MCF$refact[i] = ifelse(MCF$emission_reporting_unit[i]=="tonnes CO2",MCF$ipcc_co2_foss[i],MCF$ipcc_ghg_foss[i])
      MCF$maxfact[i] = ifelse(MCF$emission_reporting_unit[i]=="tonnes CO2",MCF$max_ipcc_co2_foss[i],MCF$max_ipcc_ghg_foss[i])
    } 
  }
}

## Detect anomalous national factors 
## Case study with tha available data
natfact_study = subset(MCF, !is.na(MCF$refact))
## inventories with national factors missing validation-national information 
natfact_nostudy = subset(MCF, is.na(MCF$refact))

## Anomalous factors
## If National
natfact_study$factor_outlier1 = as.factor(ifelse((natfact_study$emission_factor_origin=="National" & (abs(natfact_study$emission_factor_value - natfact_study$refact)>1.5*natfact_study$refact | natfact_study$emission_factor_value>natfact_study$maxfact)), "outlier", "normal"))
## If local
natfact_study$factor_outlier2 = as.factor(ifelse((natfact_study$emission_factor_origin=="Local" & natfact_study$emission_factor_value > 2*natfact_study$maxfact), "outlier", "normal"))
## If other
natfact_study$factor_outlier3 = as.factor(ifelse(is.na(natfact_study$emission_factor_origin) & natfact_study$emission_factor_value > 1.2*natfact_study$maxfact, "outlier", "normal"))
## Negative?
natfact_study$factor_outliern = as.factor(ifelse(natfact_study$emission_factor_value < 0, "outlier", "normal"))
## All together
natfact_study$factor_outlier = as.factor(ifelse(( natfact_study$factor_outlier1=="outlier"  | natfact_study$factor_outlier2=="outlier" | natfact_study$factor_outlier3=="outlier"  | natfact_study$factor_outliern=="outlier" ),"outlier","normal"))

outlier_factor = subset(natfact_study,natfact_study$factor_outlier=="outlier") 

## save result
## all the data
natfact_study = with(natfact_study, 
                     natfact_study[order(natfact_study$factor_outlier,natfact_study$emission_factor_origin, 
                                         decreasing = c(TRUE,FALSE), method = "radix"),])
write.table(natfact_study, file = "FactorFuneral.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## only outliers 
write.table(outlier_factor, file = "FactorOutliers.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## missing countries for factor validation (electricity)
ccxx = subset(ccodes, !ccodes$country %in% con_facts$country)
write.table(na.omit(ccxx), file = "MissingValidation_ElecFacts.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Complete table
write.table(MCF, file = "MyCovFactors.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Not validated, not included in the study
write.table(natfact_nostudy, file = "BMEIs_missing_factorValidation.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## export factors
## electricity
write.table(ccon_facts, file = "factorsElectricity.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
## thermal
write.table(fossil2, file = "factorsThermal.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


## MyCovenant Consumption cleaning
## Import the energy consumption
Cov0 = MC_v
Cov0$aux = paste(Cov0$action_plan_id,Cov0$inventory_year,"")
myc = setDT(Cov0)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year),.SDcols=c("energy_measure")]

## Only consumption that is positive
neg_cov = subset(Cov0,energy_measure<0)
Cov0 = subset(Cov0,energy_measure>=0)

## Import the outliers
## Consumption
e1 = read.table("electricOutliers.txt", header=TRUE)
e0 = read.table("electricOutliers0.txt", header=TRUE)
t1 = read.table("thermalOutliers.txt", header=TRUE)
t0 = read.table("thermalOutliers0.txt", header=TRUE)
e1$aux = paste(e1$action_plan_id,e1$inventory_year,"")
e0$aux = paste(e0$action_plan_id,e0$inventory_year,"")
t1$aux = paste(t1$action_plan_id,t1$inventory_year,"")
t0$aux = paste(t0$action_plan_id,t0$inventory_year,"")

## Specific list of outliers with very high buildings or transport..non allocated 
noal = read_excel("Outliers_Consumption.xlsx",sheet="outliers_nonallocated")
noal$aux = paste(noal$action_plan_id,noal$inventory_year,"")

## Missing validation
ev = read.table("BMEIs_missing_electricityValidation.txt", header=TRUE)
tv = read.table("BMEIs_missing_thermalValidation.txt", header=TRUE)
missing = rbind(ev,tv)
missing$aux = paste(missing$action_plan_id,missing$inventory_year,"")

## Rescue outliers for inclusion in open data
## Replace KWh to MWh
kw_e = subset(e1,e1$dec_elec>1000)
kw_t = subset(t1,t1$dec_elec>1000)

outs0 = rbind(kw_e[,c("organisation_name","emission_inventory_id")],kw_t[,c("organisation_name","emission_inventory_id")])
outs = unique(outs0[c("organisation_name","emission_inventory_id")])
n = dim(outs)[1]
for(i in 1:n){
  inv_aux=outs$emission_inventory_id[i]
  cov_aux=subset(Cov0,Cov0$emission_inventory_id==inv_aux)
  cov_aux$energy_measure2 = cov_aux$energy_measure*1e-3
  dec_elec2 = sum(cov_aux$energy_measure2/cov_aux$population)
  cat("\nOrganisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux,
      ",Energy consumption per capita:", dec_elec2)
  if(i==1){covs_m = cov_aux}else{covs_m=rbind(covs_m,cov_aux)}
}
## Careful inspection
invs2replace = subset(covs_m,covs_m$emission_inventory_id!=69705)# Vilanova i la Geltru
invs2replace$energy_measure2 = as.numeric(invs2replace$energy_measure2)
invs = invs2replace[,c("emission_inventory_id","energy_consumption_sector_id","energy_consumption_carrier_id","energy_measure2")]

## Replace curated values
Cov_rev = left_join(Cov0,invs,by=c("emission_inventory_id","energy_consumption_sector_id","energy_consumption_carrier_id"))
Cov_rev$energy_measure = ifelse(is.na(Cov_rev$energy_measure2),Cov_rev$energy_measure,Cov_rev$energy_measure2)

## Special case Vilanova i la Geltru (non allocated absurd)
Cov_rev$energy_measure = ifelse(Cov_rev$emission_inventory_id==69705 & Cov_rev$energy_consumption_sector_id==8,0,Cov_rev$energy_measure)
control = subset(Cov_rev,emission_inventory_id==69705) 

## remaining outliers
oute1 = subset(e1, !e1$emission_inventory_id %in% invs$emission_inventory_id) 
outt1 = subset(t1, !t1$emission_inventory_id %in% invs$emission_inventory_id) 
outt1 = subset(outt1,emission_inventory_id!=69705)

## Ignore acceptable ones although missing validation
kw = subset(missing,(missing$dec_elec>0.1 & missing$dec_elec<20))
## remaining missing validation
miss1 = subset(missing, !missing$emission_inventory_id %in% kw$emission_inventory_id) 

## Ignore BIG outliers in electricity with consumption per capita (dec_elec) less than 69
kw_e = subset(oute1,(oute1$dec_elec>0 & oute1$dec_elec<69))
kw_t = subset(outt1,(outt1$dec_elec>0 & outt1$dec_elec<174))
outs0 = rbind(kw_e[,c("organisation_name","emission_inventory_id")],kw_t[,c("organisation_name","emission_inventory_id")])
outs = unique(outs0[c("organisation_name","emission_inventory_id")])
invs2ignore = subset(outs,outs$emission_inventory_id!=69555)
Cov_rev1 = subset(Cov_rev,Cov_rev$emission_inventory_id!=69555)

## remaining outliers
oute2 = subset(oute1, !oute1$emission_inventory_id %in% invs2ignore$emission_inventory_id) 
outt2 = subset(outt1, !outt1$emission_inventory_id %in% invs2ignore$emission_inventory_id) 

## Delete the very high consumption for outliers with very high buildings or transport..non allocated.  
Cov_rev2a = subset(Cov_rev1,!(Cov_rev1$aux %in% noal$aux))

n = dim(noal)[1]
for(i in 1:n){
  inv_aux=noal$aux[i]
  covy = subset(Cov_rev1,Cov_rev1$aux==inv_aux) 
  cat("\nOld. Organisation:",as.character(covy$organisation_name[1]),"\nInventory:",noal$emission_inventory_id[i], inv_aux,
      ",Energy consumption per capita:", sum(covy$energy_measure/covy$population))
  if(inv_aux %in% c("21 2012 ","21 1990 ")){ ##don't do anything
    covy2 = subset(covy)#,!(covy$energy_measure>3100000) | covy$energy_measure<0)
    dec_elec2 = sum(covy2$energy_measure/covy2$population)
    cat("\nNew. Organisation:",as.character(covy2$organisation_name[1]),"\nInventory:",inv_aux,
        ",Energy consumption per capita:", dec_elec2)
  } else if(inv_aux %in% c("1576 2005 ")){
    covy2 = subset(covy,!(covy$energy_consumption_sector_id==8 | covy$energy_consumption_sector_id==12))
    dec_elec2 = sum(covy2$energy_measure/covy2$population)
    cat("\nNew. Organisation:",as.character(covy2$organisation_name[1]),"\nInventory:",inv_aux,
        ",Energy consumption per capita:", dec_elec2)
  }else{
    covy2 = subset(covy,!(((covy$energy_consumption_sector_id==8 | covy$energy_consumption_sector_id==12) &
                             covy$energy_measure>1100000) | covy$energy_measure<0))
    dec_elec2 = sum(covy2$energy_measure/covy2$population)
    cat("\nNew. Organisation:",as.character(covy2$organisation_name[1]),"\nInventory:",inv_aux,
        ",Energy consumption per capita:", dec_elec2)
  }
  if(i==1){covys= covy2}else{covys=rbind(covys,covy2)}
}
## Replace curated values
Cov_rev2 = rbind(Cov_rev2a,covys)
## Municipality of Loures (coal in KWh)
Cov_rev2$energy_measure = ifelse(Cov_rev2$emission_inventory_id==23959 & Cov_rev2$energy_consumption_sector_id==2 & Cov_rev2$energy_consumption_carrier_id==9,Cov_rev2$energy_measure*1e-3,Cov_rev2$energy_measure)
control = subset(Cov_rev2,emission_inventory_id==23959) 

## remaining outliers
oute3 = subset(oute2, !oute2$emission_inventory_id %in% covys$emission_inventory_id) 
outt3 = subset(outt2, !outt2$emission_inventory_id %in% covys$emission_inventory_id) 

## Ignore Gironella emission_inventory==32316
outt4 = subset(outt3,outt3$emission_inventory_id!=32316)

## Delete if total declared energy consumption is zero in all carriers, otherwise ignore
kw_e = subset(e0,e0$dec_elec==0)
kw_t = subset(t0,t0$dec_elec==0)
outs0 = rbind(kw_e[,c("organisation_name","emission_inventory_id")],kw_t[,c("organisation_name","emission_inventory_id")])
outs = outs0[duplicated(outs0$emission_inventory_id)|duplicated(outs0$emission_inventory_id, fromLast=TRUE),]
Cov_rev3 = subset(Cov_rev2,!(Cov_rev2$emission_inventory_id%in%outs$emission_inventory_id))

## remaining outliers (deleted and ignored ones)
oute01 = subset(e0, !e0$emission_inventory_id %in% outs0$emission_inventory_id) 
outt01 = subset(t0, !t0$emission_inventory_id %in% outs0$emission_inventory_id) 

## Outliers to the left (per capita)
kw_e = subset(oute01,oute01$dec_elec<0.01)
kw_t = subset(outt01,outt01$dec_elec<0.01)
outs0 = rbind(kw_e[,c("organisation_name","aux","emission_inventory_id")],kw_t[,c("organisation_name","aux","emission_inventory_id")])
outs = unique(outs0[c("organisation_name","aux","emission_inventory_id")])

n = dim(outs)[1]
for(i in 1:n){
  inv_aux=outs$aux[i]
  cov_aux=subset(Cov_rev3,Cov_rev3$aux==inv_aux)
  cat("\nOld. Organisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux, outs$emission_inventory_id[i],
      ",Energy consumption per capita:", sum(cov_aux$energy_measure/cov_aux$population))
  ## GWh 
  if(inv_aux %in% c("1311 2017 ","1203 2008 ","1246 2005 ","2179 2010 ","2612 2008 ","6135 2010 ")){
    cov_aux$energy_measure2 = cov_aux$energy_measure*1000
    dec_elec2 = sum(cov_aux$energy_measure2/cov_aux$population)
    cat("\nNew. Organisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux, outs$emission_inventory_id[i],
        ",Energy consumption per capita:", dec_elec2)
  }
  else if(inv_aux %in% c("185 2014 ")){
    cov_aux$energy_measure2 = ifelse(cov_aux$energy_consumption_carrier_id==1,cov_aux$energy_measure*1e3,cov_aux$energy_measure)
    dec_elec2 = sum(cov_aux$energy_measure2/cov_aux$population)
    cat("\nNew. Organisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux, outs$emission_inventory_id[i],
        ",Energy consumption per capita:", dec_elec2)
    ## per capita
  }else{
    cov_aux$energy_measure2 = cov_aux$energy_measure*cov_aux$population
    dec_elec2 = sum(cov_aux$energy_measure2/cov_aux$population)
    cat("\nNew. Organisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux, outs$emission_inventory_id[i],
        ",Energy consumption per capita:", dec_elec2)
  }
  if(i==1){covs_m = cov_aux}else{covs_m=rbind(covs_m,cov_aux)}
}
## Careful inspection
invs2replace = subset(covs_m,!covs_m$aux %in% c("332 2016 ","383 2014 ","1779 1990 ","1779 2005 ","3622 2012 ","4522 2013 ","5138 2005 ","5139 2005 ","5140 2005 ","5832 2014 ","5951 2017 ","2059 2016 ","2774 2009 ", "51668 2011 "))
invs2replace$energy_measure2 = as.numeric(invs2replace$energy_measure2)
invs = invs2replace[,c("aux","energy_consumption_sector_id","energy_consumption_carrier_id","energy_measure2")]

## Replace curated values
Cov_rev4 = left_join(Cov_rev3,invs,by=c("aux","energy_consumption_sector_id","energy_consumption_carrier_id"))
Cov_rev4$energy_measure = ifelse(is.na(Cov_rev4$energy_measure2.y),Cov_rev4$energy_measure,Cov_rev4$energy_measure2.y)

## Delete rejected inventories
Cov_rev4r = subset(Cov_rev4, !Cov_rev4$aux %in% c("332 2016 ","383 2014 ","1779 1990 ","1779 2005 ","3622 2012 ","4522 2013 ","5138 2005 ","5139 2005 ","5140 2005 ","5832 2014 ","5951 2017 ","2059 2016 ","2774 2009 ",
                                                  "1172 2006 "))

## remaining outliers
oute02 = subset(oute01, !oute01$aux %in% invs$aux) 
outt02 = subset(outt01, !outt01$aux %in% invs$aux) 

## Change the values for energy declared in per capita numbers (from missing validation)
kw = subset(miss1,(miss1$dec_elec<0.1))
outs = unique(kw[c("organisation_name","emission_inventory_id")])
n = dim(outs)[1]
for(i in 1:n){
  inv_aux=outs$emission_inventory_id[i]
  cov_aux=subset(Cov_rev3,Cov_rev3$emission_inventory_id==inv_aux)
  cov_aux$energy_measure2 = cov_aux$energy_measure*cov_aux$population
  dec_elec2 = sum(cov_aux$energy_measure2/cov_aux$population)
  cat("\nOrganisation:",as.character(outs$organisation_name[i]),"\nInventory:",inv_aux,
      ",Energy consumption per capita:", dec_elec2)
  if(i==1){covs_m = cov_aux}else{covs_m=rbind(covs_m,cov_aux)}
}
## Careful inspection
invs2replace = covs_m
invs2replace$energy_measure2 = as.numeric(invs2replace$energy_measure2)
invs = invs2replace[,c("emission_inventory_id","energy_consumption_sector_id","energy_consumption_carrier_id","energy_measure2")]

## Replace curated values
Cov_rev5 = left_join(Cov_rev4r,invs,by=c("emission_inventory_id","energy_consumption_sector_id","energy_consumption_carrier_id"))
Cov_rev5$energy_measure = ifelse(is.na(Cov_rev5$energy_measure2),Cov_rev5$energy_measure,Cov_rev5$energy_measure2)

## remaining missing validation
miss2 = subset(miss1, !miss1$emission_inventory_id %in% invs2replace$emission_inventory_id) 

## Sevilla: Delete the very high consumption for in buildings non allocated for liquid gad (44'mil) 
yy = subset(Cov_rev5,emission_inventory_id == 76290) 
Cov_rev6 = subset(Cov_rev5,!(Cov_rev5$emission_inventory_id == 76290 & 
                               Cov_rev5$energy_consumption_sector_id==8 &
                               Cov_rev5$energy_consumption_carrier_id==4  ))

## remaining outliers
outt5 = subset(outt4, outt4$emission_inventory_id != 76290)  

## revise remaining outliers and try to save
## for outliers in: oute3
## Arese 2015
xx = subset(Cov_rev6, Cov_rev6$emission_inventory_id %in% c(34667) & Cov_rev6$energy_consumption_sector_id %in% c(1))
Cov_rev6$energy_measure = ifelse(Cov_rev6$emission_inventory_id %in% c(34667) & Cov_rev6$energy_consumption_sector_id %in% c(1) ,Cov_rev6$energy_measure*1e-3,Cov_rev6$energy_measure)
oute3 = subset(oute3, !(oute3$emission_inventory_id %in% c(34667))) 
## Ignore Zubresti (the same for 2019 and 2020)
oute3 = subset(oute3, !(oute3$emission_inventory_id %in% c(73925))) 

## for outliers in: outt5
outt5 = subset(outt5,aux %in% Cov_rev6$aux)
## Hondarribia 2015
xx = subset(Cov_rev6, Cov_rev6$emission_inventory_id %in% c(75708) & Cov_rev6$energy_consumption_sector_id %in% c(11) & Cov_rev6$energy_consumption_carrier_id %in% c(7))
Cov_rev6$energy_measure = ifelse(Cov_rev6$emission_inventory_id %in% c(75708) & Cov_rev6$energy_consumption_sector_id %in% c(11) & Cov_rev6$energy_consumption_carrier_id %in% c(7),Cov_rev6$energy_measure*1e-3,Cov_rev6$energy_measure)
outt5 = subset(outt5, !(outt5$emission_inventory_id %in% c(75708))) 
## special case for Benissanet 2020
xx = subset(Cov_rev6, Cov_rev6$emission_inventory_id %in% c(73961) & Cov_rev6$energy_consumption_sector_id %in% c(2,3) & Cov_rev6$energy_consumption_carrier_id %in% c(3))
Cov_rev6$energy_measure = ifelse(Cov_rev6$emission_inventory_id %in% c(73961) & Cov_rev6$energy_consumption_sector_id %in% c(2,3) & Cov_rev6$energy_consumption_carrier_id %in% c(3),Cov_rev6$energy_measure*1e-3,Cov_rev6$energy_measure)
outt5 = subset(outt5, !(outt5$emission_inventory_id %in% c(73961))) 
## Raccuja 2017
xx = subset(Cov_rev6, Cov_rev6$emission_inventory_id %in% c(67646) & Cov_rev6$energy_consumption_sector_id %in% c(11) & Cov_rev6$energy_consumption_carrier_id %in% c(7))
Cov_rev6$energy_measure = ifelse(Cov_rev6$emission_inventory_id %in% c(67646) & Cov_rev6$energy_consumption_sector_id %in% c(11) & Cov_rev6$energy_consumption_carrier_id %in% c(7),Cov_rev6$energy_measure*1e-2,Cov_rev6$energy_measure)
outt5 = subset(outt5, !(outt5$emission_inventory_id %in% c(67646))) 
## El Morell 2020, La Pobla de Mafumet 2020
xx = subset(Cov_rev6, Cov_rev6$emission_inventory_id %in% c(73973, 73975) & Cov_rev6$energy_consumption_sector_id %in% c(2,3) & Cov_rev6$energy_consumption_carrier_id %in% c(3))
Cov_rev6$energy_measure = ifelse(Cov_rev6$emission_inventory_id %in% c(73973, 73975) & Cov_rev6$energy_consumption_sector_id %in% c(2,3) & Cov_rev6$energy_consumption_carrier_id %in% c(3),Cov_rev6$energy_measure*1e-2,Cov_rev6$energy_measure)
outt5 = subset(outt5, !(outt5$emission_inventory_id %in% c(73973, 73975))) 

## outliers in: oute02 & outt02 (to be deleted), miss2 (empty)
## keep Almeida, small town
outt02 = subset(outt02, !(outt02$emission_inventory_id %in% c(79411)))

## Exclude remaining outliers and missing-validation
Revcov = subset(Cov_rev6,!(Cov_rev6$emission_inventory_id  %in% oute3$emission_inventory_id |
                             Cov_rev6$emission_inventory_id  %in% outt5$emission_inventory_id |
                             Cov_rev6$emission_inventory_id  %in% miss2$emission_inventory_id |
                             Cov_rev6$emission_inventory_id  %in% oute02$emission_inventory_id |
                             Cov_rev6$emission_inventory_id  %in% outt02$emission_inventory_id))

## Exclude duplicated emission_inventories (same information-base year x2), if BEI, take BEI
emisd = setDT(Revcov)[, lapply(.SD, sum, na.rm=TRUE), by=.(action_plan_id,emission_inventory_id,inventory_year,bei_flag), 
                      .SDcols=c("energy_measure")]
emisd$em = round(emisd$energy_measure,0) 
dups = emisd[duplicated(emisd[,c("action_plan_id","inventory_year","em")])|duplicated(emisd[,c("action_plan_id","inventory_year","em")], fromLast=TRUE),]
dups = dups[with(dups, order(dups$action_plan_id,dups$inventory_year,-dups$bei_flag,-dups$emission_inventory_id)), ]
dups2 = setDT(dups)[, lapply(.SD, first), by=.(action_plan_id,inventory_year),.SDcols=c("emission_inventory_id")]
dups_out = subset(dups,!(emission_inventory_id %in% dups2$emission_inventory_id))
Revcov2 = subset(Revcov,!(emission_inventory_id %in% dups_out$emission_inventory_id))

## Exclude incoherent (by sectors) inventory. Dzierzoni?w 1996 and 2013
coves = subset(Revcov2,!(action_plan_id %in% c(1148) & inventory_year %in% c(1996,2013)) )

## examine consumption below 500
## totals
myc = setDT(coves)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year),.SDcols=c("energy_measure")]
myc2 = subset(myc,energy_measure<1000)
## Delete Aberdeenshire 2007 with 0 total consumption 
coves2 = subset(coves, !(aux %in% c("338 2007 ")))

## missing emission_factor_type
zz = subset(coves, is.na(emission_factor_type))
## Albolote 2008
albolote = subset(coves2,aux %in% c("1177 2008 "))
albolote$emission_factor_type = "IPCC"
albolote$emission_reporting_unit ="tonnes CO2 equivalent"
coves2b = subset(coves2,!(aux %in% c("1177 2008 ")))
coves2 = rbind(coves2b,albolote)

## sector-fuel consumption less than 1
coves3 = subset(coves2, energy_measure>=1)

## Error if Transport and (district heating, solar thermal or geothermal)
coves4 = subset(coves3,!(energy_consumption_sector_id==12 & energy_consumption_carrier_id %in% c(2,14,15)))

## temporal monitoring coherence
myc4 = setDT(coves4)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year),.SDcols=c("energy_measure")]
myc4 = myc4[with(myc4, order(action_plan_id,inventory_year)), ]
myc4$freq = 1
myc5 = setDT(myc4)[, lapply(.SD, sum, na.rm=TRUE), by=.(action_plan_id), .SDcols=c("freq")]
myc5 = subset(myc5,freq>1)
myc6 = subset(myc4,action_plan_id %in% myc5$action_plan_id)
myc7 = left_join(myc6,myc5,by=c("action_plan_id"))

## incoherent inventories
## 1. keep the ones that repeat and find the coherent line
myc8 = subset(myc7,freq.y>2)
apx = unique(myc8[,c("action_plan_id")])
n = dim(apx)[1]
for (i in 1:n){
  ap = apx$action_plan_id[i]
  ap1 = subset(myc8,action_plan_id %in% ap)
  m = dim(ap1)[1]
  for(j in 2:m){
    diff = (ap1$energy_measure[j]-ap1$energy_measure[j-1])/ap1$energy_measure[j-1]
    if (abs(diff)>0.7){cat("\nOrganisation:",as.character(ap1$organisation_name[j]),"\nInventory:",ap,ap1$inventory_year[j],"e0=",ap1$energy_measure[j-1],"e1=",ap1$energy_measure[j],"\nall:",ap1$energy_measure)
      print(ap1[,6])}
  }
}

## outlier MRs that are out of the city's trend go out
coves4 = subset(coves4,!(aux %in% c("9 2011 ","790 2010 ","882 2012 ","884 2012 ","991 2012 ","1472 2012 ","2054 2021 ","5004 2009 ","8944 2018 ", "8313 2018 ",
                                    "1177 2008 ","1277 2009 ","2439 2014 ","2645 2018 ","8471 2020 ","9439 2020 "))) # "882 2012","884 2012" out already

## triple outlier detection
## statistical outliers
lofe = read.table("stat_electricOutliers.txt", header=TRUE)
loft = read.table("stat_thermalOutliers.txt", header=TRUE)

seo = lofe[c("action_plan_id","inventory_year","emission_inventory_id","stat_outlier_iso","stat_outlier_lof")]
sto = loft[c("action_plan_id","inventory_year","emission_inventory_id","stat_outlier_iso_th","stat_outlier_lof_th")]

seo$action_plan_id = as.character(seo$action_plan_id)
sto$action_plan_id = as.character(sto$action_plan_id)
coves5 = left_join(coves4,seo,by=c("action_plan_id","inventory_year","emission_inventory_id"))
coves6 = left_join(coves5,sto,by=c("action_plan_id","inventory_year","emission_inventory_id"))

## electricity ISO
oz = subset(coves6,energy_consumption_carrier_id==1)
oz1 = setDT(oz)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year,stat_outlier_iso,stat_outlier_lof),.SDcols=c("energy_measure")]
oz1 = oz1[with(oz1, order(-stat_outlier_iso)), ] 
oz2 = subset(oz1,stat_outlier_iso>0.6)
apx = unique(oz2[,c("action_plan_id")])
n = dim(apx)[1]
for (i in 1:n){
  ap = apx$action_plan_id[i]
  ap1 = subset(oz1,action_plan_id %in% ap)
  cat("\nOrganisation:",as.character(ap1$organisation_name[1]),"\nInventory:",ap,"years:",ap1$inventory_year,"energies:",ap1$energy_measure)
  print(ap1[,8])
}

## Zubresti 2020
coves6$energy_measure = ifelse(coves6$aux %in% c("31861 2020 ") & coves6$energy_consumption_carrier_id==1 & coves6$energy_consumption_sector_id==3, coves6$energy_measure*1e-3,coves6$energy_measure)
## Zubresti 2014
coves6$energy_measure = ifelse(coves6$aux %in% c("3813 2014 ") & coves6$energy_consumption_carrier_id==1, coves6$energy_measure*1e-1,coves6$energy_measure)
## Bilbao 
coves6$energy_measure = ifelse(coves6$aux %in% c("1409 2009 ") & coves6$energy_consumption_carrier_id==1 & coves6$energy_consumption_sector_id==2, coves6$energy_measure*1e-1,coves6$energy_measure)

## Chalkidona 2014, no remedy; and Vienna 2014 out, buildings non-allocated suspicious
coves6 = subset(coves6,!(aux %in% c("3813 2014 ","4331 2014 ")))
control = subset(coves6,organisation_name=="Vienna")

## electricity LOF
oz = subset(coves6,energy_consumption_carrier_id==1)
oz1 = setDT(oz)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year,stat_outlier_iso,stat_outlier_lof),.SDcols=c("energy_measure")]
oz1 = oz1[with(oz1, order(-stat_outlier_lof)), ]
oz2 = subset(oz1,stat_outlier_lof>6)
apx = unique(oz2[,c("action_plan_id")])
n = dim(apx)[1]
for (i in 1:n){
  ap = apx$action_plan_id[i]
  ap1 = subset(oz1,action_plan_id %in% ap)
  cat("\nOrganisation:",as.character(ap1$organisation_name[1]),"\nInventory:",ap,"years:",ap1$inventory_year,"energies:",ap1$energy_measure)
  print(ap1[,8])
}


## thermal ISO
oz = subset(coves6,energy_consumption_carrier_id!=1)
oz1 = setDT(oz)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year,stat_outlier_iso_th,stat_outlier_lof_th),.SDcols=c("energy_measure")]
oz1 = oz1[with(oz1, order(-stat_outlier_iso_th)), ]
oz2 = subset(oz1,stat_outlier_iso_th>0.6)
apx = unique(oz2[,c("action_plan_id")])
n = dim(apx)[1]
for (i in 1:n){
  ap = apx$action_plan_id[i]
  ap1 = subset(oz1,action_plan_id %in% ap)
  cat("\nOrganisation:",as.character(ap1$organisation_name[1]),"\nInventory:",ap,"years:",ap1$inventory_year,"energies:",ap1$energy_measure)
  print(ap1[,8])
}

## thermal LOF
oz = subset(coves6,energy_consumption_carrier_id!=1)
oz1 = setDT(oz)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year,stat_outlier_iso_th,stat_outlier_lof_th),.SDcols=c("energy_measure")]
oz1 = oz1[with(oz1, order(-stat_outlier_lof_th)), ]
oz2 = subset(oz1,stat_outlier_lof_th>6)
apx = unique(oz2[,c("action_plan_id")])
n = dim(apx)[1]
for (i in 1:n){
  ap = apx$action_plan_id[i]
  ap1 = subset(oz1,action_plan_id %in% ap)
  cat("\nOrganisation:",as.character(ap1$organisation_name[1]),"\nInventory:",ap,"years:",ap1$inventory_year,"energies:",ap1$energy_measure)
  print(ap1[,8])
}

## range-based outlier detection by size of city, small, medium and large
coves_tot = setDT(coves6)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_name,action_plan_id,emission_inventory_id,inventory_year,population_in_the_inventory_year),.SDcols=c("energy_measure")]
xx = subset(coves_tot,organisation_name== "Koeln") #population=0
small = subset(coves_tot,population_in_the_inventory_year<=5000 & population_in_the_inventory_year>0)
medium =  subset(coves_tot,population_in_the_inventory_year>5000 & population_in_the_inventory_year<200000)
big = subset(coves_tot,population_in_the_inventory_year>=200000)

## range by size
range(small$energy_measure)
cat("max=",subset(small,energy_measure==max(small$energy_measure))$organisation_name[1])
range(medium$energy_measure)
cat("max=",subset(medium,energy_measure==max(medium$energy_measure))$organisation_name[1])
range(big$energy_measure)
cat("max=",subset(big,energy_measure==max(big$energy_measure))$organisation_name[1])

## end of outlier search
apply(coves6, 2, function(x) any(is.na(x)))

coves7 = coves6[,c("organisation_id","organisation_name","action_plan_id",
                   "action_plan_submission_status_id","monitoring_report_id",
                   "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                   "inventory_year","emission_factor_type","emission_reporting_unit","emission_factor_type_id","energy_measure","emission_measure","item_text","energy_consumption_sector",
                   "energy_consumption_sector_id","energy_consumption_carrier","energy_consumption_carrier_id","population_in_the_inventory_year","country_code","IEA_code","area","country",
                   "lau_comb_elab","nuts3_code_elab","population","organisation_latitude","organisation_longitude")]
write.table(coves7, file = "CleanedConsumption.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


## Clean emissions
## Import the emission factors (original)
Fac = MC_f

## Factor outlier analysis 
f1 = read.table("FactorOutliers.txt", header=TRUE)
f1 = subset(f1,!is.na(refact))
fv = read.table("BMEIs_missing_factorValidation.txt", header=TRUE)
fv = subset(fv,!is.na(refact))
reference = read.table("MyCovFactors.txt", header=TRUE)
reference = subset(reference,!is.na(refact))

## Reference factors
f_elec = read.table("factorsElectricity.txt", header=TRUE)
f_thermal = read.table("factorsThermal.txt", header=TRUE)

## 1. Replace outlier emission factors
f1$emission_inventory_id = as.character(f1$emission_inventory_id)
Fac_rev = left_join(Fac,f1,by=c("emission_inventory_id","emission_reporting_unit","emission_factor_type_","energy_consumption_carrier_id",
                                "emission_factor_origin"))

Fac_rev$emission_factor_value = ifelse(!is.na(Fac_rev$refact),Fac_rev$refact,Fac_rev$emission_factor_value.x)

## Include acceptable factors although missing validation
fm = subset(fv,(fv$emission_factor_value<1 & !is.na(fv$emission_inventory_year)))
## Replace Bergen and Stavanger's National-electricity factor to 0.01 in 1991
xx = subset(Fac_rev,action_plan_id.x %in% c(316,164) & energy_consumption_carrier_id==1)# & emission_inventory_year.x==1991)
j = which(Fac_rev$action_plan_id.x %in% c(316,164) & Fac_rev$emission_inventory_year.x==1991 & Fac_rev$energy_consumption_carrier_id==1)
Fac_rev$emission_factor_value[j] = 0.01

## remaining missing validation
fm1 = subset(fv,is.na(fv$emission_inventory_year))


## Exclude remaining outliers and missing-validation
Revfac = subset(Fac_rev,!(Fac_rev$emission_inventory_id %in% fm1$emission_inventory_id))

## Join consumption and factors (local over national)
ef = with(Revfac, Revfac[order(Revfac$emission_inventory_id,
                               Revfac$energy_consumption_carrier_id,Revfac$emission_factor_origin,
                               decreasing = c(FALSE), method = "radix"),])
ef = as.data.frame(ef)
local = subset(ef,is.na(emission_factor_value) & energy_consumption_carrier_id==1)
efs = setDT(ef)[, lapply(.SD, first), by=.(emission_inventory_id,energy_consumption_carrier_id), 
                .SDcols=c("emission_factor_origin","emission_reporting_unit","emission_factor_type_","emission_factor_value")]

Revcov = read.table("CleanedConsumption.txt", header=TRUE)
Revcov$emission_inventory_id = as.character(Revcov$emission_inventory_id)
Refs = left_join(Revcov,efs,by=c("emission_inventory_id","energy_consumption_carrier_id"))

## 2. Imputation of missing factors
## How many missing factors in total:
nas_in_Refs = subset(Refs,is.na(Refs$emission_factor_value))

## missing the emission_reporting_unit?
miss_rep_unit = subset(Refs,is.na(emission_reporting_unit.x)) 

## 2.1 Control the fuel for district heating
## LHCP: Local heat/cold production
lhcp = read_excel("dfLHCP_AP.xlsx")
heat = subset(lhcp,(lhcp$energy_carrier_id %in% c(5,10,11) & lhcp$energy_supply_type_id==13 ))
heat2 = subset(heat,heat$energy_produced>0 & heat$emission_inventory_id %in% nas_in_Refs$emission_inventory_id)
heat4 = unique(heat2[c("emission_inventory_id")])

factors_dh = matrix(0,3,2)
factors_dh[,1] = as.numeric(c(0.569,0.281,0.419))
factors_dh[,2] = as.numeric(c(0.583,0.333,0.672))

n = dim(heat4)[1]
j=0
for(i in 1:n){
  emi_aux = heat4$emission_inventory_id[i]
  semi_aux = subset(heat2,heat2$emission_inventory_id==emi_aux)
  cemi_aux = subset(nas_in_Refs,nas_in_Refs$emission_inventory_id==emi_aux & nas_in_Refs$energy_consumption_carrier_id==2)
  total = sum(semi_aux$energy_produced)
  biomass = ifelse(11 %in% semi_aux$energy_carrier_id,subset(semi_aux,semi_aux$energy_carrier_id==11)$energy_produced,0)
  natu_gas = ifelse(5 %in% semi_aux$energy_carrier_id,subset(semi_aux,semi_aux$energy_carrier_id==5)$energy_produced,0)
  plant_oil = ifelse(10 %in% semi_aux$energy_carrier_id,subset(semi_aux,semi_aux$energy_carrier_id==10)$energy_produced,0)
  weights = c(biomass,natu_gas,plant_oil)
  factor_type = as.character(cemi_aux$emission_factor_type[1])
  if(dim(cemi_aux)[1]>0){
    fac1 = factors_dh[,1] %*% weights / total
    fac2 = factors_dh[,2] %*% weights / total
    emission_factor_m = ifelse(factor_type=="IPCC",fac1,fac2) 
    resf = as.data.frame(cbind(emi_aux,2,emission_factor_m))
    if(j==0){demis = resf}else{demis=rbind(demis,resf)}
    j=j+1
  }
}
colnames(demis) = c("emission_inventory_id","energy_consumption_carrier_id","emission_factor_m")
demis$emission_inventory_id = as.character(demis$emission_inventory_id)
Refsh = left_join(Refs,demis,by=c("emission_inventory_id","energy_consumption_carrier_id"))
zz = subset(Refsh,!is.na(emission_factor_m))
Refsh$emission_factor_value = ifelse(is.na(Refsh$emission_factor_value),Refsh$emission_factor_m,Refsh$emission_factor_value)
nas_in_Refsh = subset(Refsh,is.na(Refsh$emission_factor_value))

## 2.2 imputation by country, year, carrier and emission_reporting_unit and emission_factor_type
reference$inventory_year = reference$emission_inventory_year
reference$emission_factor_type = reference$emission_factor_type_
new_facts = unique(reference[c("country_code","inventory_year","emission_reporting_unit","emission_factor_type","energy_consumption_carrier_id","refact")])
apply(new_facts, 2, function(x) any(is.na(x)))

new_facts = with(new_facts, new_facts[order(new_facts$country_code,new_facts$inventory_year,new_facts$emission_factor_type,new_facts$emission_reporting_unit,new_facts$energy_consumption_carrier_id,decreasing = c(FALSE), method = "radix"),])
Refsh = Refsh %>% rename(emission_reporting_unit = emission_reporting_unit.x)
## new var for merging (data up to 2018)
new_facts$year = new_facts$inventory_year
Refsh$year = as.integer(ifelse(Refsh$inventory_year<2019,Refsh$inventory_year,2018))
Refs2 = left_join(Refsh,new_facts,by=c("country_code","year","emission_reporting_unit","emission_factor_type","energy_consumption_carrier_id"))
ii = subset(Refs2,!is.na(emission_factor_origin))

## Imputation
imp_f = subset(Refs2,is.na(emission_factor_value)) 
Refs2$emission_factor_value = ifelse(is.na(Refs2$emission_factor_value),Refs2$refact,Refs2$emission_factor_value)
## How many remaining missing factors:
nas_in_Refs2 = subset(Refs2,is.na(Refs2$emission_factor_value))


## 2.3 Imputation by country and carrier from the reference source
## import national references
new_facts_e1 = f_elec[c("country_code","energy_consumption_carrier_id","year","ipcc_co2")]
new_facts_e1$emission_reporting_unit = "tonnes CO2"
new_facts_e1$emission_factor_type = "IPCC"
new_facts_e1 = new_facts_e1 %>% rename( emission_factor_value = ipcc_co2)
new_facts_e2 = f_elec[c("country_code","energy_consumption_carrier_id","year","ipcc_ghg")]
new_facts_e2$emission_reporting_unit = "tonnes CO2 equivalent"
new_facts_e2$emission_factor_type = "IPCC"
new_facts_e2 = new_facts_e2 %>% rename( emission_factor_value = ipcc_ghg)
new_facts_e3 = f_elec[c("country_code","energy_consumption_carrier_id","year","lca_ghg")]
new_facts_e3$emission_reporting_unit = "tonnes CO2 equivalent"
new_facts_e3$emission_factor_type = "LCA"
new_facts_e3 = new_facts_e3 %>% rename( emission_factor_value = lca_ghg)

new_facts_e = rbind(new_facts_e1,new_facts_e2,new_facts_e3)
apply(new_facts_e, 2, function(x) any(is.na(x)))
new_facts_e = subset(new_facts_e,!is.na(country_code) & !is.na(emission_factor_value))

new_facts_t1 = f_thermal[c("energy_consumption_carrier_id","year","ipcc_co2_foss")]
new_facts_t1$emission_reporting_unit = "tonnes CO2"
new_facts_t1$emission_factor_type = "IPCC"
new_facts_t1 = new_facts_t1 %>% rename( emission_factor_value = ipcc_co2_foss)
new_facts_t2 = f_thermal[c("energy_consumption_carrier_id","year","ipcc_ghg_foss")]
new_facts_t2$emission_factor_type = "IPCC"
new_facts_t2$emission_reporting_unit = "tonnes CO2 equivalent"
new_facts_t2 = new_facts_t2 %>% rename( emission_factor_value = ipcc_ghg_foss)
new_facts_t3 = f_thermal[c("energy_consumption_carrier_id","year","lca_ghg_foss")]
new_facts_t3$emission_factor_type = "LCA"
new_facts_t3$emission_reporting_unit = "tonnes CO2 equivalent"
new_facts_t3 = new_facts_t3 %>% rename( emission_factor_value = lca_ghg_foss)

new_facts_t = rbind(new_facts_t1,new_facts_t2,new_facts_t3)
new_facts_t = new_facts_t %>% rename( emission_factor = emission_factor_value)
apply(new_facts_t, 2, function(x) any(is.na(x)))

Refs3 = left_join(Refs2,new_facts_e,by=c("country_code","year","emission_reporting_unit","emission_factor_type","energy_consumption_carrier_id"))
Refs3$emission_factor = ifelse(is.na(Refs3$emission_factor_value.x),Refs3$emission_factor_value.y,Refs3$emission_factor_value.x)

Refs4 = left_join(Refs3,new_facts_t,by=c("year","emission_reporting_unit","emission_factor_type","energy_consumption_carrier_id"))
Refs4$factor = ifelse(is.na(Refs4$emission_factor.x),Refs4$emission_factor.y,Refs4$emission_factor.x)

## Still missing:
nas_in_Refs4 = subset(Refs4,is.na(Refs4$factor))
ss = subset(Refs4,action_plan_id==1316)

## 2.4 National/sub-national factor types
Refs4_aux = subset(Refs4,Refs4$emission_factor_type=="National/sub-national")
nas_in_Refs4_aux = subset(Refs4_aux,is.na(Refs4_aux$factor))

new_facts_e_aux = subset(new_facts_e, new_facts_e$emission_factor_type=="IPCC")# & new_facts_e$emission_reporting_unit == "tonnes CO2")
new_facts_t_aux = subset(new_facts_t, new_facts_t$emission_factor_type=="IPCC")# & new_facts_t$emission_reporting_unit == "tonnes CO2")
Refs5_aux = left_join(Refs4_aux,new_facts_e_aux,by=c("country_code","year","emission_reporting_unit","energy_consumption_carrier_id"))
Refs5_aux$factor = ifelse(is.na(Refs5_aux$factor),Refs5_aux$emission_factor_value,Refs5_aux$factor)
Refs6_aux = left_join(Refs5_aux,new_facts_t_aux,by=c("year","emission_reporting_unit","energy_consumption_carrier_id"))
Refs6_aux$factor = ifelse(is.na(Refs6_aux$factor),Refs6_aux$emission_factor,Refs6_aux$factor)

nas_in_Refs6 = subset(Refs6_aux,is.na(Refs6_aux$factor))

Refs4_dual = subset(Refs4,Refs4$emission_factor_type!="National/sub-national")
Refs4_dual = Refs4_dual[c("organisation_id","organisation_name","country_code","IEA_code","bei_flag","area","population_in_the_inventory_year","population","action_plan_id","emission_inventory_id","inventory_year.x","year","energy_consumption_carrier_id","energy_consumption_sector_id","emission_reporting_unit","emission_factor_type","emission_factor_origin","refact","factor","energy_measure")]
Refs6_aux = Refs6_aux[c("organisation_id","organisation_name","country_code","IEA_code","bei_flag","area","population_in_the_inventory_year","population","action_plan_id","emission_inventory_id","inventory_year.x","year","energy_consumption_carrier_id","energy_consumption_sector_id","emission_reporting_unit","emission_factor_type.x","emission_factor_origin","refact","factor","energy_measure")]
Refs6_aux = Refs6_aux %>% rename(emission_factor_type=emission_factor_type.x)
Refs7 = rbind(Refs4_dual,Refs6_aux)
nas_in_Refs7 = subset(Refs7,is.na(Refs7$factor))
ss = subset(Refs7,action_plan_id==1316)

## 2.5 missing with emission factor type LCA but tonnes co2
Refs7_aux = subset(Refs7,Refs7$emission_factor_type=="LCA")
nas_in_Refs7_aux = subset(Refs7_aux,is.na(Refs7_aux$factor))

new_facts_e_aux = subset(new_facts_e, new_facts_e$emission_factor_type=="LCA")
new_facts_t_aux = subset(new_facts_t, new_facts_t$emission_factor_type=="LCA")
Refs8_aux = left_join(Refs7_aux,new_facts_e_aux,by=c("country_code","year","energy_consumption_carrier_id"))
Refs8_aux$factor = ifelse(is.na(Refs8_aux$factor),Refs8_aux$emission_factor_value,Refs8_aux$factor)
Refs9_aux = left_join(Refs8_aux,new_facts_t_aux,by=c("year","energy_consumption_carrier_id"))
Refs9_aux$factor = ifelse(is.na(Refs9_aux$factor),Refs9_aux$emission_factor,Refs9_aux$factor)

nas_in_Refs9 = subset(Refs9_aux,is.na(Refs9_aux$factor))

Refs7_aux_dual = subset(Refs7,Refs7$emission_factor_type!="LCA" | is.na(Refs7$emission_factor_type))
Refs7_aux_dual = Refs7_aux_dual[c("organisation_id","organisation_name","country_code","IEA_code","bei_flag","area","population_in_the_inventory_year","population","action_plan_id","emission_inventory_id","inventory_year.x","year","energy_consumption_carrier_id","energy_consumption_sector_id","emission_reporting_unit","emission_factor_type","emission_factor_origin","refact","factor","energy_measure")]
Refs9_aux = Refs9_aux[c("organisation_id","organisation_name","country_code","IEA_code","bei_flag","area","population_in_the_inventory_year","population","action_plan_id","emission_inventory_id","inventory_year.x","year","energy_consumption_carrier_id","energy_consumption_sector_id","emission_reporting_unit.x","emission_factor_type.x","emission_factor_origin","refact","factor","energy_measure")]
Refs9_aux = Refs9_aux %>% rename( emission_factor_type = emission_factor_type.x, emission_reporting_unit=emission_reporting_unit.x)
Refs8 = rbind(Refs7_aux_dual,Refs9_aux)
nas_in_Refs8 = subset(Refs8,is.na(Refs8$factor))

## 3. Case by case
aa = subset(Fac,country_code=="no" & energy_consumption_carrier_id==1)
Refs8$factor = ifelse(Refs8$action_plan_id %in% c(163,700) & Refs8$inventory_year.x %in% c(2006,2009) & is.na(Refs8$factor) & Refs8$energy_consumption_sector_id == 1, 0.132, Refs8$factor)
bb = subset(Refs8,action_plan_id %in% c(163,700))
nas_in_Refs8 = subset(Refs8,is.na(factor))
## Switzerland: Montreux 2010
Refs8$factor = ifelse(is.na(Refs8$factor) & Refs8$action_plan_id %in% c(1316), 0.459, Refs8$factor)
nas_in_Refs8 = subset(Refs8,is.na(factor))

Refs9 = Refs8
xx = subset(Refs9,is.na(emission_factor_type))


## 4. clean final factors
Refs9$factor = ifelse(Refs9$energy_consumption_carrier_id>2 & Refs9$energy_consumption_carrier_id<11 &  Refs9$factor<0.15, Refs9$refact, Refs9$factor)

nas_in_Refs9 = subset(Refs9,is.na(factor))
## Duisburg Liquid gas, LCA tonnes reference not available 
Refs9$factor = ifelse(Refs9$action_plan_id==1558 & Refs9$inventory_year.x==1995 & Refs9$energy_consumption_carrier_id==4,0.2,Refs9$factor)

## check
nas_in_Refs9 = subset(Refs9,is.na(factor))

drop = c("year","refact")
Refs9 = Refs9[,!(names(Refs9) %in% drop)]
Refs9 = Refs9 %>% rename(inventory_year = inventory_year.x)

## Cleanded emissions
Refs9$emissions = Refs9$factor*Refs9$energy_measure
apply(Refs9, 2, function(x) any(is.na(x)))

## Export cleaned emissions
write.table(Refs9, file = "CleanedEmissions.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


## Preliminary validated files
## Import the cleaned emissions
dfEI_cl = read.table("CleanedEmissions.txt", header=TRUE)
dfEI_cl = dfEI_cl %>% rename(emission_factor_value = factor)

## DHC emission factors
Fac = MC_f
dhc = subset(dfEI_cl,emission_factor_value %in% c(0.569,0.672) & energy_consumption_carrier_id ==2)
dhc$aux = paste0(dhc$action_plan_id,dhc$inventory_year,dhc$energy_consumption_carrier_id)
Fac$aux = paste0(Fac$action_plan_id,Fac$inventory_year,Fac$energy_consumption_carrier_id)
aa = subset(dhc,(aux %in% Fac$aux))
dhc = unique(dhc[c("action_plan_id","inventory_year","emission_inventory_id","energy_consumption_carrier_id","emission_factor_type","emission_reporting_unit","emission_factor_value","aux","country_code")])
xu = unique(dhc[c("action_plan_id","inventory_year")])

## check for other inventories
dhc2 = subset(dfEI_cl,action_plan_id %in% dhc$action_plan_id & energy_consumption_carrier_id ==2)
dhc2$aux = paste0(dhc2$action_plan_id,dhc2$inventory_year,dhc2$energy_consumption_carrier_id)
dhc3 = subset(dhc2, !(aux %in% dhc$aux))
dhc3 = unique(dhc3[c("action_plan_id","inventory_year","bei_flag","energy_consumption_carrier_id","emission_factor_value","emission_inventory_id")])
dhc4 = left_join(dhc,dhc3,by=c("action_plan_id"))
dhc4$diff = dhc4$inventory_year.x-dhc4$inventory_year.y
dhc4 = subset(dhc4,!is.na(diff))
dhc4 = dhc4[with(dhc4, order(dhc4$action_plan_id,abs(dhc4$diff),-dhc4$bei_flag,-dhc4$emission_inventory_id.y)), ]
newf1 = setDT(dhc4)[, lapply(.SD, first), by=.(action_plan_id,inventory_year.x,energy_consumption_carrier_id.x),.SDcols=c("emission_factor_value.y","aux")]
newf1 = newf1 %>% rename(inventory_year=inventory_year.x,energy_consumption_carrier_id=energy_consumption_carrier_id.x,emission_factor_value=emission_factor_value.y)
xu2 = unique(newf1[,c("action_plan_id","inventory_year")])

## check for values in their countries by factor type and reporting unit
new2 = subset(dfEI_cl,country_code %in% dhc$country_code & energy_consumption_carrier_id ==2)
new2$aux = paste0(new2$action_plan_id,new2$inventory_year,new2$energy_consumption_carrier_id)
new3 = subset(new2, !(aux %in% dhc$aux))
new3 = unique(new3[c("action_plan_id","inventory_year","bei_flag","energy_consumption_carrier_id","emission_factor_value","country_code","emission_factor_type","emission_reporting_unit","emission_inventory_id")])
new4 = left_join(dhc,new3,by=c("country_code","emission_factor_type","emission_reporting_unit"))
new4$diff = new4$inventory_year.x-new4$inventory_year.y
new4 = subset(new4,!is.na(diff))
new4 = new4[with(new4, order(new4$action_plan_id.x,abs(new4$diff),-new4$bei_flag,-new4$emission_inventory_id.y)), ]
newf2 = setDT(new4)[, lapply(.SD, first), by=.(action_plan_id.x,inventory_year.x,energy_consumption_carrier_id.x),.SDcols=c("emission_factor_type","emission_reporting_unit","emission_factor_value.y","aux")]
newf2 = newf2 %>% rename(action_plan_id=action_plan_id.x,inventory_year=inventory_year.x,energy_consumption_carrier_id=energy_consumption_carrier_id.x,emission_factor_value=emission_factor_value.y)
newf2 = subset(newf2,!(aux%in%newf1$aux))
newf2 = newf2[,c("action_plan_id","inventory_year","energy_consumption_carrier_id","emission_factor_value","aux")]
xu3 = unique(newf2[,c("action_plan_id","inventory_year")])

news = rbind(newf1,newf2)

## Manual assignation
missing = subset(dhc,!(aux%in%newf1$aux) & !(aux%in%newf2$aux))
xx = subset(dfEI_cl,emission_factor_type=="LCA" & energy_consumption_carrier_id==2 & country_code%in%c("fr"))
newf3 = missing[,c("action_plan_id","inventory_year","energy_consumption_carrier_id","emission_factor_value")]
newf3$emission_factor_value = ifelse(newf3$action_plan_id==316,0.1,ifelse(newf3$action_plan_id==1300,0.282,ifelse(newf3$action_plan_id==35371,0.165,0.27)))

news = news[,-c("aux")]
news2 = rbind(news,newf3)

## correct the EFs
dfEI_cl_corrected = left_join(dfEI_cl,news2,by=c("action_plan_id","inventory_year","energy_consumption_carrier_id"))
dfEI_cl_corrected$emission_factor_value = ifelse(is.na(dfEI_cl_corrected$emission_factor_value.y),dfEI_cl_corrected$emission_factor_value.x,dfEI_cl_corrected$emission_factor_value.y)
drop = c("emission_factor_value.x","emission_factor_value.y")
dfEI_cl_corrected = dfEI_cl_corrected[,!(names(dfEI_cl_corrected) %in% drop)]
dfEI_cl = dfEI_cl_corrected 

## Original emissions comparison
emisa = read_excel("dfEI_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",3),rep("numeric",2),rep("text",5),"numeric"))
emisr = read_excel("dfEI_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",3),rep("numeric",2),rep("text",5),"numeric"))
emis = rbind(emisa,emisr)
emis$organisation_id = as.character(emis$organisation_id)
emis$action_plan_id = as.character(emis$action_plan_id)
## recover other variables
other_vs = unique(emis[c("organisation_id","action_plan_id","emission_inventory_id","energy_consumption_carrier_id",
                         "energy_consumption_sector_id","action_plan_submission_status_id","monitoring_report_id",
                         "monitoring_report_submission_status_id","item_text","energy_consumption_sector","energy_consumption_carrier")])

dfEI_cl$emission_inventory_id = as.character(dfEI_cl$emission_inventory_id)
dfEI_cl$energy_consumption_carrier_id = as.character(dfEI_cl$energy_consumption_carrier_id)
dfEI_cl$energy_consumption_sector_id = as.character(dfEI_cl$energy_consumption_sector_id)
dfEI_cl2 = left_join(dfEI_cl,other_vs,by=c("organisation_id","action_plan_id","emission_inventory_id","energy_consumption_carrier_id",
                                           "energy_consumption_sector_id"))

## population in the inventory year 
aa = subset(dfEI_cl2,dfEI_cl2$population_in_the_inventory_year<100) 
dfEI_cl2$dif = dfEI_cl2$population - dfEI_cl2$population_in_the_inventory_year
dfEI_cl2$population_in_the_inventory_year = ifelse(dfEI_cl2$population_in_the_inventory_year == 0, dfEI_cl2$population, dfEI_cl2$population_in_the_inventory_year)
dfEI_cl2$population_in_the_inventory_year = ifelse(dfEI_cl2$action_plan_id %in% c(306,456,990,1006,1048,1533,1829), dfEI_cl2$population, dfEI_cl2$population_in_the_inventory_year)
dfEI_cl2$dif = dfEI_cl2$population - dfEI_cl2$population_in_the_inventory_year
dfEI_cl2 = dfEI_cl2[with(dfEI_cl2, order(-dfEI_cl2$dif)), ]

dfEI_cl2$activity_reporting_unit = "MWh/year"
dfEI_cl2 = dfEI_cl2 %>% rename(emission_measure=emissions)

dfEI_cl3 = dfEI_cl2[c("organisation_id","organisation_name","action_plan_id",
                      "action_plan_submission_status_id","monitoring_report_id",
                      "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                      "inventory_year","activity_reporting_unit","energy_measure","emission_factor_type","emission_factor_origin","emission_reporting_unit","emission_factor_value","emission_measure",
                      "item_text","energy_consumption_sector_id","energy_consumption_sector",
                      "energy_consumption_carrier_id","energy_consumption_carrier","population_in_the_inventory_year",
                      "country_code","IEA_code","area")]

apply(dfEI_cl3, 2, function(x) any(is.na(x)))

## mei2bei
m2b = read.table("mei2bei.txt", header=TRUE)
dfEI_cl3$aux = paste(dfEI_cl3$action_plan_id,dfEI_cl3$inventory_year,"")
m2b$aux = paste(m2b$action_plan_id,m2b$inventory_year,"")
dfEI_cl3$bei_flag = as.character(dfEI_cl3$bei_flag)
dfEI_cl3$bei_flag = ifelse(dfEI_cl3$aux %in% m2b$aux, "TRUE", dfEI_cl3$bei_flag)
## bei2mei
b2m = read.table("bei2mei.txt", header=TRUE)
b2m = subset(b2m,!(b2m$action_plan_id %in% c(352,830,4115)))
b2m$aux = paste(b2m$action_plan_id,b2m$inventory_year,"")
b2m$aux2 = paste(b2m$action_plan_id,b2m$inventory_year.y,"")
dfEI_cl3$bei_flag = ifelse(dfEI_cl3$aux %in% b2m$aux, "FALSE", dfEI_cl3$bei_flag)
dfEI_cl3$bei_flag = ifelse(dfEI_cl3$aux %in% b2m$aux2, "TRUE", dfEI_cl3$bei_flag)
drop = c("aux")
dfEI_cl3 = dfEI_cl3[,!(names(dfEI_cl3) %in% drop)]
## Change the bei_flag to True if inventory_year=1990 except Birmingham
dfEI_cl3$bei_flag = ifelse((dfEI_cl3$bei_flag=="FALSE" & dfEI_cl3$inventory_year==1990 & dfEI_cl3$action_plan_id!=352),"TRUE",dfEI_cl3$bei_flag)

## Delete if no bei
beis = subset(dfEI_cl3,bei_flag=="TRUE")
meis = subset(dfEI_cl3,bei_flag!="TRUE")
meix = subset(meis,meis$action_plan_id %in% beis$action_plan_id)
# Drop Lessines (another action plan goes out with one emission inventory) 
dfEI_cl3 = rbind(beis,meix)
apply(dfEI_cl3, 2, function(x) any(is.na(x)))

dfEI_cl4 = dfEI_cl3 
dfEI_cl4$energy_consumption_macro_sector = ifelse(dfEI_cl4$energy_consumption_sector_id %in% c(3,8,12,13,14),as.character(dfEI_cl4$energy_consumption_sector),as.character(dfEI_cl4$item_text))

dfEI_cl5 = dfEI_cl4[c("organisation_id","organisation_name","action_plan_id",
                      "action_plan_submission_status_id","monitoring_report_id",
                      "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                      "inventory_year","activity_reporting_unit","energy_measure",
                      "emission_factor_type","emission_reporting_unit","emission_factor_value","emission_measure","energy_consumption_macro_sector","energy_consumption_sector",
                      "energy_consumption_sector_id","energy_consumption_carrier","energy_consumption_carrier_id","population_in_the_inventory_year","country_code","IEA_code","area")]

dfEI_cl5 = with(dfEI_cl5, dfEI_cl5[order(dfEI_cl5$organisation_id,
                                         dfEI_cl5$action_plan_id,
                                         dfEI_cl5$emission_inventory_id,
                                         decreasing = c(FALSE), method = "radix"),])

df_EIEF_AP = subset(dfEI_cl5,dfEI_cl5$emission_inventory_id %in% emisa$emission_inventory_id)
df_EIEF_MR = subset(dfEI_cl5,dfEI_cl5$emission_inventory_id %in% emisr$emission_inventory_id)

apply(df_EIEF_AP, 2, function(x) all(is.na(x)))
apply(df_EIEF_AP, 2, function(x) any(is.na(x)))
apply(df_EIEF_MR, 2, function(x) any(is.na(x)))

write.table(df_EIEF_AP, file = "df_EIEF_AP.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(df_EIEF_MR, file = "df_EIEF_MR.txt", sep = "\t", row.names = FALSE, col.names = TRUE)


## Part III. Energy supply
## Import the cleaned emissions
eAP = read.table("df_EIEF_AP.txt", header=TRUE)
eMR = read.table("df_EIEF_MR.txt", header=TRUE)
MC_v = rbind(eAP,eMR)
MC_v$emission_inventory_id = as.character(MC_v$emission_inventory_id)

## LDEPR: Local/distributed electricity production (renewable energy only)
ldepr_ap0 = read_excel("dfLDEPR_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
ldepr_mr0 = read_excel("dfLDEPR_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
ldepr = rbind(ldepr_ap0,ldepr_mr0)
ldepr = subset(ldepr,inventory_year<2023)

## Clean BEI flag according to declared consumption EIs
beif = unique(MC_v[c("emission_inventory_id","bei_flag")])
ldepr_b = left_join(ldepr,beif,by=("emission_inventory_id"))
ldepr_b$baseline_year_flag = ifelse(is.na(ldepr_b$bei_flag),ldepr_b$baseline_year_flag,ldepr_b$bei_flag)
drop = c("bei_flag")
ldepr = ldepr_b[,!(names(ldepr_b) %in% drop)]

## Clean inventories, keep only the ones we have in the consumption EIs
ldepr = subset(ldepr,(emission_inventory_id %in% MC_v$emission_inventory_id))

## Versions to work on
ldepr_ap = subset(ldepr, ldepr$emission_inventory_id %in% ldepr_ap0$emission_inventory_id)
ldepr_mr = subset(ldepr, ldepr$emission_inventory_id %in% ldepr_mr0$emission_inventory_id)

## LDEP: Local/distributed electricity production
ldep_ap0 = read_excel("dfLDEP_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
ldep_mr0 = read_excel("dfLDEP_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
ldep = rbind(ldep_ap0,ldep_mr0)
ldep = subset(ldep,inventory_year<2023)

## Clean BEI flag
ldep_b = left_join(ldep,beif,by=("emission_inventory_id"))
ldep_b$baseline_year_flag = ifelse(is.na(ldep_b$bei_flag),ldep_b$baseline_year_flag,ldep_b$bei_flag)
drop = c("bei_flag")
ldep = ldep_b[,!(names(ldep_b) %in% drop)]

## Clean inventories
ldep = subset(ldep,(emission_inventory_id %in% MC_v$emission_inventory_id))

## Versions to work on
ldep_ap = subset(ldep, ldep$emission_inventory_id %in% ldep_ap0$emission_inventory_id)
ldep_mr = subset(ldep, ldep$emission_inventory_id %in% ldep_mr0$emission_inventory_id)

## LHCP: Local heat/cold production
lhcp_ap0 = read_excel("dfLHCP_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
lhcp_mr0 = read_excel("dfLHCP_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",4),rep("numeric",2)))
lhcp = rbind(lhcp_ap0,lhcp_mr0)

lhcp = subset(lhcp,inventory_year<2023)

## Clean BEI flag
lhcp_b = left_join(lhcp,beif,by=("emission_inventory_id"))
lhcp_b$baseline_year_flag = ifelse(is.na(lhcp_b$bei_flag),lhcp_b$baseline_year_flag,lhcp_b$bei_flag)
drop = c("bei_flag")
lhcp = lhcp_b[,!(names(lhcp_b) %in% drop)]

## Clean inventories
lhcp = subset(lhcp,(emission_inventory_id %in% MC_v$emission_inventory_id))

## Versions to work on
lhcp_ap = subset(lhcp, lhcp$emission_inventory_id %in% lhcp_ap0$emission_inventory_id)
lhcp_mr = subset(lhcp, lhcp$emission_inventory_id %in% lhcp_mr0$emission_inventory_id)

## NERS: Non-energy related sectors
ners_ap0 = read_excel("dfNERS_AP.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",1),rep("numeric",2)))
ners_mr0 = read_excel("dfNERS_MR.xlsx", col_types = c(rep("text",8),rep("numeric",1),rep("text",1),rep("numeric",2)))
ners = rbind(ners_ap0,ners_mr0)
ners = subset(ners,inventory_year<2023)

## Clean BEI flag
ners_b = left_join(ners,beif,by=("emission_inventory_id"))
ners_b$baseline_year_flag = ifelse(is.na(ners_b$bei_flag),ners_b$baseline_year_flag,ners_b$bei_flag)
drop = c("bei_flag")
ners = ners_b[,!(names(ners_b) %in% drop)]

## Clean inventories
ners = subset(ners,(emission_inventory_id %in% MC_v$emission_inventory_id))

## Versions to work on
ners_ap = subset(ners, ners$emission_inventory_id %in% ners_ap0$emission_inventory_id)
ners_mr = subset(ners, ners$emission_inventory_id %in% ners_mr0$emission_inventory_id)

## CGE: Certified green electricity
cge_ap0 = read_excel("dfCGE_AP.xlsx", col_types = c(rep("text",3),rep("numeric",1),rep("text",2),rep("numeric",1)))
cge_mr0 = read_excel("dfCGE_MR.xlsx", col_types = c(rep("text",3),rep("numeric",1),rep("text",2),rep("numeric",1)))
cge_ap0$monitoring_report_id = NA
apex = MC_v[c("action_plan_id","emission_inventory_id")]
apex2 = unique(apex[c("action_plan_id","emission_inventory_id")])
cge_mr01 = left_join(cge_mr0,apex2,by=c("emission_inventory_id"))
cge = rbind(cge_ap0,cge_mr01)
cge = subset(cge,inventory_year<2023)

## Clean BEI flag
cge_b = left_join(cge,beif,by=("emission_inventory_id"))
cge_b$baseline_year_flag = ifelse(is.na(cge_b$bei_flag),cge_b$baseline_year_flag,cge_b$bei_flag)
drop = c("bei_flag")
cge = cge_b[,!(names(cge_b) %in% drop)]

## Clean inventories
cge = subset(cge,(emission_inventory_id %in% MC_v$emission_inventory_id))

## Versions to work on
cge_ap = subset(cge, cge$emission_inventory_id %in% cge_ap0$emission_inventory_id)
cge_mr = subset(cge, cge$emission_inventory_id %in% cge_mr0$emission_inventory_id)


## Cleaned DataSets
## Energy for heat and cold against District heating
lhcp_c = lhcp
## 2. Keep only if the inventory has district heating measure >0
dhs = subset(lhcp_c,lhcp_c$energy_supply_type_id==13)
dhsi = subset(dhs,dhs$energy_produced>0)
lhcp_cd = subset(lhcp_c,lhcp_c$emission_inventory_id %in% dhsi$emission_inventory_id)
in_conshd = unique(lhcp_cd[c("emission_inventory_id")])

## 3. Compare production and related emissions
ener_lhcp = subset(lhcp_cd,is.na(lhcp_cd$co2co2_eq_emissions))

## 3a. Delete if energy_production < 3
ener_lhcp2 = subset(ener_lhcp,ener_lhcp$energy_produced > 3) ## reference for energy production
## Aggregate by fossils and renewables to cross with emissions
inputs = subset(ener_lhcp2, !(ener_lhcp2$energy_carrier %in% c("Heat/cold renewable","Heat/cold non renewable")))
supply = subset(ener_lhcp2, (ener_lhcp2$energy_carrier %in% c("Heat/cold renewable","Heat/cold non renewable")))

supply$energy_carrier2 = ifelse(supply$energy_carrier %in% c("Heat/cold renewable"),"Renewable sources","Fossil sources")
supply4 = setDT(supply)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,organisation_name,emission_inventory_id,inventory_year,energy_carrier2,energy_supply_type), 
                        .SDcols=c("energy_produced")]

## Separate emissions
emis_lhcp = subset(lhcp_cd,!is.na(lhcp_cd$co2co2_eq_emissions))
emis_lhcp = emis_lhcp %>% rename( energy_carrier2 = energy_carrier)
emis_lhcps = setDT(emis_lhcp )[, lapply(.SD, sum, na.rm=TRUE), by=.(emission_inventory_id,energy_carrier2,energy_supply_type), 
                               .SDcols=c("co2co2_eq_emissions")]
## Join production and emissions only if there is production
emis_lhcp2 = left_join(supply4,emis_lhcps,by=c("emission_inventory_id","energy_carrier2","energy_supply_type")) 
emis_lhcp2 = subset(emis_lhcp2,!is.na(emis_lhcp2$co2co2_eq_emissions)) 
#min(emis_lhcp2$energy_produced)

## 3b. Delete if fossils production>0 and emissions==0
emis_lhcp3 = subset(emis_lhcp2,!(emis_lhcp2$energy_carrier2=="Fossil sources" & emis_lhcp2$co2co2_eq_emissions==0))

## 4. Emission-supply factors
emis_lhcp3$factor = emis_lhcp3$co2co2_eq_emissions/emis_lhcp3$energy_produced
emis_lhcp4 = subset(emis_lhcp3, emis_lhcp3$factor<2 & !(emis_lhcp3$energy_carrier2=="Fossil sources" & emis_lhcp3$factor<0.1))
emis_lhcp4  = emis_lhcp4[with(emis_lhcp4 , order(-emis_lhcp4$co2co2_eq_emissions)), ]

## Prepare for cleaned versions
emis_lhcp4$emicar = paste(emis_lhcp4$emission_inventory_id,emis_lhcp4$energy_carrier2,emis_lhcp4$energy_supply_type,"")

lhcp_aps = subset(lhcp_ap, (lhcp_ap$energy_carrier %in% c("Heat/cold renewable","Heat/cold non renewable")))
lhcp_aps$energy_carrier2 = ifelse(lhcp_aps$energy_carrier %in% c("Heat/cold renewable"),"Renewable sources","Fossil sources")

lhcp_mrs = subset(lhcp_mr, (lhcp_mr$energy_carrier %in% c("Heat/cold renewable","Heat/cold non renewable")))
lhcp_mrs$energy_carrier2 = ifelse(lhcp_mrs$energy_carrier %in% c("Heat/cold renewable"),"Renewable sources","Fossil sources")

lhcp_aps$emicar = paste(lhcp_aps$emission_inventory_id,lhcp_aps$energy_carrier2,lhcp_aps$energy_supply_type,"")
lhcp_mrs$emicar = paste(lhcp_mrs$emission_inventory_id,lhcp_mrs$energy_carrier2,lhcp_mrs$energy_supply_type,"")
lhcp_aps$typex = paste(lhcp_aps$emission_inventory_id,lhcp_aps$energy_supply_type,"")
lhcp_mrs$typex = paste(lhcp_mrs$emission_inventory_id,lhcp_mrs$energy_supply_type,"")

## Production only
lhcp_ener_AP = subset(lhcp_aps, lhcp_aps$emicar %in% emis_lhcp4$emicar & !is.na(lhcp_aps$energy_produced))
lhcp_ener_MR = subset(lhcp_mrs, lhcp_mrs$emicar %in% emis_lhcp4$emicar & !is.na(lhcp_mrs$energy_produced))

inputs$typex = paste(inputs$emission_inventory_id,inputs$energy_supply_type,"")
inputs_AP = subset(inputs,inputs$typex %in% lhcp_ener_AP$typex)
inputs_MR = subset(inputs,inputs$typex %in% lhcp_ener_MR$typex)

apply(lhcp_ener_AP, 2, function(x) any(is.na(x)))
apply(lhcp_ener_AP, 2, function(x) all(is.na(x)))

lhcp_ener_AP = lhcp_ener_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
lhcp_ener_MR = lhcp_ener_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
inputs_AP = inputs_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                         "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
inputs_MR = inputs_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                         "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]

lhcp_ener_AP2 = rbind(lhcp_ener_AP,inputs_AP)
lhcp_ener_MR2 = rbind(lhcp_ener_MR,inputs_MR)

lhcp_ener_AP2 = lhcp_ener_AP2[with(lhcp_ener_AP2, order(lhcp_ener_AP2$organisation_id,
                                                        lhcp_ener_AP2$action_plan_id,
                                                        lhcp_ener_AP2$emission_inventory_id,
                                                        lhcp_ener_AP2$energy_supply_type,
                                                        lhcp_ener_AP2$energy_carrier)), ]
lhcp_ener_MR2 = lhcp_ener_MR2[with(lhcp_ener_MR2, order(lhcp_ener_MR2$organisation_id,
                                                        lhcp_ener_MR2$action_plan_id,
                                                        lhcp_ener_MR2$emission_inventory_id,
                                                        lhcp_ener_MR2$energy_supply_type,
                                                        lhcp_ener_MR2$energy_carrier)), ]

write.table(lhcp_ener_AP2, file = "lhcp_ener_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(lhcp_ener_MR2, file = "lhcp_ener_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Emissions only
lhcp_apx = lhcp_ap
lhcp_apx$emicar = paste(lhcp_apx$emission_inventory_id,lhcp_apx$energy_carrier,lhcp_apx$energy_supply_type,"")
lhcp_emis_AP = subset(lhcp_apx, lhcp_apx$emicar %in% emis_lhcp4$emicar & !is.na(lhcp_apx$co2co2_eq_emissions) )

lhcp_mrx = lhcp_mr
lhcp_mrx$emicar = paste(lhcp_mrx$emission_inventory_id,lhcp_mrx$energy_carrier,lhcp_mrx$energy_supply_type,"")

lhcp_emis_MR = subset(lhcp_mrx, lhcp_mrx$emicar %in% emis_lhcp4$emicar & !is.na(lhcp_mrx$co2co2_eq_emissions) )

apply(lhcp_emis_AP, 2, function(x) any(is.na(x)))
apply(lhcp_emis_AP, 2, function(x) all(is.na(x)))

lhcp_emis_AP = lhcp_emis_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","co2co2_eq_emissions")]
lhcp_emis_MR = lhcp_emis_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","co2co2_eq_emissions")]

write.table(lhcp_emis_AP, file = "lhcp_emis_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(lhcp_emis_MR, file = "lhcp_emis_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## check
uen_ap = unique(lhcp_ener_AP[c("emission_inventory_id")])
uem_ap = unique(lhcp_emis_AP[c("emission_inventory_id")])
uen_mr = unique(lhcp_ener_MR[c("emission_inventory_id")])
uem_mr = unique(lhcp_emis_MR[c("emission_inventory_id")])

## LDEP: Local/distributed electricity production
## 1. Only if present in Consumption
ldep_c = ldep

## 2. Compare energy produced and emissions
## Only energy produced
ener_ldep0 = subset(ldep_c,is.na(ldep_c$co2co2_eq_emissions))

## 3a. Delete if production <3
ener_ldep = subset(ener_ldep0,ener_ldep0$energy_produced>3)

## Aggregate by fossils and renewables to cross with emissions
inputs = subset(ener_ldep, !(ener_ldep$energy_carrier %in% c("Electricity renewable","Electricity non renewable")))
supply = subset(ener_ldep, (ener_ldep$energy_carrier %in% c("Electricity renewable","Electricity non renewable")))

supply$energy_carrier2 = ifelse(supply$energy_carrier %in% c("Electricity renewable"),"Renewable sources","Fossil sources")

supply4 = setDT(supply)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,organisation_name,emission_inventory_id,inventory_year,energy_carrier2,energy_supply_type), 
                        .SDcols=c("energy_produced")]

## Separate emissions
emis_ldep = subset(ldep_c,!is.na(ldep_c$co2co2_eq_emissions))
emis_ldep = emis_ldep %>% rename( energy_carrier2 = energy_carrier)
emis_ldeps = setDT(emis_ldep )[, lapply(.SD, sum, na.rm=TRUE), by=.(emission_inventory_id,energy_carrier2,energy_supply_type), 
                               .SDcols=c("co2co2_eq_emissions")]
## Join production and emissions only if there is production
emis_ldep2 = left_join(supply4,emis_ldeps,by=c("emission_inventory_id","energy_carrier2","energy_supply_type")) 
emis_ldep2 = subset(emis_ldep2,!is.na(emis_ldep2$co2co2_eq_emissions)) 

## 3b. Delete if fossils production>0 and emissions==0
emis_ldep3 = subset(emis_ldep2,!(emis_ldep2$energy_carrier2=="Fossil sources" & emis_ldep2$co2co2_eq_emissions==0))

## 4. Emission-supply factors
emis_ldep3$factor = emis_ldep3$co2co2_eq_emissions/emis_ldep3$energy_produced
emis_ldep4 = subset(emis_ldep3, emis_ldep3$factor<2 & !(emis_ldep3$energy_carrier2=="Fossil sources" & emis_ldep3$factor<0.1))
emis_ldep4 = emis_ldep4[with(emis_ldep4, order(-emis_ldep4$co2co2_eq_emissions)), ]

## Prepare for cleaned versions
emis_ldep4$emicar = paste(emis_ldep4$emission_inventory_id,emis_ldep4$energy_carrier2,emis_ldep4$energy_supply_type,"")

ldep_aps = subset(ldep_ap, (ldep_ap$energy_carrier %in% c("Electricity renewable","Electricity non renewable")))
ldep_aps$energy_carrier2 = ifelse(ldep_aps$energy_carrier %in% c("Electricity renewable"),"Renewable sources","Fossil sources")

ldep_mrs = subset(ldep_mr, (ldep_mr$energy_carrier %in% c("Electricity renewable","Electricity non renewable")))
ldep_mrs$energy_carrier2 = ifelse(ldep_mrs$energy_carrier %in% c("Electricity renewable"),"Renewable sources","Fossil sources")

ldep_aps$emicar = paste(ldep_aps$emission_inventory_id,ldep_aps$energy_carrier2,ldep_aps$energy_supply_type,"")
ldep_mrs$emicar = paste(ldep_mrs$emission_inventory_id,ldep_mrs$energy_carrier2,ldep_mrs$energy_supply_type,"")
ldep_aps$typex = paste(ldep_aps$emission_inventory_id,ldep_aps$energy_supply_type,"")
ldep_mrs$typex = paste(ldep_mrs$emission_inventory_id,ldep_mrs$energy_supply_type,"")

## Production only
ldep_ener_AP = subset(ldep_aps, ldep_aps$emicar %in% emis_ldep4$emicar & !is.na(ldep_aps$energy_produced) & !is.na(ldep_aps$energy_carrier_id))
ldep_ener_MR = subset(ldep_mrs, ldep_mrs$emicar %in% emis_ldep4$emicar & !is.na(ldep_mrs$energy_produced) & !is.na(ldep_mrs$energy_carrier_id))

inputs$typex = paste(inputs$emission_inventory_id,inputs$energy_supply_type,"")
inputs_AP = subset(inputs,inputs$typex %in% ldep_ener_AP$typex)
inputs_MR = subset(inputs,inputs$typex %in% ldep_ener_MR$typex)

apply(ldep_ener_AP, 2, function(x) any(is.na(x)))
apply(ldep_ener_AP, 2, function(x) all(is.na(x)))

ldep_ener_AP = ldep_ener_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
ldep_ener_MR = ldep_ener_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]

inputs_AP = inputs_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                         "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
inputs_MR = inputs_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                         "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]

ldep_ener_AP2 = rbind(ldep_ener_AP,inputs_AP)
ldep_ener_MR2 = rbind(ldep_ener_MR,inputs_MR)

ldep_ener_AP2 = ldep_ener_AP2[with(ldep_ener_AP2, order(ldep_ener_AP2$organisation_id,
                                                        ldep_ener_AP2$action_plan_id,
                                                        ldep_ener_AP2$emission_inventory_id,
                                                        ldep_ener_AP2$energy_supply_type,
                                                        ldep_ener_AP2$energy_carrier)), ]
ldep_ener_MR2 = ldep_ener_MR2[with(ldep_ener_MR2, order(ldep_ener_MR2$organisation_id,
                                                        ldep_ener_MR2$action_plan_id,
                                                        ldep_ener_MR2$emission_inventory_id,
                                                        ldep_ener_MR2$energy_supply_type,
                                                        ldep_ener_MR2$energy_carrier)), ]


write.table(ldep_ener_AP2, file = "ldep_ener_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(ldep_ener_MR2, file = "ldep_ener_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Emissions only
ldep_apx = ldep_ap
ldep_apx$emicar = paste(ldep_apx$emission_inventory_id,ldep_apx$energy_carrier,ldep_apx$energy_supply_type,"")
ldep_emis_AP = subset(ldep_apx, ldep_apx$emicar %in% emis_ldep4$emicar & !is.na(ldep_apx$co2co2_eq_emissions) & !is.na(ldep_apx$energy_carrier_id))

ldep_mrx = ldep_mr
ldep_mrx$emicar = paste(ldep_mrx$emission_inventory_id,ldep_mrx$energy_carrier,ldep_mrx$energy_supply_type,"")
ldep_emis_MR = subset(ldep_mrx, ldep_mrx$emicar %in% emis_ldep4$emicar & !is.na(ldep_mrx$co2co2_eq_emissions) & !is.na(ldep_mrx$energy_carrier_id))

apply(ldep_emis_AP, 2, function(x) any(is.na(x)))
apply(ldep_emis_AP, 2, function(x) all(is.na(x)))

ldep_emis_AP = ldep_emis_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","co2co2_eq_emissions")]
ldep_emis_MR = ldep_emis_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                               "energy_supply_type","energy_carrier_id","energy_carrier","co2co2_eq_emissions")]

write.table(ldep_emis_AP, file = "ldep_emis_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(ldep_emis_MR, file = "ldep_emis_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


## LDEPR: Local/distributed electricity production (renewable energy only)
## 1. Only if present in Consumption
ldepr_c = ldepr
## 2. Keep only if the inventory has energy_produced >3
ldepr_cd = subset(ldepr_c,ldepr_c$energy_produced>3)
in_conshd = unique(ldepr_cd[c("emission_inventory_id")])

## 3. Compare with electricity consumption in MyCovenant (check production!>>consumption)
es = setDT(ldepr_cd)[, lapply(.SD, sum, na.rm=TRUE), by=.(emission_inventory_id), 
                     .SDcols=c("energy_produced")]
elec_c = subset(MC_v,MC_v$energy_consumption_carrier_id==1)
mces = setDT(elec_c)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,organisation_name,emission_inventory_id,inventory_year,emission_factor_type), 
                     .SDcols=c("energy_measure")]
ea0 = left_join(mces,es,by=("emission_inventory_id"))
ea = subset(ea0,!is.na(ea0$energy_produced))

## Production Outliers in MyCovenant
ea$diffs_h = (ea$energy_produced-ea$energy_measure)/ea$energy_measure
ea = ea[with(ea, order(-ea$diffs_h)), ]
summary(ea)

out = subset(ea,ea$diffs_h>150)

## Production only
ldepr_ener_AP = subset(ldepr_ap, ldepr_ap$emission_inventory_id %in% ldepr_cd$emission_inventory_id & !(ldepr_ap$emission_inventory_id %in% out$emission_inventory_id) & !is.na(ldepr_ap$inventory_year))
ldepr_ener_MR = subset(ldepr_mr, ldepr_mr$emission_inventory_id %in% ldepr_cd$emission_inventory_id & !(ldepr_mr$emission_inventory_id %in% out$emission_inventory_id) & !is.na(ldepr_mr$inventory_year))

apply(ldepr_ener_AP, 2, function(x) any(is.na(x)))
apply(ldepr_ener_AP, 2, function(x) all(is.na(x)))

ldepr_ener_AP = ldepr_ener_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                                 "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]
ldepr_ener_MR = ldepr_ener_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status","monitoring_report_id","monitoring_report_submission_status","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type_id",
                                 "energy_supply_type","energy_carrier_id","energy_carrier","energy_produced")]

write.table(ldepr_ener_AP, file = "ldepr_ener_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(ldepr_ener_MR, file = "ldepr_ener_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


##  CGE: Certified green electricity only
## 1. Only if present in Consumption
cge_c = cge
## 2. Keep only if the inventory has positive purchases>3
cge_cd = subset(cge_c,cge_c$energy_produced>3)

cge_p = subset(cge_cd,cge_cd$energy_supply_type=="Purchases Guarantees of Origins (within the municipality boundaries)")
cge_s = subset(cge_cd,cge_cd$energy_supply_type=="Sales Guarantees of Origins (within the municipality boundaries)")

## 3. Compare purchases with consumption
cget = left_join(cge_p, mces, by=("emission_inventory_id"))
cget$diffs_h = (cget$energy_produced-cget$energy_measure)/cget$energy_measure
summary(cget)
cget = cget[with(cget, order(-cget$diffs_h)), ]
p1 = subset(cget,cget$diffs_h<=0.05)
po = subset(cget,cget$diffs_h>0.05)
pout = unique(po[c("emission_inventory_id")])

## Cleaned CGE
cge_AP = subset(cge_ap, cge_ap$emission_inventory_id %in% p1$emission_inventory_id) 
cge_MR = subset(cge_mr, cge_mr$emission_inventory_id %in% p1$emission_inventory_id) 

apply(cge_AP, 2, function(x) any(is.na(x)))
apply(cge_AP, 2, function(x) all(is.na(x)))

cge_AP = cge_AP[,c("action_plan_id","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type",
                   "energy_carrier","energy_produced")]
cge_MR = cge_MR[,c("monitoring_report_id","emission_inventory_id","baseline_year_flag","inventory_year","energy_supply_type",
                   "energy_carrier","energy_produced")]

write.table(cge_AP, file = "cge_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(cge_MR, file = "cge_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


# NERS: Non-energy related sectors and EDGAR
## 1. Only if present in Consumption
ners_c = ners
ners_a = subset(ners_c,!is.na(ners_c$co2_eq_emission_t))
ners_a2 = subset(ners_a,!is.na(ners_a$non_energy_related_sectors))

## 2. Keep only if the inventory has positive emissions > 0
ners_a3 = subset(ners_a2,ners_a2$co2_eq_emission_t>0)

## 3. Check emissions in total and per capita
ners_t = setDT(ners_a3)[, lapply(.SD, sum, na.rm=TRUE), by=.(organisation_id,organisation_name,action_plan_id,emission_inventory_id),#,non_energy_related_sectors), only Other non-energy related such as fuggitive emissions, Waste management, Wastewater treatment and discharge
                        .SDcols=c("co2_eq_emission_t")]

pop = unique(MC_v[,c("emission_inventory_id","population_in_the_inventory_year")])
plh2 = left_join(ners_t,pop,by=c("emission_inventory_id"))
plh2$co2_eq_emission_t_pc = plh2$co2_eq_emission_t/plh2$population_in_the_inventory_year
plh2 = plh2[with(plh2, order(-plh2$co2_eq_emission_t_pc)), ]

## Cleaned NERS
ners_AP = subset(ners_ap, ners_ap$emission_inventory_id %in% plh2$emission_inventory_id & !is.na(ners_ap$non_energy_related_sectors ) & ners_ap$co2_eq_emission_t>0) 
ners_MR = subset(ners_mr, ners_mr$emission_inventory_id %in% plh2$emission_inventory_id & !is.na(ners_mr$non_energy_related_sectors ) & ners_mr$co2_eq_emission_t>0) 

apply(ners_AP, 2, function(x) any(is.na(x)))
apply(ners_AP, 2, function(x) all(is.na(x)))
apply(ners_MR, 2, function(x) any(is.na(x)))

## Re-organization
ners_AP = subset(ners_AP,(non_energy_related_sectors != "Waste management"))
ners_MR = subset(ners_MR,(non_energy_related_sectors != "Waste management"))

ners_AP = ners_AP[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status_id","emission_inventory_id","baseline_year_flag","inventory_year","non_energy_related_sectors",
                     "co2_eq_emission_t")]
ners_MR = ners_MR[,c("organisation_id","organisation_name","action_plan_id","action_plan_submission_status_id","monitoring_report_id","monitoring_report_submission_status_id","emission_inventory_id","baseline_year_flag","inventory_year","non_energy_related_sectors",
                     "co2_eq_emission_t")]

write.table(ners_AP, file = "ners_AP.txt", sep = "\t",row.names = FALSE, col.names = TRUE)
write.table(ners_MR, file = "ners_MR.txt", sep = "\t",row.names = FALSE, col.names = TRUE)


##Part IV. Policies
## DF2A
coms = read_excel("df2A_1.xlsx", col_types = c("text",rep("numeric",9),rep("text",8),rep("numeric",8)))
actions = read_excel("dfMAOclean.xlsx", col_types = c(rep("text",3),rep("numeric",12)))
actions = subset(actions,!is.na(action_sector))

## manual changes
ff=subset(coms,action_plan_id %in% c(899,1312,1315,912,66,1468,8336))
coms$base_year_CoM2030 = ifelse(coms$action_plan_id %in% c(1315),2009, coms$base_year_CoM2030)
coms$base_year_CoM2030 = ifelse(coms$action_plan_id %in% c(912,66),2005, coms$base_year_CoM2030)

## Import the cleaned emissions
eAP = read.table("df_EIEF_AP.txt", header=TRUE)
eMR = read.table("df_EIEF_MR.txt", header=TRUE)
clems2 = rbind(eAP,eMR)
clems2$action_plan_id = as.character(clems2$action_plan_id)

## Get BEIs
coms20 = subset(coms,!is.na(base_year_CoM2020))
coms20$aux = paste(coms20$action_plan_id,coms20$base_year_CoM2020,"")
coms20$base_year = coms20$base_year_CoM2020
coms30 = subset(coms,!is.na(base_year_CoM2030))
coms30$aux = paste(coms30$action_plan_id,coms30$base_year_CoM2030,"")
coms30$base_year = coms30$base_year_CoM2030
coms50 = subset(coms,!is.na(`base_year_Long-term_climate_neutrality_target(CoM2050)`))
coms50$aux = paste(coms50$action_plan_id,coms50$`base_year_Long-term_climate_neutrality_target(CoM2050)`,"")
coms50$base_year = coms50$`base_year_Long-term_climate_neutrality_target(CoM2050)`
comsm = subset(coms,!is.na(`base_year_Mid-term target`))
comsm$aux = paste(comsm$action_plan_id,comsm$`base_year_Mid-term target`,"")
comsm$base_year = comsm$`base_year_Mid-term target`
beis = rbind(coms20,coms30,coms50,comsm)
xx = subset(beis,is.na(base_year))
beis_0 = subset(beis,base_year>=1990 & base_year<2023 & action_plan_id %in% clems2$action_plan_id)

## MyCov emissions by plan, where Bei
clems2$base_year = clems2$inventory_year
emissions = setDT(clems2)[, lapply(.SD, sum), by=.(organisation_name,action_plan_id,emission_inventory_id,base_year,bei_flag,population_in_the_inventory_year), 
                          .SDcols=c("emission_measure","energy_measure")]
xx = emissions[duplicated(emissions[,c("action_plan_id","base_year")])|duplicated(emissions[,c("action_plan_id","base_year")], fromLast=TRUE),]
xx = xx[with(xx, order(xx$action_plan_id,xx$base_year,-xx$bei_flag)), ]
xx2 = setDT(xx)[, lapply(.SD, first), by=.(action_plan_id,base_year),.SDcols=c("emission_inventory_id")]
xx_out = subset(xx,!(emission_inventory_id %in% xx2$emission_inventory_id))
emissions2 = subset(emissions,!(emission_inventory_id %in% xx_out$emission_inventory_id))
beis2 = left_join(beis_0,emissions2,by=c("action_plan_id","base_year"))
zz = subset(beis2,is.na(energy_measure))

## missing bei
ids = unique(zz[,c("action_plan_id","base_year")])
n= dim(ids)[1]
for(i in 1:n){
  ap = ids$action_plan_id[i]
  allaps = subset(emissions2,action_plan_id==ap)
  allaps$yearx = abs(allaps$base_year - ids$base_year[i])
  allaps = allaps[with(allaps, order(allaps$yearx)), ]
  emis3 = allaps[1,]
  if(i==1){emis4=emis3}else{emis4=rbind(emis4,emis3)}
}

## get all BEIs as reported in the actions 
emissions2$aux = paste(emissions2$action_plan_id,emissions2$base_year,"")
beis3 = subset(beis2, !is.na(beis2$energy_measure))
beis3$aux= paste(beis3$action_plan_id,beis3$base_year,"")
emis4$aux = paste(emis4$action_plan_id,emis4$base_year,"")
emissions = subset(emissions2,aux %in% beis3$aux | aux %in% emis4$aux)

## CO2 reduction aggregate by action plan
study2020 = subset(actions,!is.na(actions$estimates_co2_reduction_2020) & actions$estimates_co2_reduction_2020>10)
study2030 = subset(actions,!is.na(actions$estimates_co2_reduction_2030) & actions$estimates_co2_reduction_2030>10)
studymid = subset(actions,!is.na(actions$estimates_co2_reduction_longterm) & actions$estimates_co2_reduction_longterm>10)
study2050 = subset(actions,!is.na(actions$estimates_co2_reduction_2050) & actions$estimates_co2_reduction_2050>10)

## Estimates by plan
actions20 = setDT(study2020)[, lapply(.SD, sum), by=.(action_plan_id), 
                             .SDcols=c("estimates_co2_reduction_2020","estimates_energy_savings_2020","estimates_energy_production_2020")]
actions30 = setDT(study2030)[, lapply(.SD, sum), by=.(action_plan_id), 
                             .SDcols=c("estimates_co2_reduction_2030","estimates_energy_savings_2030","estimates_energy_production_2030")]
actionsmi = setDT(studymid)[, lapply(.SD, sum), by=.(action_plan_id),.SDcols=c("estimates_co2_reduction_longterm","estimates_energy_savings_longterm","estimates_energy_production_longterm")]
actions50 = setDT(study2050)[, lapply(.SD, sum), by=.(action_plan_id), 
                             .SDcols=c("estimates_co2_reduction_2050","estimates_energy_savings_2050","estimates_energy_production_2050")]

## targets
years20 = coms20[c("action_plan_id","co2_target_CoM2020","base_year_CoM2020","reduction_type_CoM2020","bau_flag_CoM2020","population_estimate_in_target_year_CoM2020")]
years20 = years20 %>% rename (base_year=base_year_CoM2020,co2_target=co2_target_CoM2020,reduction=reduction_type_CoM2020,population=population_estimate_in_target_year_CoM2020)
years30 = coms30[c("action_plan_id","co2_target_CoM2030","base_year_CoM2030","reduction_type_CoM2030","bau_flag_CoM2030","population_estimate_in_target_year_CoM2030")]
years30 = years30 %>% rename (base_year=base_year_CoM2030,co2_target=co2_target_CoM2030,reduction=reduction_type_CoM2030,population=population_estimate_in_target_year_CoM2030)
yearsmi = comsm[c("action_plan_id","co2_target_Mid-term target","base_year_Mid-term target","target_year_Mid-term target","reduction_type_Mid-term target","bau_flag_Mid-term target","population_estimate_in_target_year_Mid-term target")]
yearsmi = yearsmi %>% rename (base_year=`base_year_Mid-term target`,co2_target=`co2_target_Mid-term target`,reduction=`reduction_type_Mid-term target`,population=`population_estimate_in_target_year_Mid-term target`)
years50 = coms50[c("action_plan_id","co2_target_Long-term_climate_neutrality_target(CoM2050)","base_year_Long-term_climate_neutrality_target(CoM2050)","reduction_type_Long-term_climate_neutrality_target(CoM2050)","bau_flag_Long-term_climate_neutrality_target(CoM2050)","population_estimate_in_target_year_Long-term_climate_neutrality_target(CoM2050)")]
years50 = years50 %>% rename (base_year=`base_year_Long-term_climate_neutrality_target(CoM2050)`,co2_target=`co2_target_Long-term_climate_neutrality_target(CoM2050)`,reduction=`reduction_type_Long-term_climate_neutrality_target(CoM2050)`,population=`population_estimate_in_target_year_Long-term_climate_neutrality_target(CoM2050)`)

as20 = left_join(actions20,years20,by=c("action_plan_id"))
as30 = left_join(actions30,years30,by=c("action_plan_id"))
asmi = left_join(actionsmi,yearsmi,by=c("action_plan_id"))
as50 = left_join(actions50,years50,by=c("action_plan_id"))

## missing targets
nas_2020 = subset(as20,is.na(co2_target) | co2_target==0)
nas20 = subset(as20,!is.na(co2_target) & co2_target>0 & co2_target<=1)
nas_2030 = subset(as30,is.na(co2_target) | co2_target==0)
nas30 = subset(as30,!is.na(co2_target) & co2_target>0 & co2_target<=1)
nas_mi = subset(asmi, is.na(co2_target) | co2_target==0)
nasmi = subset(asmi, !is.na(co2_target) & co2_target>0 & co2_target<=1)
nas_2050 = subset(as50,is.na(co2_target) | co2_target==0)
nas50 = subset(as50,!is.na(co2_target) & co2_target>0 & co2_target<=1)

## need to cross it with the base year and inventory year
s20 = left_join(nas20,emissions,by=c("action_plan_id","base_year"))
s20 = subset(s20,!is.na(s20$emission_measure))
s30 = left_join(nas30,emissions,by=c("action_plan_id","base_year"))
s30 = subset(s30,!is.na(s30$emission_measure))
smid = left_join(nasmi,emissions,by=c("action_plan_id","base_year"))
smid = subset(smid,!is.na(smid$emission_measure))
s50 = left_join(nas50,emissions,by=c("action_plan_id","base_year"))
s50 = subset(s50,!is.na(s50$emission_measure))

## verified emissions for the targets
s20$real = ifelse(s20$reduction=="per_capita",(s20$co2_target*(s20$emission_measure/s20$population))*s20$population,s20$co2_target*s20$emission_measure)
s30$real = ifelse(s30$reduction=="per_capita",(s30$co2_target*(s30$emission_measure/s30$population))*s30$population,s30$co2_target*s30$emission_measure)
smid$real = ifelse(smid$reduction=="per_capita",(smid$co2_target*(smid$emission_measure/smid$population))*smid$population,smid$co2_target*smid$emission_measure)
s50$real = ifelse(s50$reduction=="per_capita",(s50$co2_target*(s50$emission_measure/s50$population))*s50$population,s50$co2_target*s50$emission_measure)

ns20=subset(s20,is.na(s20$real))
ns30=subset(s30,is.na(s30$real))
nsmi=subset(smid,is.na(smid$real))
ns50=subset(s50,is.na(s50$real))

## differences
s20$difp = (s20$estimates_co2_reduction_2020-s20$real)/s20$real
s30$difp = (s30$estimates_co2_reduction_2030-s30$real)/s30$real
smid$difp = (smid$estimates_co2_reduction_longterm-smid$real)/smid$real
s50$difp = (s50$estimates_co2_reduction_2050-s50$real)/s50$real

as_20 = subset(s20,s20$difp<=1.2 & s20$estimates_co2_reduction_2020 < s20$emission_measure)
as_20 = as_20[with(as_20, order(as_20$estimates_co2_reduction_2020)), ]
as_30 = subset(s30,s30$difp<=1.2 & s30$estimates_co2_reduction_2030 < s30$emission_measure)
as_30 = as_30[with(as_30, order(as_30$estimates_co2_reduction_2030)), ]
as_mi = subset(smid,smid$difp<=1.2 & smid$estimates_co2_reduction_longterm < smid$emission_measure)
as_mi = as_mi[with(as_mi, order(as_mi$estimates_co2_reduction_longterm)), ]
as_50 = subset(s50,s50$difp<=1.2 & s50$estimates_co2_reduction_2050 < s50$emission_measure)
as_50 = as_50[with(as_50, order(as_50$estimates_co2_reduction_2050)), ]

## Estimated savings
## total consumption difference with savings 
as_20$difs = (as_20$estimates_energy_savings_2020-as_20$energy_measure)/as_20$energy_measure
as_30$difs = (as_30$estimates_energy_savings_2030-as_30$energy_measure)/as_30$energy_measure
as_mi$difs = (as_mi$estimates_energy_savings_longterm-as_mi$energy_measure)/as_mi$energy_measure
as_50$difs = (as_50$estimates_energy_savings_2050-as_50$energy_measure)/as_50$energy_measure

xa20 = subset(as_20,as_20$difs<=0.2)
xa20 = xa20[with(xa20, order(-xa20$difs)), ]
xa20$aux = paste(xa20$action_plan_id,xa20$base_year,"")
xa30 = subset(as_30,as_30$difs<=0.2)
xa30 = xa30[with(xa30, order(-xa30$difs)), ]
xa30$aux = paste(xa30$action_plan_id,xa30$base_year,"")
xami = subset(as_mi,as_mi$difs<=0.2)
xami = xami[with(xami, order(-xami$difs)), ]
xami$aux = paste(xami$action_plan_id,xami$base_year,"")
xa50 = subset(as_50,as_50$difs<=0.2)
xa50 = xa50[with(xa50, order(-xa50$difs)), ]
xa50$aux = paste(xa50$action_plan_id,xa50$base_year,"")

## Select the inventories that make sense in an aggregated level
xas = rbind(xa20[,1],xa30[,1],xami[,1],xa50[,1])

actions2 = subset(actions,actions$action_plan_id %in% xas$action_plan_id & !is.na(actions$action_sector))

## Integrate everything into three columns to simplify the analysis
actions220 = left_join(actions2,years20,by=c("action_plan_id"))
actions220 = subset(actions220, !is.na(estimates_co2_reduction_2020) & !is.na(co2_target)  & co2_target>0 & co2_target<=1)
actions220$commitment = "2020"
actions220$target_year = 2020
actions220 = actions220[c("action_plan_id","commitment","co2_target","base_year","target_year","reduction","bau_flag_CoM2020","population","action_sector","mitigation_action_number","estimates_co2_reduction_2020","estimates_energy_savings_2020","estimates_energy_production_2020")]
actions220 = actions220 %>% rename( estimates_co2_reduction = estimates_co2_reduction_2020, estimates_energy_savings = estimates_energy_savings_2020, estimates_energy_production = estimates_energy_production_2020, population_estimate_in_target_year = population, reduction_type_id = reduction, bau_flag = bau_flag_CoM2020)

actions230 = left_join(actions2,years30,by=c("action_plan_id"))
actions230 = subset(actions230, !is.na(estimates_co2_reduction_2030) & !is.na(co2_target)  & co2_target>0 & co2_target<=1)
actions230$commitment = "2030"
actions230$target_year = 2030
actions230 = actions230[c("action_plan_id","commitment","co2_target","base_year","target_year","reduction","bau_flag_CoM2030","population","action_sector","mitigation_action_number","estimates_co2_reduction_2030","estimates_energy_savings_2030","estimates_energy_production_2030")]
actions230 = actions230 %>% rename( estimates_co2_reduction = estimates_co2_reduction_2030, estimates_energy_savings = estimates_energy_savings_2030, estimates_energy_production = estimates_energy_production_2030, population_estimate_in_target_year = population, reduction_type_id = reduction, bau_flag = bau_flag_CoM2030)

actions2mi = left_join(actions2,yearsmi,by=c("action_plan_id"))
actions2mi = subset(actions2mi, !is.na(estimates_co2_reduction_longterm) & !is.na(co2_target)  & co2_target>0 & co2_target<=1)
actions2mi$commitment = "Mid_term"
actions2mi$target_year = actions2mi$`target_year_Mid-term target`
actions2mi = actions2mi[c("action_plan_id","commitment","co2_target","base_year","target_year","reduction","bau_flag_Mid-term target","population","action_sector","mitigation_action_number","estimates_co2_reduction_longterm","estimates_energy_savings_longterm","estimates_energy_production_longterm")]
actions2mi = actions2mi %>% rename( estimates_co2_reduction = estimates_co2_reduction_longterm, estimates_energy_savings = estimates_energy_savings_longterm, estimates_energy_production = estimates_energy_production_longterm, population_estimate_in_target_year = population, reduction_type_id = reduction, bau_flag=`bau_flag_Mid-term target`)

actions250 = left_join(actions2,years50,by=c("action_plan_id"))
actions250 = subset(actions250, !is.na(estimates_co2_reduction_2050) & !is.na(co2_target) & co2_target>0 & co2_target<=1)
actions250$commitment = "2050"
actions250$target_year = 2050
actions250 = actions250[c("action_plan_id","commitment","co2_target","base_year","target_year","reduction","bau_flag_Long-term_climate_neutrality_target(CoM2050)","population","action_sector","mitigation_action_number","estimates_co2_reduction_2050","estimates_energy_savings_2050","estimates_energy_production_2050")]
actions250 = actions250 %>% rename( estimates_co2_reduction = estimates_co2_reduction_2050, estimates_energy_savings = estimates_energy_savings_2050, estimates_energy_production = estimates_energy_production_2050, population_estimate_in_target_year = population, reduction_type_id = reduction, bau_flag = `bau_flag_Long-term_climate_neutrality_target(CoM2050)`)

actions3 = rbind(actions220,actions230,actions2mi,actions250)

actionsF = subset(actions3,estimates_energy_production>=0 & estimates_energy_savings>=0 & estimates_co2_reduction>10 )
actionsF = subset(actionsF,!(estimates_energy_production==0 & estimates_energy_savings==0  ))
actionsF$action_sector = ifelse(actionsF$action_sector=="waste","other",actionsF$action_sector)

## Coherence of CO2 reduction estimates by action sectors
unique(actionsF[,c("action_sector")])
actions4 = subset(actionsF,actionsF$action_sector!="other") # No implicit factors for "other"

study = setDT(actions4)[, lapply(.SD, sum, na.rm=TRUE), by=.(action_plan_id,action_sector), 
                        .SDcols=c("estimates_energy_production","estimates_energy_savings","estimates_co2_reduction")]

study$ps = study$estimates_energy_savings + study$estimates_energy_production

study = subset(study, study$ps>0)
study$ifactor = study$estimates_co2_reduction/study$ps
oifs = subset(study,study$ifactor>2)
oifs = oifs[with(oifs, order(-oifs$ifactor)), ]
lifs = subset(study,study$ifactor<0.01)
lifs = lifs[with(lifs, order(lifs$ifactor)), ]
outs = rbind(oifs,lifs)

outs$asec = paste(outs$action_plan_id,outs$action_sector,"")
actionsF$asec = paste(actionsF$action_plan_id,actionsF$action_sector,"")

actions_F1 = subset(actionsF, !(actionsF$asec %in% outs$asec))
actions_F1 = subset(actions_F1, select=-c(asec))

## Compare disaggregated (by sector) estimates in target year vs the BEI.
## new sectors
unique(actions_F1[,c("action_sector")])
unique(clems2[,c("energy_consumption_sector_id")])
clems2$sector =ifelse(clems2$energy_consumption_sector_id %in% c(10,11,12,32,33,34,36,37,38,39),as.character("transport"),ifelse(clems2$energy_consumption_sector_id %in% c(13,14),as.character("others"),ifelse(clems2$energy_consumption_sector_id %in% c(1,4,26,2,28,3,6,7,8),as.character("buildings"),"extra")))
actions_F1$sector =ifelse(actions_F1$action_sector %in% c("municipal","tertiary","residential","industry"),as.character("buildings"),actions_F1$action_sector)
emissions = setDT(clems2)[, lapply(.SD, sum), by=.(organisation_name,action_plan_id,emission_inventory_id,base_year,sector), 
                          .SDcols=c("emission_measure")]
emissions$action_plan_id = as.character(emissions$action_plan_id)
acti = setDT(actions_F1)[, lapply(.SD, sum), by=.(action_plan_id,co2_target,target_year,base_year,sector), 
                         .SDcols=c("estimates_co2_reduction")]
actis = left_join(acti,emissions,by=c("action_plan_id","base_year","sector"))
actis2 = subset(actis,actis$sector %in% c("buildings","transport","others") & !is.na(actis$emission_measure))
actis2$difss = (actis2$estimates_co2_reduction-actis2$emission_measure)/actis2$emission_measure

xactis = subset(actis2,actis2$difss>0)
xactis = xactis[with(xactis, order(-xactis$difss)), ]

xactis$asec = paste(xactis$action_plan_id,xactis$sector,"")
actions_F1$asec = paste(actions_F1$action_plan_id,actions_F1$sector,"")

actions_F3 = subset(actions_F1, !(actions_F1$asec %in% xactis$asec))
actions_F3 = subset(actions_F3, select = -c(sector,asec))
summary(actions_F3)

write.table(actions_F3, file = "actions_new.txt", sep = "\t",row.names = FALSE, col.names = TRUE)

## Individual policies
act = read_excel("df2Cbefore_cam.xlsx",col_types = c(rep("text",15),rep("numeric",2),rep("text",3),"numeric","text",rep("numeric",3),rep("text",3),rep("numeric",3),rep("text",4),"numeric","text","numeric","text",rep("numeric",6),rep("text",2),rep("numeric",2),rep("text",8),"numeric"))

## Identify duplicate actions, ignore organisation_id because these actions are published without it
dups = act[duplicated(act[,c("action_plan_id","action_id","action_title","short_description","comment","mitigation_action_sector","mitigation_action_area","mitigation_action_instrument","adaptation_hazards","adaptation_sectors","energy_poverty_details_macro_areas")])|duplicated(act[,c("action_plan_id","action_id","action_title","short_description","comment","mitigation_action_sector","mitigation_action_area","mitigation_action_instrument","adaptation_hazards","adaptation_sectors","energy_poverty_details_macro_areas")], fromLast=TRUE),]

dups = subset(dups, select = -c(organisation_id,organisation_name))
act2b = act[with(act, order(act$action_plan_id,act$action_id,act$action_title,act$short_description,act$comment,act$mitigation_action_sector,act$mitigation_action_area,act$mitigation_action_instrument,act$adaptation_hazards,act$adaptation_sectors,act$energy_poverty_details_macro_areas)), ]

act2c = setDT(act2b)[, lapply(.SD, first), by=.(action_plan_id,action_id,action_title,short_description,comment,mitigation_action_sector,mitigation_action_area,mitigation_action_instrument,adaptation_hazards,adaptation_sectors,mitigation_details_vulnerability_group_targeter,adaptation_details_vulnerability_group_targeter,energy_poverty_details_macro_areas),.SDcols=c("mitigation_estimated_impact_co2_reduction","mitigation_estimated_impact_energy_savings","mitigation_estimated_impact_renewable_energy_production","adaptation_reached_description","adaptation_reached_indicator")]

## Change negative values to 0
negatives = subset(act2c, mitigation_estimated_impact_co2_reduction<0 | mitigation_estimated_impact_energy_savings<0 | mitigation_estimated_impact_renewable_energy_production<0)
act2c$mitigation_estimated_impact_co2_reduction = ifelse(act2c$mitigation_estimated_impact_co2_reduction<0,0,act2c$mitigation_estimated_impact_co2_reduction)
act2c$mitigation_estimated_impact_energy_savings = ifelse(act2c$mitigation_estimated_impact_energy_savings<0,0,act2c$mitigation_estimated_impact_energy_savings)
act2c$mitigation_estimated_impact_renewable_energy_production = ifelse(act2c$mitigation_estimated_impact_renewable_energy_production<0,0,act2c$mitigation_estimated_impact_renewable_energy_production)

## Import the cleaned emissions
eAP = read.table("df_EIEF_AP.txt", header=TRUE)
eMR = read.table("df_EIEF_MR.txt", header=TRUE)
clems2 = rbind(eAP,eMR)
clems2$action_plan_id = as.character(clems2$action_plan_id)

## Cleaned actions
actions_F3 = read.table("actions_new.txt", header=TRUE)

act2 = act2c
p_ab = subset(act2, !is.na(action_title))

## Coherence of initial CO2 reduction, savings, RElecS production, estimates by mitigation action sectors
non_miti=subset(p_ab,is.na(mitigation_action_sector))
study=subset(p_ab,!is.na(mitigation_action_sector))
unique(study[,c("mitigation_action_sector")])

study$ps = study$mitigation_estimated_impact_energy_savings + study$mitigation_estimated_impact_renewable_energy_production

study$ifactor = study$mitigation_estimated_impact_co2_reduction/study$ps
oifs = subset(study,study$ifactor>2 & study$ifactor!=Inf)
oifs = oifs[with(oifs, order(-oifs$ifactor)), ]
lifs = subset(study,study$ifactor<0.01)
lifs = lifs[with(lifs, order(lifs$ifactor)), ]
outs = rbind(oifs,lifs)

outs$asec = paste(outs$action_plan_id,outs$action_id,outs$mitigation_action_sector,"")
study$asec = paste(study$action_plan_id,study$action_id,study$mitigation_action_sector,"")

tochange = subset(study,asec %in% outs$asec)
study$mitigation_estimated_impact_co2_reduction = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_co2_reduction)
study$mitigation_estimated_impact_energy_savings = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_energy_savings)
study$mitigation_estimated_impact_renewable_energy_production = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_renewable_energy_production)
study = subset(study, select=-c(asec,ifactor,ps))

## 5. Compare estimates reduction by sector in Actions
## new sectors
unique(study[,c("mitigation_action_sector")])
unique(actions_F3[,c("action_sector")])
study$mitigation_action_sector = tolower(study$mitigation_action_sector)
study$mitigation_action_sector = ifelse(study$mitigation_action_sector %in% c("waste","others"),"other",study$mitigation_action_sector)
actionsF4 = actions_F3
actionsF4$action_sector = ifelse(as.character(actionsF4$action_sector) %in% c("lighting"),"municipal",as.character(actionsF4$action_sector))
unique(actionsF4[,c("action_sector")])

study$action_sector = ifelse(study$mitigation_action_sector %in% c("residential buildings"),"residential",ifelse(study$mitigation_action_sector %in% c("municipal buildings","municipal buildings, equipment, facilities"),"municipal",ifelse(study$mitigation_action_sector %in% c("tertiary (non municipal) buildings, equipment/factilities"),"tertiary",ifelse(study$mitigation_action_sector %in% c("local heat/cold production"),"heat",ifelse(study$mitigation_action_sector %in% c("local electricity production"),"electricity",study$mitigation_action_sector)))))
unique(study[,c("action_sector")])

mF2 = setDT(study)[, lapply(.SD, sum, na.rm=TRUE), by=.(action_plan_id,action_sector), 
                   .SDcols=c("mitigation_estimated_impact_co2_reduction","mitigation_estimated_impact_energy_savings","mitigation_estimated_impact_renewable_energy_production")]

acs_t = actionsF4[with(actionsF4, order(actionsF4$action_plan_id,-actionsF4$target_year)), ]
acs_t2 = setDT(acs_t)[, lapply(.SD, first), by=.(action_plan_id),.SDcols=c("target_year")]
acs_t2$aux = paste(acs_t2$action_plan_id,acs_t2$target_year,"")
actionsF4$aux = paste(actionsF4$action_plan_id,actionsF4$target_year,"")
acs = subset(actionsF4,aux %in% acs_t2$aux)

aF3 = setDT(acs)[, lapply(.SD, sum, na.rm=TRUE), by=.(action_plan_id,action_sector), 
                 .SDcols=c("estimates_energy_production","estimates_energy_savings","estimates_co2_reduction")]
aF3 = aF3[,c("action_plan_id","action_sector","estimates_energy_production","estimates_energy_savings","estimates_co2_reduction")]
aF3$action_plan_id = as.character(aF3$action_plan_id)
mF2_a = left_join(mF2,aF3,by=c("action_plan_id","action_sector"))

mF2_a$difss = (mF2_a$mitigation_estimated_impact_co2_reduction-mF2_a$estimates_co2_reduction)/mF2_a$estimates_co2_reduction

outs = subset(mF2_a,mF2_a$difss>0.2) 
outs = outs[with(outs, order(-outs$difss)), ]

outs$asec = paste(outs$action_plan_id,outs$action_sector,"")
study$asec = paste(study$action_plan_id,study$action_sector,"")

tochange = subset(study,asec %in% outs$asec)
study$mitigation_estimated_impact_co2_reduction = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_co2_reduction)
study$mitigation_estimated_impact_energy_savings = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_energy_savings)
study$mitigation_estimated_impact_renewable_energy_production = ifelse(study$asec %in% outs$asec,NA,study$mitigation_estimated_impact_renewable_energy_production)
mF3 = subset(study, select = -c(action_sector,asec))

w_estimates = subset(mF3,!is.na(mitigation_estimated_impact_co2_reduction | mitigation_estimated_impact_energy_savings | mitigation_estimated_impact_renewable_energy_production))

## new policies
policies2 = rbind(mF3,non_miti) 

others = unique(act[,c("action_plan_id","action_id","key_action_title")])
transa4 = left_join(policies2,others,by=c("action_plan_id","action_id"))
others = act[,c("action_plan_id","action_id","action_origin","responsible_body","website_address","video_address","timeframe_start","timeframe_end","action_implementation_status","action_stakeholders","financing_sources",
                "adaptation_reached_value","adaptation_reached_unit","energy_poverty_details_macro_areas","energy_poverty_details_vulnerability_group_targeter","energy_poverty_outcome_reached_description","energy_poverty_outcome_reached_indicator", "energy_poverty_outcome_reached_unit","energy_poverty_outcome_reached_value")]
others = unique(others[c("action_plan_id","action_id","action_origin","responsible_body","website_address","video_address","timeframe_start","timeframe_end","action_implementation_status","action_stakeholders","financing_sources",
                         "adaptation_reached_value","adaptation_reached_unit","energy_poverty_details_macro_areas","energy_poverty_details_vulnerability_group_targeter","energy_poverty_outcome_reached_description","energy_poverty_outcome_reached_indicator", "energy_poverty_outcome_reached_unit","energy_poverty_outcome_reached_value")])
transa5 = left_join(transa4,others,by=c("action_plan_id","action_id"))
policies4 = transa5[,c("action_plan_id","action_id","action_title","key_action_title",
                       "action_origin","responsible_body","short_description","website_address","video_address","timeframe_start","timeframe_end",
                       "action_implementation_status","action_stakeholders","comment","financing_sources","mitigation_action_sector","mitigation_action_area",
                       "mitigation_action_instrument","mitigation_estimated_impact_co2_reduction","mitigation_estimated_impact_energy_savings","mitigation_estimated_impact_renewable_energy_production",
                       "adaptation_hazards","adaptation_sectors","adaptation_reached_description","adaptation_reached_indicator","adaptation_reached_value","adaptation_reached_unit",
                       "energy_poverty_details_vulnerability_group_targeter","energy_poverty_outcome_reached_description","energy_poverty_outcome_reached_indicator", "energy_poverty_outcome_reached_unit","energy_poverty_outcome_reached_value")]

write.csv(policies4,"policies_new.csv", row.names = FALSE)

## Part V. Energy and emissions inventories for publication
ners_ap = read.table("ners_AP.txt", header=TRUE)
ners_mr = read.table("ners_MR.txt", header=TRUE)
ners_ap$monitoring_report_id = NA
ners_ap$monitoring_report_submission_status_id = NA
ners = rbind(ners_ap,ners_mr)
ners$emission_inventory_id = as.character(ners$emission_inventory_id)
ners_a2 = subset(ners,!is.na(ners$non_energy_related_sectors))
ners_a3 = subset(ners_a2,ners_a2$co2_eq_emission_t>0)
ners_a4 = subset(ners_a3,non_energy_related_sectors %in% c("Other non-energy related such as fuggitive emissions", "Solid waste disposal","Incineration and open burning of waste","Biological treatment of solid waste","Other","Wastewater treatment and discharge"))
ners_a4$non_energy_related_sectors = ifelse(ners_a4$non_energy_related_sectors %in% c("Wastewater treatment and discharge"),ners_a4$non_energy_related_sectors,"Waste management")
ners_a5 = ners_a4 
ners_a5 = ners_a5 %>% rename( bei_flag = baseline_year_flag, sector=non_energy_related_sectors, emission_measure=co2_eq_emission_t )
ners_a5$energy_measure = 0
ners_a5$carrier = "Non-energy related emissions"
ners_a5b = setDT(ners_a5)[, lapply(.SD, sum, na.rm=TRUE), by=.(emission_inventory_id,sector), 
                          .SDcols=c("energy_measure","emission_measure")]

ners_a6 = unique(ners_a5[,c("organisation_id","organisation_name","action_plan_id",
                            "action_plan_submission_status_id","monitoring_report_id",
                            "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                            "inventory_year","sector","carrier")])
ners_a6b = left_join(ners_a6,ners_a5b,by=c("emission_inventory_id","sector"))

extras = unique(dfEI_cl3[c("emission_inventory_id","activity_reporting_unit",
                           "emission_factor_type","emission_reporting_unit","population_in_the_inventory_year",
                           "country_code","IEA_code","area")])

ners_a7 = left_join(ners_a6b,extras,by=c("emission_inventory_id"))
ners_a8 = subset(ners_a7,!is.na(emission_reporting_unit))

dfEI_cl4 = dfEI_cl3 
dfEI_cl4$sector = ifelse(dfEI_cl4$energy_consumption_sector_id %in% c(3,6,7,8,12,13,14),as.character(dfEI_cl4$energy_consumption_sector),as.character(dfEI_cl4$item_text))
aa=unique(dfEI_cl4[c("sector")])
dfEI_cl4$carrier = ifelse(dfEI_cl4$energy_consumption_carrier_id %in% c(3,4,5,6,7,8,9,10),"Fossil fuels",ifelse(dfEI_cl4$energy_consumption_carrier_id %in% c(11,12,13,14,15,22),"Renewable fuels",as.character(dfEI_cl4$energy_consumption_carrier)))
ab=unique(dfEI_cl4[c("carrier")])

dfEI_cl5 = setDT(dfEI_cl4)[, lapply(.SD, sum, na.rm=TRUE), by=c("organisation_id","organisation_name","action_plan_id",
                                                                "action_plan_submission_status_id","monitoring_report_id",
                                                                "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                                                                "inventory_year","sector","carrier","activity_reporting_unit","emission_reporting_unit",
                                                                "emission_factor_type","population_in_the_inventory_year",
                                                                "country_code","IEA_code","area"),.SDcols=c("emission_measure","energy_measure")]

dfEI_ners = rbind(dfEI_cl5,ners_a8)
dfEI_ners = dfEI_ners[with(dfEI_ners, order(dfEI_ners$organisation_id,
                                            dfEI_ners$action_plan_id,
                                            dfEI_ners$emission_inventory_id)), ]

ac=unique(dfEI_ners[,c("sector")])
ad=unique(dfEI_ners[,c("carrier")])

dfEI_ners = dfEI_ners[,c("organisation_id","organisation_name","action_plan_id",
                         "action_plan_submission_status_id","monitoring_report_id",
                         "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                         "inventory_year","activity_reporting_unit","energy_measure",
                         "emission_factor_type","emission_reporting_unit","emission_measure","sector","carrier",
                         "population_in_the_inventory_year","country_code","IEA_code","area")]


## For publishing OpenDataPortal
dfEI_p = dfEI_ners
dfEI_p$macro_sector = ifelse(dfEI_p$sector %in% c("Transport non allocated","Private and commercial transport","Municipal fleet","Public transport"),"Transportation",
                             ifelse(dfEI_p$sector %in% c("Other non allocated","Agriculture, Forestry, Fisheries"),"Other",
                                    ifelse(dfEI_p$sector %in% c("Wastewater treatment and discharge","Waste management"),"Waste/wastewater",
                                           dfEI_p$sector)))
ac2=unique(dfEI_p[,c("macro_sector")])

dfEI_q = setDT(dfEI_p)[, lapply(.SD, sum, na.rm=TRUE), by=.(emission_inventory_id,macro_sector,carrier),.SDcols=c("emission_measure","energy_measure")]
dfEI_p_q = unique(dfEI_p[,c("organisation_id","organisation_name","action_plan_id",
                            "action_plan_submission_status_id","monitoring_report_id",
                            "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                            "inventory_year","activity_reporting_unit",
                            "emission_factor_type","emission_reporting_unit","macro_sector","carrier",
                            "population_in_the_inventory_year","country_code","IEA_code","area")])

dfEI_pq = left_join(dfEI_p_q,dfEI_q,by=c("emission_inventory_id","macro_sector","carrier"))

dfEI_pq = dfEI_pq[,c("organisation_id","organisation_name","action_plan_id",
                     "action_plan_submission_status_id","monitoring_report_id",
                     "monitoring_report_submission_status_id","bei_flag","emission_inventory_id",
                     "inventory_year","activity_reporting_unit","energy_measure",
                     "emission_factor_type","emission_reporting_unit","emission_measure","macro_sector","carrier",
                     "population_in_the_inventory_year","country_code","IEA_code","area")]

dfEIpq_AP = subset(dfEI_pq,dfEI_pq$emission_inventory_id %in% emisa$emission_inventory_id)
dfEIpq_MR = subset(dfEI_pq,dfEI_pq$emission_inventory_id %in% emisr$emission_inventory_id)

write.table(dfEIpq_AP, file = "EI_4pub_AP.txt", sep = "\t", row.names = FALSE, col.names = TRUE)
write.table(dfEIpq_MR, file = "EI_4pub_MR.txt", sep = "\t", row.names = FALSE, col.names = TRUE)

## end :)