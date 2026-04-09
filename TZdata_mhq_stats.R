### Run Stats
##Data Preparation
##trend of only mean values

# Function to add significance stars
add_stars <- function(p) {
  stars <- ifelse(p <= 0.001, "***",
                  ifelse(p <= 0.01, "**",
                         ifelse(p <= 0.05, "*", "")))
  p_sci <- formatC(p, format = "e", digits = 2)
  paste0(p_sci, stars)
}

##trend of only mean values
##Run One-way Anova followed by Tukey's HSD
RunStats_1wayAnovaTukey_MeanTrend <- function (dat, mhq_flds){
  ##ONE way ANOVA by Tribe/group AND TUKEYS HSD##
  ##CREATE NEW AGE GROUPS##
  ##RUN and Print Stats##
  
  dat$NewAgeGroup = as.character(dat$AgeGroup)
  dat$NewAgeGroup=factor(dat$NewAgeGroup)
  index = which(dat$NewAgeGroup=='')
  if (length(index)>0){dat = dat[-(index),]}
  
  group_names = levels(dat$Group)
  
  tukeys_summary_group = NULL
  OverallStatsMerged = NULL
  aov.stats = list()
  tukey.stats = list()
  trendR2.stats = list()
  
  for (k in 1:length(mhq_flds)){
    print(mhq_flds[k])
    
    aov.stats[[mhq_flds[k]]] = NULL
    tukey.stats[[mhq_flds[k]]] = NULL
    trendR2.stats[[mhq_flds[k]]] = NULL
    
    for (q in 1:length(group_names)){
      #print(group_names[q])
      dat0 = na.omit(dat[dat$Group %in% group_names[q],c('NewAgeGroup',mhq_flds[k])])
      model <- aov(as.formula(paste(mhq_flds[k],'~ NewAgeGroup')), data = dat0)  
      tukey_results <- TukeyHSD(model)
      
      aov.stats[[mhq_flds[k]]] = rbind(aov.stats[[mhq_flds[k]]], data.frame(n = nrow(dat0), Group = group_names[q], aov.F = summary(model)[[1]][1,'F value'],aov.pval=summary(model)[[1]][1,'Pr(>F)']))
      tukey.stats[[mhq_flds[k]]] = cbind(tukey.stats[[mhq_flds[k]]], tukey_results$NewAgeGroup[,c('p adj')])
      rm(model,tukey_results)
      
      ##calculate mean value trend
      colnames(dat0)[2]='Field'
      mhq_by_age = ddply(dat0, .(NewAgeGroup), summarise, 
                         mean_val = round(mean(Field, na.rm=T),1))
      
      x=as.numeric(mhq_by_age$NewAgeGroup)
      y=mhq_by_age$mean_val
      df = data.frame(x,y)
      model <- lm(y ~ x, data = df)
      model_summary <- summary(model)
      slope <- model_summary$coefficients["x", "Estimate"]
      r_squared <- model_summary$r.squared
      p_value <- model_summary$coefficients["x", "Pr(>|t|)"]
      rm(df, model, model_summary, x, y)
      
      trendR2.stats[[mhq_flds[k]]]=rbind(trendR2.stats[[mhq_flds[k]]],
                                         data.frame(Group = group_names[q], slope, p_value,r_squared))
      
      
    }
    colnames(tukey.stats[[mhq_flds[k]]]) = group_names ##tukeys p-value
    
    ##add stars
    ts = t(sapply(1:nrow(tukey.stats[[mhq_flds[k]]]), function (x) add_stars(tukey.stats[[mhq_flds[k]]][x,])))
    colnames(ts) = colnames(tukey.stats[[mhq_flds[k]]])
    row.names(ts) = row.names(tukey.stats[[mhq_flds[k]]])
    ts=t(ts)
    tukey.stats[[mhq_flds[k]]]= cbind(Group = row.names(ts), ts)
    
    aov.stats[[mhq_flds[k]]]$aov.pval = add_stars(aov.stats[[mhq_flds[k]]]$aov.pval)
    
    ##Merge_stats + trend-stats 
    trendR2.stats[[mhq_flds[k]]]$slope = round(trendR2.stats[[mhq_flds[k]]]$slope,2)
    trendR2.stats[[mhq_flds[k]]]$p_value_sign = add_stars(trendR2.stats[[mhq_flds[k]]]$p_value)
    trendR2.stats[[mhq_flds[k]]]$r_squared = round(trendR2.stats[[mhq_flds[k]]]$r_squared,4)
    
    
    merged_Stats = merge(aov.stats[[mhq_flds[k]]],tukey.stats[[mhq_flds[k]]], sort=F)
    merged_Stats = merge(merged_Stats,trendR2.stats[[mhq_flds[k]]], sort=F)
    merged_Stats = cbind('Field' = mhq_flds[k], merged_Stats)
    OverallStatsMerged = rbind(merged_Stats, OverallStatsMerged)
  }
  
  
  return(OverallStatsMerged)
}

Summary_Anova_Tukey <- function (dat, mhq_flds, mhq_fld_labels, broad_age_grps){
  aov.stats = NULL
  tukey.stats.diff = NULL
  tukey.stats.pval = NULL
  
  for (k in 1:length(mhq_flds)){
    for (t in 1:length(broad_age_grps)){
      anova_model <- aov(as.formula(paste(mhq_flds[k],'~ Group')), data = dat[dat$BroadAgeGroup %in% broad_age_grps[t],])
      #summary(anova_model)
      tukey_results=TukeyHSD(anova_model)
      aov.stats = rbind(aov.stats, data.frame(Fld = mhq_fld_labels[k], Age = broad_age_grps[t],
                                              aov.F = round(summary(anova_model)[[1]][1,'F value'],1),
                                              aov.pval=add_stars(summary(anova_model)[[1]][1,'Pr(>F)'])))
      
      tukey.stats.diff = cbind(tukey.stats.diff, round(tukey_results$Group[,c('diff')],2))
      tukey.stats.pval = cbind(tukey.stats.pval, 'p adj'=add_stars(tukey_results$Group[,c('p adj')]))
      colnames(tukey.stats.diff)[ncol(tukey.stats.diff)] = paste(mhq_fld_labels[k],broad_age_grps[t],sep=':')
      colnames(tukey.stats.pval)[ncol(tukey.stats.pval)] = paste(mhq_fld_labels[k],broad_age_grps[t],sep=':')
      row.names(tukey.stats.pval) = row.names(tukey.stats.diff)
    }}
  
  ##reorganizing Tukey stats
  out=data.frame(t(tukey.stats.diff))
  out$Age = sapply(row.names(out), function (x) strsplit(x,'[:]')[[1]][2])
  out$Fld =  sapply(row.names(out), function (x) strsplit(x,'[:]')[[1]][1])
  colnames(out)[1:3]=paste(colnames(out)[1:3],'diff',sep='.')
  out = out[,c(4,5,1:3)]
  out.diff = out[order(out$Age,decreasing = T),]
  
  out=data.frame(t(tukey.stats.pval))
  out$Age = sapply(row.names(out), function (x) strsplit(x,'[:]')[[1]][2])
  out$Fld =  sapply(row.names(out), function (x) strsplit(x,'[:]')[[1]][1])
  colnames(out)[1:3]=paste(colnames(out)[1:3],'pval',sep='.')
  out = out[,c(4,5,1:3)]
  out.pval = out[order(out$Age,decreasing = T),]
  
  out = merge(out.diff, out.pval, by=c('Age','Fld'),sort = F)
  
  return(list(anova = aov.stats, tukey = out, n=as.matrix(table(dat$Group,dat$BroadAgeGroup))))
}


library(mblm)
library(plyr)
library(kableExtra)

broad_age_grps_list=list("18-34"=c('25-34','18-24'),
                         '35-54'=c('35-44','45-54'),
                         '55+'=c('55-64','65+','65-74','75+'))

mhq_fld_labels = read.csv('mhq_field_labels2.csv')
dataset_complete = read.csv('Study_DemoDataset_N10000.csv')
  
##remove NA from Overall.MHQ 
remove_index = which(is.na(dataset_complete$Overall.MHQ)==T )
if (length(remove_index)>0){dataset_complete = dataset_complete[-remove_index,]}
dataset_complete = dataset_complete[which(dataset_complete$clean==1),]

TribeOfInterest = list('Hadza' = c('Hadza'), 
                       'Rural-TZ' = setdiff(unique(dataset_complete$Tribe), c('Hadza',"GM.Online")),
                       'IE-TZ' = c('GM.Online'), 'IE-Global' = c("GM.Global") ) 

dataset_complete$Group=NA
for (i in 1:length(TribeOfInterest)){
  dataset_complete$Group[dataset_complete$Tribe %in% TribeOfInterest[[i]]] = names(TribeOfInterest)[i]
}
dataset_complete$AgeGroup[dataset_complete$AgeGroup %in% c("65-74",'75+')]="65+"

##remove who did not understand questionnaire 
remove_index = which(dataset_complete$Understanding.Assessment=='No' & dataset_complete$Group=='GM' )
if (length(remove_index)>0){dataset_complete = dataset_complete[-remove_index,]}


##Fig 2 - ANOVA and Mean Fit for Overall MHQ vs Age
##MHQ vs age trand
df = dataset_complete[,c('Group','AgeGroup',  "Overall.MHQ")]
df$AgeGroup[df$AgeGroup %in% c('65+')] = '55-64'
df$Group = factor(df$Group, levels = names(TribeOfInterest))
df$AgeGroup = factor(df$AgeGroup)
output = RunStats_1wayAnovaTukey_MeanTrend(dat=df,"Overall.MHQ")
write.csv(output, 'MHQ_vs_Age_Anova_Trend.csv', row.names=F)

##Fig 3 - Radar Plots Anova followed by Tukey's HSD : MHQ dims

mhq_flds = mhq_fld_labels$Fld[mhq_fld_labels$Type == 'Dims'][mhq_fld_labels$Order[mhq_fld_labels$Type == 'Dims']] 
df = dataset_complete[,c('Group','AgeGroup', mhq_flds)]
df$AgeGroup[df$AgeGroup %in% c('65-74','75+','65+')] = '55-64'
df$Group = factor(df$Group, levels = names(TribeOfInterest))
df$AgeGroup = factor(df$AgeGroup)

# Fit ANOVA model
df = df[,c('Group','AgeGroup',mhq_flds)]
df$Group=as.character(df$Group)
dg=df[df$Group %in% c('Rural-TZ','Hadza','IE-TZ'),]
dg$BroadAgeGroup = NA
for (q1 in 1:length(broad_age_grps_list)){
  dg$BroadAgeGroup[dg$AgeGroup %in% broad_age_grps_list[[q1]]] = names(broad_age_grps_list)[q1] 
}

broad_age_grps = names(broad_age_grps_list)
mhq_label=merge(data.frame(Fld=mhq_flds),mhq_fld_labels,sort=F, all.x=T)$FullLabel
stats = Summary_Anova_Tukey(dat=dg, mhq_flds = mhq_flds, mhq_fld_labels =  mhq_label, rev(broad_age_grps))
# kable(stats$anova, booktabs = T, caption = "Anova - MHQ dims by population group",row.names = T)%>%kable_styling(full_width = F,position="left")
kable(stats$tukey, booktabs = T, caption = "Tukeys HSD - MHQ dims by population group",row.names = F)%>%kable_styling(full_width = F,position="left") 
# kable(stats$n, booktabs = T, caption = "N used for above test - by Group, population group",row.names = T)%>%kable_styling(full_width = F,position="left") 
write.csv(stats$tukey, 'MHQDims_AnovaTukeys.csv', row.names=F)

rm(stats, dg)

#############

spectrum=mhq_fld_labels[mhq_fld_labels$Type == 'Capacity', c('Fld','Order')]
spectrum=spectrum[order(spectrum$Order),]
spectrum_qnames = spectrum$Fld

problem=mhq_fld_labels[mhq_fld_labels$Type == 'Problem', c('Fld','Order')]
problem=problem[order(problem$Order),]
problem_qnames = problem$Fld
rm(spectrum, problem)

df = dataset_complete[,c('Group','AgeGroup', spectrum_qnames, problem_qnames)]
df$AgeGroup[df$AgeGroup %in% c('65-74','75+','65+')] = '55-64'
df$Group = factor(df$Group, levels = names(TribeOfInterest))
df$AgeGroup = factor(df$AgeGroup)

spec_label=merge(data.frame(Fld=spectrum_qnames),mhq_fld_labels,sort=F, all.x=T)$FullLabel
prob_label=merge(data.frame(Fld=problem_qnames),mhq_fld_labels,sort=F, all.x=T)$FullLabel

# Fit ANOVA model
dg=df
df$Group=as.character(df$Group)
dg=dg[dg$Group %in% c('Rural-TZ','Hadza','IE-TZ'),]
dg$BroadAgeGroup = NA
for (q1 in 1:length(broad_age_grps_list)){
  dg$BroadAgeGroup[dg$AgeGroup %in% broad_age_grps_list[[q1]]] = names(broad_age_grps_list)[q1] 
}
broad_age_grps = names(broad_age_grps_list)

###Capacity Stats
stats = Summary_Anova_Tukey(dat=dg, mhq_flds = spectrum_qnames, mhq_fld_labels = spec_label, broad_age_grps)
#kable(stats$anova, booktabs = T, caption = "Anova - Capacities by population group",row.names = T)%>%kable_styling(full_width = F,position="left")
kable(stats$tukey, booktabs = T, caption = "Tukeys HSD - Capacities by population group",row.names = F)%>%kable_styling(full_width = F,position="left") 
#kable(stats$n, booktabs = T, caption = "N used for above test - by Group, population group",row.names = T)%>%kable_styling(full_width = F,position="left") 
write.csv(stats$tukey, 'MHQCapacities_Tukeys.csv', row.names=F)

rm(stats)

###Problem Stats
stats = Summary_Anova_Tukey(dat=dg, mhq_flds = problem_qnames, mhq_fld_labels = prob_label,broad_age_grps)
#kable(stats$anova, booktabs = T, caption = "Anova - Problem by population group",row.names = T)%>%kable_styling(full_width = F,position="left")
kable(stats$tukey, booktabs = T, caption = "Tukeys HSD - Problem by population group",row.names = F)%>%kable_styling(full_width = F,position="left") 
#kable(stats$n, booktabs = T, caption = "N used for above test - by Group, population group",row.names = T)%>%kable_styling(full_width = F,position="left") 
write.csv(stats$tukey, 'MHQProblems_Tukeys.csv', row.names=F)


rm(stats)


