library(tidyr)
library(stringr)
library(lemon)
library(lmerTest)
library(car)
library(patchwork)
library(dplyr)
library(ggplot2)

#Set-Up Data
#path <- "<add_path>"

UV_Table <- read.table(file.path(path,'UV_Zap_Data.txt'),sep = '\t',
                       header = T,check.names = F, comment.char = "",quote = "" )

time_variables <- c("Activitive_Post_30mins","Activitive_Post_1hr","Activitive_Post_2hr",
                    "Activitive_Post_4hr","Activitive_Post_24hr")
#Make the time variable discrete, not continuous
time <- c(0.5,1,2,4,24)

#Correct sex variable
UV_Table$Sex <- ifelse(UV_Table$Sex == "Female","female",UV_Table$Sex )
UV_Table$Sex <- ifelse(UV_Table$Sex == "Male","male",UV_Table$Sex )

#Specify name of an experiment
UV_Table$experiment <- paste(UV_Table$Species,UV_Table$Strain,UV_Table$Sex,UV_Table$Replicate,sep = '_')

df_list <- list()

for(v in time_variables){
  
  UV_Table[,v] <- gsub("\"","",UV_Table[,v])
  
  UV_Table_time <- UV_Table %>% tidyr::separate(col = v,into = c('active','barely_active','inactive','dead'),remove = T,sep = ',') %>%
    dplyr::select(-setdiff(time_variables,v)) %>%
    dplyr::mutate(Activity_time = time[time_variables == v], active = str_remove(active,'a'),active = as.numeric(ifelse(active=="",0,active)),
                  barely_active = str_remove(barely_active,'b'), barely_active = as.numeric(ifelse(barely_active=="",0,barely_active)),
                  inactive = str_remove(inactive,'i'), inactive = as.numeric(ifelse(inactive=="",0,inactive)),
                  dead = str_remove(dead,'d'), dead = as.numeric(ifelse(dead=="",0,dead)),
                  dead = 5 - (active + barely_active + inactive))
  df_list[[v]] <- UV_Table_time
}

# Bind all vectors (v) from the for loop above
UV_Table_all <- dplyr::bind_rows(df_list)
UV_Table_all$Activity_time_factor <- factor(UV_Table_all$Activity_time)
UV_Table_all$Region <- factor(UV_Table_all$Region, levels = c("Mainland_Dyak","Island_Dyak","Dsan"))

#Split Line plot
pj <- position_jitter(width = 0.1, seed = 9)

plot_active_split <- ggplot(UV_Table_all, aes_string(y = "active",x = "Activity_time_factor"))+
  geom_jitter(aes_string(color = "Region"),width = 0.1, alpha = 0.7) +
  theme_classic()+
  geom_pointpath(aes_string(color = "Region", group = "experiment"), position = pj, alpha = 0.3, linecolour = "gray", linesize = 0.1)+
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38"))+
  labs( x = 'post-UV exposure (hr)', y = 'Active Fly Count') +
  theme(axis.text.x = element_text(size = 16),axis.text.y = element_text(size = 16), axis.title = element_text(size = 20))+
  facet_wrap(.~Sex+Region)

pdf(file.path(path,'active_split_sex_region_flies.pdf'),8,5)
print(plot_active_split)
dev.off()

#post-activity plots

plot_active <- ggplot(UV_Table_all, aes_string(y = "active",x = "Activity_time_factor"))+
  geom_jitter(aes_string(color = "Region"),width = 0.1, alpha = 0.7) +
  theme_classic()+
  geom_pointpath(aes_string(color = "Region", group = "experiment"), position = pj, alpha = 0.3, linecolour = "gray", linesize = 0.1)+
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  labs( x = 'post-UV exposure (hr)', y = 'number of active flies') +
  facet_wrap(.~Sex)

plot_ba <- ggplot(UV_Table_all, aes_string(y = "barely_active",x = "Activity_time_factor"))+
  geom_jitter(aes_string(color = "Region"),width = 0.1, alpha = 0.7) +
  theme_classic()+
  geom_pointpath(aes_string(color = "Region", group = "experiment"), position = pj, alpha = 0.3, linecolour = "gray", linesize = 0.1)+
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  labs( x = 'post-UV exposure (hr)', y = 'number of barely active flies') +
  facet_wrap(.~Sex)

plot_inactive <- ggplot(UV_Table_all, aes_string(y = "inactive",x = "Activity_time_factor"))+
  geom_jitter(aes_string(color = "Region"),width = 0.1, alpha = 0.7) +
  theme_classic()+
  geom_pointpath(aes_string(color = "Region", group = "experiment"), position = pj, alpha = 0.3, linecolour = "gray", linesize = 0.1)+
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  labs( x = 'post-UV exposure (hr)', y = 'number of inactive flies') +
  facet_wrap(.~Sex)

plot_dead <- ggplot(UV_Table_all, aes_string(y = "dead",x = "Activity_time_factor"))+
  geom_jitter(aes_string(color = "Region"),width = 0.1, alpha = 0.7) +
  theme_classic()+
  geom_pointpath(aes_string(color = "Region", group = "experiment"), position = pj, alpha = 0.3, linecolour = "gray", linesize = 0.1)+
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  labs( x = 'post-UV exposure (hr)', y = 'number of dead flies') +
  facet_wrap(.~Sex)

pdf(file.path(path,'split_active_flies.pdf'),8,5)
print(plot_active)
dev.off()

pdf(file.path(path,'split_barely_active_flies.pdf'),8,5)
print(plot_ba)
dev.off()

pdf(file.path(path,'split_inactive_flies.pdf'),8,5)
print(plot_inactive)
dev.off()

pdf(file.path(path,'split_dead_flies.pdf'),8,5)
print(plot_dead)
dev.off()

# Linear models by sex
UV_Table_male <- UV_Table_all %>% filter(Sex == 'male')
UV_Table_female <- UV_Table_all %>% filter(Sex == 'female')
fit_male_summary <- summary(lmerTest::lmer(active ~ Activity_time * Region + (1|experiment), data = UV_Table_male  ))
fit_male_anova <- anova(lmerTest::lmer(active ~ Activity_time * Region + (1|experiment), data = UV_Table_male  ))

fit_female_summary <- summary(lmerTest::lmer(active ~ Activity_time * Region + (1|experiment), data = UV_Table_female  ))
fit_female_anova <- anova(lmerTest::lmer(active ~ Activity_time * Region + (1|experiment), data = UV_Table_female  ))

#MANOVA
UV_Post_UV_realtime_MANOVA <- manova(cbind(active, barely_active, inactive, dead) ~ Activity_time * Region * Sex, data = UV_Table_all)
summary.manova(UV_Post_UV_realtime_MANOVA, tol = 0)

#Boxplots
UV_Table_sub <- UV_Table_all %>% dplyr::select(c("Species","Region","Strain","Sex" ,"Replicate","active",
                                       "barely_active","inactive","dead","Activity_time","experiment"))
UV_Table_long <- reshape2::melt(UV_Table_sub,id.vars = c("Species","Region","Strain","Sex" ,"Replicate","Activity_time","experiment"))

boxplot_male <- UV_Table_long %>% filter(Sex == 'male') %>%
  ggplot(aes_string(x = 'variable', y = 'value')) +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Region),size = 0.8)+
  geom_boxplot(outlier.shape = NA, alpha = 0.5) +
  theme_classic()+
  labs(y = 'number of flies', x = '') +
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))+
  facet_wrap(.~Activity_time)

boxplot_female <- UV_Table_long %>% filter(Sex == 'female') %>%
  ggplot(aes_string(x = 'variable', y = 'value')) +
  geom_boxplot(outlier.shape = NA ) +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Region),size = 0.8)+
  theme_classic()+
  labs(y = 'number of flies', x = '') +
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38"))+
  theme(axis.text.x = element_text(angle = 60,hjust = 1))+
  facet_wrap(.~Activity_time)

pdf(file.path(path,'supp_boxplots.pdf'),8,6)
print(boxplot_male)
print(boxplot_female)
dev.off()

# Line plot
lineplot_male <- UV_Table_long %>% filter(Sex == 'male') %>%
  ggplot(aes(x = Activity_time, y = value, color = Region, group = Region)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.2)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.2)) +
  labs(y = 'Average Number of Flies', x = 'Time in Hours', color = "Region") +
  scale_color_manual(values = c("#619CFF","#F8766D","#00BA38")) +
  ylim(0,5) +
  theme_classic() +
  facet_wrap(~variable, scales = "fixed", ncol = 4)

lineplot_female <- UV_Table_long %>% filter(Sex == 'female') %>%
  ggplot(aes(x = Activity_time, y = value, color = Region, group = Region)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.2)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.2, position = position_dodge(width = 0.2)) +
  labs(y = 'Average Number of Flies', x = 'Time in Hours', color = "Region") +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38")) +
  ylim(0,5) +
  theme_classic() +
  facet_wrap(~variable, scales = "fixed", ncol = 4)

combined_lineplot <- lineplot_male / lineplot_female

pdf(file.path(path,'combined_lineplots.pdf'),11,6)
print(combined_lineplot)
dev.off()

#Box plots real-time zap
df <- UV_Table %>% dplyr::select(-c(colnames(UV_Table)[6:17],"Temp_min","Temp_Max","Delta_Temp"))
df <- na.omit(df)
df_long <- reshape2::melt(df, id.vars = c("Species","Region","Strain","Sex" ,"Replicate","experiment"))
df_long$Region <- factor(df_long$Region,levels = c("Mainland_Dyak","Island_Dyak","Dsan"))

realtime_boxplot <- ggplot(df_long, aes_string(x = 'Region', y = 'value'))+
  geom_violin()+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Region),size = 0.8)+
  geom_boxplot(notch = TRUE, width = 0.3, outlier.shape = NA, alpha = 0)+
  theme_classic()+
  labs(y = element_text('Time (mins)'), x = '',) +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38"))+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 20))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 16),axis.text.y = element_text(size = 16), axis.title = element_text(size = 20))+
  #theme(legend.text = element_text(size = 10))
  facet_wrap(.~Sex)

#######
realtime_violinplot <- ggplot(df_long, aes_string(x = 'Region', y = 'value'))+
  geom_violin()+
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Region),size = 0.8)+
  theme_classic()+
  labs(y = element_text('Time (mins)'), x = '',) +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38"))+
  theme(axis.text.y = element_text(size = 16), axis.title = element_text(size = 20))+
  theme(axis.text.x = element_text(angle = 60,hjust = 1, size = 16),axis.text.y = element_text(size = 16), axis.title = element_text(size = 20))+
  facet_wrap(.~Sex)
#######

pdf(file.path(path,'Real-time_Zap_flies.pdf'),8,5)
print(realtime_boxplot)
dev.off()

#Wilcoxon Tests
df_long %>% filter(Sex == 'male') %>%
  rstatix::pairwise_wilcox_test(value ~ Region)

df_long %>% filter(Sex == 'female') %>%
  rstatix::pairwise_wilcox_test(value ~ Region)

# Construct table with mean per replicate
UV_Table_all$FellMean <- rowMeans(UV_Table_all[, c(16:20)], na.rm=TRUE)

# Region
summary(aov(FellMean ~ Region,data = UV_Table_all))
Region_ANOVA = aov(FellMean ~ Region,data = UV_Table_all)
summary(Region_ANOVA)
TukeyHSD(Region_ANOVA)
ggplot(UV_Table_all, aes(x = Region, FellMean, colour = Region))+
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Region),size = 0.8)+
  theme_classic() +
  ggtitle("Fly UV Knockout Time by Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(y = element_text('Time (minutes)'), x = '',) +
  scale_color_manual(values = c("#619CFF", "#F8766D", "#00BA38"))

# Sex
summary(aov(FellMean ~ Sex,data = UV_Table_all))
Sex_ANOVA = aov(FellMean ~ Sex,data = UV_Table_all)
summary(Sex_ANOVA)
TukeyHSD(Sex_ANOVA)
ggplot(UV_Table_all, aes(x = Sex, FellMean, colour = Sex))+
  geom_boxplot() +
  geom_jitter(position = position_jitter(width = 0.1), alpha = 0.7,aes(color = Sex),size = 0.8)+
  theme_classic() +
  ggtitle("Fly UV Knockout Time by Sex") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(y = element_text('Time (minutes)'), x = '',) +
  scale_color_manual(values = c( "#F8766D", "#619CFF"))

# Sex and Region
summary(aov(FellMean ~ Sex * Region,data = UV_Table_all))
Sex_Region_ANOVA = aov(FellMean ~ Sex * Region, data = UV_Table_all)
summary(Sex_Region_ANOVA)
TukeyHSD(Sex_Region_ANOVA)
ggplot(UV_Table_all, aes(x = Region, FellMean, colour = Sex))+
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.75), alpha = 0.7,aes(color = Sex),size = 0.8)+
  theme_classic() +
  ggtitle("Fly UV Knockout Time by Sex and Region") +
  theme(plot.title = element_text(hjust = 0.5, size = 20)) +
  labs(y = element_text('Time (minutes)'), x = '',) +
  scale_color_manual(values = c( "#F8766D", "#619CFF"))

# Variance test on Fall time outside of ANOVA replicates
# Reshape the data from wide to long format
UV_Table_sex_pop <- UV_Table_all %>%
  gather(key = "FellTime", value = "FellValue", `1st_felled`:`5th_felled`)

# Perform the F-test on the variances between sexes
# Group by 'Sex' and run the F-test on the 'FellValue' variable
f_test_sex <- var.test(FellValue ~ Sex, data = UV_Table_sex_pop)

# Print result
print(f_test_sex)

# Perform Levene's Test between populations [Region factor]
levene_test_region <- leveneTest(FellValue ~ Region, data = UV_Table_sex_pop)

# Print the result
print(levene_test_region)

# Perform Bartlett's Test between populations [Region factor]
bartlett_test_region <- bartlett.test(FellValue ~ Region, data = UV_Table_sex_pop)

# Print the result
print(bartlett_test_region)


