#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#              Load Libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

library('boot')
library(reshape2)
library(statsr)
library(dplyr)
require(visreg)
library(car)
require(plm)
#install.packages("formattable")
library("formattable")
library(car)
require(car)
#install.packages("data.table")
library(data.table)
library(ggplot2)
require(lmtest)
require(plm)
library(gridExtra)
require(methods)
library(stargazer)
# install.packages("corrplot")
# install.packages("car")
library(corrplot)
library(car)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#                Load Data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

films= read.csv("Films.csv") 
attach(films) 
imdb_films<-films[, c('imdb_score', 
                      'genre_action', 
                      'genre_animation', 
                      'genre_comedy', 
                      'genre_documentary', 
                      'genre_family', 
                      'genre_filmnoir', 
                      'genre_horror', 
                      'genre_musical', 
                      'genre_scifi', 
                      'genre_sport', 
                      'genre_war', 
                      'main_actor1_name', 
                      'main_actor2_name', 
                      'main_actor3_name', 
                      'total_number_of_actors', 
                      'main_director_is_female', 
                      'main_producer_name', 
                      'editor_name', 
                      'total_number_of_production_companies', 
                      'total_number_of_production_countries', 
                      'budget_in_millions', 
                      'year_of_release', 
                      'main_lang', 
                      'genre_adventure', 
                      'genre_biography', 
                      'genre_crime', 
                      'genre_drama', 
                      'genre_fantasy', 
                      'genre_history', 
                      'genre_music', 
                      'genre_mystery', 
                      'genre_romance', 
                      'genre_thriller', 
                      'genre_western', 
                      'main_actor1_is_female', 
                      'main_actor2_is_female', 
                      'main_actor3_is_female', 
                      'main_director_name', 
                      'total_number_of_directors', 
                      'total_number_of_producers', 
                      'main_production_company', 
                      'main_production_country', 
                      'month_of_release', 
                      'duration_in_hours', 
                      'total_number_languages' 
)] 

imdb_films$total_number_of_genres=imdb_films$genre_action +imdb_films$genre_animation +imdb_films$genre_comedy+ imdb_films$genre_documentary + imdb_films$genre_family  + imdb_films$genre_filmnoir +imdb_films$genre_horror + imdb_films$genre_musical + imdb_films$genre_scifi + imdb_films$genre_sport +imdb_films$genre_war +  imdb_films$genre_adventure + imdb_films$genre_biography + imdb_films$genre_crime + imdb_films$genre_drama +imdb_films$genre_fantasy + imdb_films$genre_history + imdb_films$genre_music + imdb_films$genre_mystery +  imdb_films$genre_romance + imdb_films$genre_thriller + imdb_films$genre_western 
attach(imdb_films) 

imdb_numeric = imdb_films[c('imdb_score', 
                            'total_number_of_actors', 
                            'total_number_of_production_companies', 
                            'total_number_of_production_countries', 
                            'budget_in_millions', 
                            'year_of_release', 
                            'total_number_of_directors', 
                            'total_number_of_producers', 
                            'month_of_release', 
                            'duration_in_hours', 
                            'total_number_languages')] 
imdb_categorical<-films[,c('genre_action',
                           'genre_animation',
                           'genre_comedy',
                           'genre_documentary',
                           'genre_family',
                           'genre_filmnoir',
                           'genre_horror',
                           'genre_musical',
                           'genre_scifi',
                           'genre_sport',
                           'genre_war',
                           'main_actor1_name',
                           'main_actor2_name',
                           'main_actor3_name',
                           'main_producer_name',
                           'editor_name',
                           'main_lang',
                           'genre_adventure',
                           'genre_biography',
                           'genre_crime',
                           'genre_drama',
                           'genre_fantasy',
                           'genre_history',
                           'genre_music',
                           'genre_mystery',
                           'genre_romance',
                           'genre_thriller',
                           'genre_western',
                           'main_actor1_is_female',
                           'main_actor2_is_female',
                           'main_actor3_is_female',
                           'main_director_name',
                           'main_production_company',
                           'main_production_country',
                           'main_director_is_female')]



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#   bar graph for the categorical columns 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

windows(width=50,height=50) 
par(mfrow=c(5,7)) 
axis(1, at = 0:1) 
for(i in 1:ncol(imdb_categorical)){ 
  counts<-table((imdb_categorical)[i]) 
  barplot(counts,xlab=names(imdb_categorical)[i],col="light green",label=TRUE,plot = TRUE) 
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   
#   histogram for the continuous columns 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#  

windows(width=50,height=50) 
par(mfrow=c(4,3)) 

for(i in 1:ncol(imdb_numeric)){ 
  hist(imdb_numeric[[i]] ,main="Histogram",xlab=names(imdb_numeric)[i],col="#E0FFFF",label=TRUE,plot = TRUE) 
} 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#    Boxplot for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

par(mfrow=c(2,6)) 

for(i in 1:11){ 
  boxplot((imdb_numeric)[i], xlab=names(imdb_numeric)[i], col = 'light blue') 
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Correlation Matrix for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 


cor_mat = cor(imdb_numeric) 
#view(cor_mat) 
#corrplot(cor_mat) 
corrplot(cor_mat, method = 'number') 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#       VIF for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

model = lm(imdb_score ~ .,data=imdb_numeric) 
summary(model)  
vif(model) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#   Scatter plot for continuous variables 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
x<-list()
y<-list()
z<-list()
par(mfrow=c(3,4))
for(i in 1:ncol(imdb_numeric))
{ y[[i]]<-names(imdb_numeric)[i]
mreg_p1_2=lm(imdb_numeric$imdb_score~imdb_numeric[[i]],data=imdb_numeric)
x[[i]]<-summary(mreg_p1_2)$adj.r.squared
z[[i]]<-summary(mreg_p1_2)[[4]][8]
plot(predict(mreg_p1_2), residuals(mreg_p1_2),ylab='Residuals',xlab=names(imdb_numeric)[i], col="#f56042")
abline(0,0, lty=2)}
adj_r_sq_lm<-do.call(rbind, Map(data.frame, A=y, B=x,C=z))
colnames(adj_r_sq_lm) <- c("Name", "Adjusted R Squared","p-value")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#       Linear model analysis for each variable w.r.t imdb_score
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
for(i in 1:ncol(imdb_films))
{ y[[i]]<-names(imdb_films)[i]
mreg_p1_2=lm(imdb_films$imdb_score~imdb_films[[i]],data=imdb_films)
x[[i]]<-summary(mreg_p1_2)$adj.r.squared
z[[i]]<-summary(mreg_p1_2)[[4]][8]
}
adj_r_sq_lm<-do.call(rbind, Map(data.frame, A=y, B=x,C=z))
colnames(adj_r_sq_lm) <- c("Name", "Adjuster R Squared","p-value")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#    heteroskedasticity analysis for each variable w.r.t imdb_score
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
names<-list()
with_h<-list()
without_h<-list()
issignificant<-list()
for(i in 1:ncol(imdb_films))
{ mreg_p1_2=lm(imdb_films$imdb_score~imdb_films[[i]],data=imdb_films)
names[[i]]<-names(imdb_films)[i]
with_h[[i]]<-ncvTest(mreg_p1_2)$p
without_h[[i]]<-coeftest(mreg_p1_2, vcov=vcovHC(mreg_p1_2, type="HC1"))[[8]]
issignificant[[i]]<-without_h[[i]]<0.05
}
hsk<-do.call(rbind, Map(data.frame, A=names, B=with_h, C=without_h,D=issignificant))
colnames(hsk) <- c("Name", "With heteroskedasticity","Without heteroskedasticity","is significant?")

#To select only those predictors which are significant after removing heteroskedasticity
hsk%>%filter(`is significant?`==TRUE)%>%select(Name)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#                               Outlier Removal
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
imdb_films=imdb_films[-c(633,895,2610,2045,526,575,2718,2310,756,446,2816,1382,439,1515,1167,1915,1901),]
#imdb_films=imdb_films[-c(633,895,2610,2045,526,575,2718,2310,756,446,2816,1382,439,1515,1167,1915,1901,553,783,2216,1868,1474,1988,1992,1982,1955,2062,878,264,955),]#1461,909,432,683,882,1560,114,1154,2094,2681),] 

attach(imdb_films) 

library(data.table) 

top_list_main_producer_name = data.frame(sort(table(imdb_films$main_producer_name),decreasing=TRUE))[1:7,c(1)] 
top_list_main_director_name = data.frame(sort(table(imdb_films$main_director_name),decreasing=TRUE))[1:8,c(1)] 
top_list_editor_name = data.frame(sort(table(imdb_films$editor_name),decreasing=TRUE))[1:3,c(1)] 
top_list_main_actor1_name = data.frame(sort(table(imdb_films$main_actor1_name),decreasing=TRUE))[1:6,c(1)] 
top_list_main_lang = data.frame(sort(table(imdb_films$main_lang),decreasing=TRUE))[1:3,c(1)] 
top_list_main_production_company = data.frame(sort(table(imdb_films$main_production_company),decreasing=TRUE))[1:6,c(1)] 
top_list_main_production_country = data.frame(sort(table(imdb_films$main_production_country),decreasing=TRUE))[1:4,c(1)] 
top_list_total_number_of_directors = data.frame(sort(table(imdb_films$total_number_of_directors),decreasing=TRUE))[1:2,c(1)] 
top_list_total_number_languages = data.frame(sort(table(imdb_films$total_number_languages),decreasing=TRUE))[1:2,c(1)] 

imdb_films$month_of_release = ifelse(imdb_films$month_of_release>8,'Winter','Non Winter') 
imdb_films$main_producer_name = ifelse(imdb_films$main_producer_name %in% top_list_main_producer_name,paste(imdb_films$main_producer_name), 'Other') 
imdb_films$editor_name = ifelse(imdb_films$editor_name %in% top_list_editor_name,paste(imdb_films$editor_name), 'Other') 
imdb_films$main_director_name = ifelse(imdb_films$main_director_name %in% top_list_main_director_name,paste(imdb_films$main_director_name), 'Other') 
imdb_films$main_actor1_name = ifelse(imdb_films$main_actor1_name %in% top_list_main_actor1_name,paste(imdb_films$main_actor1_name), 'Other') 
imdb_films$main_lang = ifelse(imdb_films$main_lang %in% top_list_main_lang,paste(imdb_films$main_lang), 'Other') 
imdb_films$main_production_company = ifelse(imdb_films$main_production_company %in% top_list_main_production_company,paste(imdb_films$main_production_company), 'Other') 

imdb_films$main_production_country = ifelse(imdb_films$main_production_country %in% top_list_main_production_country,paste(imdb_films$main_production_country), 'Other') 
imdb_films$total_number_of_directors = ifelse(imdb_films$total_number_of_directors %in% top_list_total_number_of_directors,paste(imdb_films$total_number_of_directors), 'Other') 
imdb_films$total_number_languages = ifelse(imdb_films$total_number_languages %in% top_list_total_number_languages,paste(imdb_films$total_number_languages), 'Other') 
attach(imdb_films) 

factorCols <- c('genre_action',  
                'genre_animation', 
                'genre_comedy',  
                'genre_documentary',  
                'genre_family',  
                'genre_filmnoir', 
                'genre_horror',  
                'month_of_release', 
                'genre_musical',  
                'genre_scifi',  
                'genre_sport',  
                'genre_war',  
                'main_actor1_name', 
                'main_actor2_name',  
                'main_actor3_name',  
                'editor_name',  
                'genre_adventure',  
                'genre_biography',  
                'genre_crime',  
                'genre_drama',  
                'genre_fantasy',  
                'genre_history',  
                'genre_music',  
                'genre_mystery',  
                'genre_romance',  
                'genre_thriller',  
                'genre_western',  
                'main_actor1_is_female',  
                'main_actor2_is_female',  
                'main_actor3_is_female',  
                'main_director_name',  
                'main_production_company',  
                'main_production_country',  
                'main_director_is_female',  
                'total_number_of_directors',  
                'total_number_languages',  
                'month_of_release',  
                'total_number_of_producers',  
                'total_number_of_production_companies',  
                'total_number_of_production_countries',  
                'main_producer_name',  
                'main_lang' 
)  

for (factorCol in factorCols) { 
  imdb_films[, factorCol] <- as.factor(imdb_films[, factorCol]) 
} 
attach(imdb_films) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#                     Non-linearity polynomial tests
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
############## Non-linearity analysis for number of actors #################

regdegree1_numberOfActors=lm(imdb_numeric$imdb_score~imdb_numeric$total_number_of_actors) 
regdegree2_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,2)) 
regdegree3_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,3)) 
regdegree4_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,4)) 
regdegree5_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,5)) 
regdegree6_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,6)) 
regdegree7_numberOfActors=lm(imdb_numeric$imdb_score~poly(imdb_numeric$total_number_of_actors,7)) 

names(regdegree2_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2') 
names(regdegree3_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2','total_number_of_actors3') 
names(regdegree4_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2','total_number_of_actors3','total_number_of_actors4') 
names(regdegree5_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2','total_number_of_actors3','total_number_of_actors4','total_number_of_actors5') 
names(regdegree6_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2','total_number_of_actors3','total_number_of_actors4','total_number_of_actors5','total_number_of_actors6') 
names(regdegree7_numberOfActors$coefficients) <- c('Constant','total_number_of_actors','total_number_of_actors2','total_number_of_actors3','total_number_of_actors4','total_number_of_actors5','total_number_of_actors6','total_number_of_actors7') 

stargazer(regdegree2_numberOfActors, regdegree3_numberOfActors, regdegree4_numberOfActors,regdegree5_numberOfActors,regdegree6_numberOfActors,regdegree7_numberOfActors, 
          type="html", align=TRUE, dep.var.labels=c("IMDB Rating (based on non-linear number of actors)"), 
          covariate.labels=c("Intercept","Number of Actors","Number of Actors^{2}","Number of Actors^{3}","Number of Actors^{4}","Number of Actors^{5}","Number of Actors^{6}","Number of Actors^{7}"),  
          omit.stat=c("LL","ser","f"),intercept.bottom=FALSE,no.space=TRUE) 

numberOfActors_degree2_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,2), se=F, aes(color="Degree 2"))+scale_color_manual(name="Degree",values=c("Degree 2"="red"))+theme(legend.position = c(0.9,0.92)) 
numberOfActors_degree3_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,3), se=F, aes(color="Degree 3"))+scale_color_manual(name="Degree",values=c("Degree 3"="red"))+theme(legend.position = c(0.9,0.92)) 
numberOfActors_degree4_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,4), se=F, aes(color="Degree 4"))+scale_color_manual(name="Degree",values=c("Degree 4"="red"))+theme(legend.position = c(0.9,0.92)) 
numberOfActors_degree5_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,5), se=F, aes(color="Degree 5"))+scale_color_manual(name="Degree",values=c("Degree 5"="red"))+theme(legend.position = c(0.9,0.92)) 
numberOfActors_degree6_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,6), se=F, aes(color="Degree 6"))+scale_color_manual(name="Degree",values=c("Degree 6"="red"))+theme(legend.position = c(0.9,0.92)) 
numberOfActors_degree7_plot=ggplot(imdb_numeric,aes(x=total_number_of_actors,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,7), se=F, aes(color="Degree 7"))+scale_color_manual(name="Degree",values=c("Degree 7"="red"))+theme(legend.position = c(0.9,0.92)) 
grid.arrange(numberOfActors_degree2_plot, numberOfActors_degree3_plot, numberOfActors_degree4_plot,numberOfActors_degree5_plot, numberOfActors_degree6_plot, numberOfActors_degree7_plot, nrow=3, ncol=2) 

anova(regdegree1_numberOfActors,regdegree2_numberOfActors, regdegree3_numberOfActors, regdegree4_numberOfActors, regdegree5_numberOfActors, regdegree6_numberOfActors, regdegree7_numberOfActors) 
# Non-linearity analysis of budget 
regdegree1_budget=lm(imdb_numeric$imdb_score~imdb_numeric$budget_in_millions) 
regdegree2_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,2)) 
regdegree3_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,3)) 
regdegree4_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,4)) 
regdegree5_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,5)) 
regdegree6_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,6)) 
regdegree7_budget=lm(imdb_numeric$imdb_score~poly(imdb_numeric$budget_in_millions,7)) 

names(regdegree2_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2') 
names(regdegree3_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2','budget_in_millions3') 
names(regdegree4_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2','budget_in_millions3','budget_in_millions4') 
names(regdegree5_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2','budget_in_millions3','budget_in_millions4','budget_in_millions5') 
names(regdegree6_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2','budget_in_millions3','budget_in_millions4','budget_in_millions5','budget_in_millions6') 
names(regdegree7_budget$coefficients) <- c('Constant','budget_in_millions','budget_in_millions2','budget_in_millions3','budget_in_millions4','budget_in_millions5','budget_in_millions6','budget_in_millions7') 

stargazer(regdegree2_budget, regdegree3_budget, regdegree4_budget,regdegree5_budget,regdegree6_budget,regdegree7_budget, 
          type="html", align=TRUE, dep.var.labels=c("IMDB Rating (based on non-linear Budget)"), 
          covariate.labels=c("Intercept","Budget","Budget^{2}","Budget^{3}","Budget^{4}","Budget^{5}","Budget^{6}","Budget^{7}"),  
          omit.stat=c("LL","ser","f"),intercept.bottom=FALSE,no.space=TRUE) 

budget_degree2_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,2), se=F, aes(color="Degree 2"))+scale_color_manual(name="Degree",values=c("Degree 2"="red"))+theme(legend.position = c(0.9,0.92)) 
budget_degree3_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,3), se=F, aes(color="Degree 3"))+scale_color_manual(name="Degree",values=c("Degree 3"="red"))+theme(legend.position = c(0.9,0.92)) 
budget_degree4_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,4), se=F, aes(color="Degree 4"))+scale_color_manual(name="Degree",values=c("Degree 4"="red"))+theme(legend.position = c(0.9,0.92)) 
budget_degree5_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,5), se=F, aes(color="Degree 5"))+scale_color_manual(name="Degree",values=c("Degree 5"="red"))+theme(legend.position = c(0.9,0.92)) 
budget_degree6_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,6), se=F, aes(color="Degree 6"))+scale_color_manual(name="Degree",values=c("Degree 6"="red"))+theme(legend.position = c(0.9,0.92)) 
budget_degree7_plot=ggplot(imdb_numeric,aes(x=budget_in_millions,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,7), se=F, aes(color="Degree 7"))+scale_color_manual(name="Degree",values=c("Degree 7"="red"))+theme(legend.position = c(0.9,0.92)) 
grid.arrange(budget_degree2_plot, budget_degree3_plot, budget_degree4_plot,budget_degree5_plot, budget_degree6_plot, budget_degree7_plot, nrow=3, ncol=2) 

anova(regdegree1_budget,regdegree2_budget, regdegree3_budget, regdegree4_budget, regdegree5_budget, regdegree6_budget, regdegree7_budget) 
# Non-linearity test for duration of movie 
regdegree1_duration_in_hours=lm(imdb_numeric$imdb_score~imdb_numeric$duration_in_hours) 
regdegree2_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,2)) 
regdegree3_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,3)) 
regdegree4_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,4)) 
regdegree5_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,5)) 
regdegree6_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,6)) 
regdegree7_duration_in_hours=lm(imdb_numeric$imdb_score~poly(imdb_numeric$duration_in_hours,7)) 

names(regdegree2_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2') 
names(regdegree3_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2','duration_in_hours3') 
names(regdegree4_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2','duration_in_hours3','duration_in_hours4') 
names(regdegree5_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2','duration_in_hours3','duration_in_hours4','duration_in_hours5') 
names(regdegree6_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2','duration_in_hours3','duration_in_hours4','duration_in_hours5','duration_in_hours6') 
names(regdegree7_duration_in_hours$coefficients) <- c('Constant','duration_in_hours','duration_in_hours2','duration_in_hours3','duration_in_hours4','duration_in_hours5','duration_in_hours6','duration_in_hours7') 

stargazer(regdegree2_duration_in_hours, regdegree3_duration_in_hours, regdegree4_duration_in_hours,regdegree5_duration_in_hours,regdegree6_duration_in_hours,regdegree7_duration_in_hours, 
          type="html", align=TRUE, dep.var.labels=c("IMDB Rating (based on non-linear duration_in_hours)"), 
          covariate.labels=c("Intercept","duration_in_hours","duration_in_hours^{2}","duration_in_hours^{3}","duration_in_hours^{4}","duration_in_hours^{5}","duration_in_hours^{6}","duration_in_hours^{7}"),  
          omit.stat=c("LL","ser","f"),intercept.bottom=FALSE,no.space=TRUE) 

duration_in_hours_degree2_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,2), se=F, aes(color="Degree 2"))+scale_color_manual(name="Degree",values=c("Degree 2"="red"))+theme(legend.position = c(0.9,0.92)) 
duration_in_hours_degree3_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,3), se=F, aes(color="Degree 3"))+scale_color_manual(name="Degree",values=c("Degree 3"="red"))+theme(legend.position = c(0.9,0.92)) 
duration_in_hours_degree4_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,4), se=F, aes(color="Degree 4"))+scale_color_manual(name="Degree",values=c("Degree 4"="red"))+theme(legend.position = c(0.9,0.92)) 
duration_in_hours_degree5_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,5), se=F, aes(color="Degree 5"))+scale_color_manual(name="Degree",values=c("Degree 5"="red"))+theme(legend.position = c(0.9,0.92)) 
duration_in_hours_degree6_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,6), se=F, aes(color="Degree 6"))+scale_color_manual(name="Degree",values=c("Degree 6"="red"))+theme(legend.position = c(0.9,0.92)) 
duration_in_hours_degree7_plot=ggplot(imdb_numeric,aes(x=duration_in_hours,y=imdb_score))+geom_point(color="grey")+geom_smooth(method="lm", formula=y~poly(x,7), se=F, aes(color="Degree 7"))+scale_color_manual(name="Degree",values=c("Degree 7"="red"))+theme(legend.position = c(0.9,0.92)) 
grid.arrange(duration_in_hours_degree2_plot, duration_in_hours_degree3_plot, duration_in_hours_degree4_plot,duration_in_hours_degree5_plot, duration_in_hours_degree6_plot, duration_in_hours_degree7_plot, nrow=3, ncol=2) 

anova(regdegree1_duration_in_hours,regdegree2_duration_in_hours, regdegree3_duration_in_hours, regdegree4_duration_in_hours, regdegree5_duration_in_hours, regdegree6_duration_in_hours, regdegree7_duration_in_hours) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#                       Interactions Terms Testing 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

############~~~~~~Interaction Term 1~~~~~~~~##################### 

mreg5=lm(imdb_score~total_number_of_actors+genre_drama+total_number_of_actors*genre_drama) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=total_number_of_actors, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama and number of actors interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

############~~~~~~Interaction Term 2~~~~~~~~##################### 

mreg5=lm(imdb_score~duration_in_hours+genre_drama+duration_in_hours*genre_drama ) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=duration_in_hours 
     , y=imdb_score, col=ifelse("red", "blue"), main="genre_drama and duration of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 


############~~~~~~~Interaction Term 3~~~~~~~##################### 

mreg5=lm(imdb_score~genre_drama+genre_romance+genre_drama*genre_romance) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=genre_drama 
     , y=imdb_score, col=ifelse("red", "blue"), main="genre_drama and genre_romance of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

############~~~~~~Interaction Term 4~~~~~~~~##################### 

mreg5=lm(imdb_score~genre_thriller+genre_drama+genre_drama*genre_thriller) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4]  

plot(x=genre_thriller, y=imdb_score, col=ifelse("red", "blue"),  main="genre_drama and genre_thriller of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

############~~~~~~~~Interaction Term 5~~~~~~##################### 

mreg5=lm(imdb_score~genre_drama+genre_horror+genre_drama*genre_horror) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4]  

plot(x=genre_drama, y=imdb_score, col=ifelse("red", "blue")) 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~~Interaction Term 6~~~~~~~######################## 

mreg5=lm(imdb_score~genre_animation+genre_fantasy+genre_animation*genre_fantasy) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4]  

plot(x=genre_animation, y=imdb_score, col=ifelse("red", "blue"), main="genre_animation and genre_fantasy of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~~Interaction Term 7~~~~~~~######################## 

mreg5=lm(imdb_score~genre_fantasy+genre_adventure+genre_fantasy*genre_adventure) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=genre_sport, y=imdb_score, col=ifelse("red", "blue"), main="genre_adventure and genre_fantasy of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~~Interaction Term 8~~~~~~~######################## 

mreg5=lm(imdb_score~year_of_release+genre_drama+year_of_release*genre_drama) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_drama and year of release of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~~Interaction Term 9~~~~~~~######################## 

mreg5=lm(imdb_score~year_of_release+genre_comedy+year_of_release*genre_comedy) 
summary(mreg5)  

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_comedy and year of release of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~Interaction Term 10~~~~~~~~######################## 

mreg5=lm(imdb_score~year_of_release+genre_action+year_of_release*genre_action) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=year_of_release, y=imdb_score, col=ifelse("red", "blue"), main="genre_action and year of release of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~Interaction Term 11~~~~~~~~######################## 

mreg5=lm(imdb_score~genre_drama+genre_action+genre_drama*genre_action) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=genre_action, y=imdb_score, col=ifelse("red", "blue"), main="genre_action and genre_drama interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~Interaction Term 12~~~~~~~~######################## 

mreg5=lm(imdb_score~genre_drama+genre_mystery+genre_drama*genre_mystery) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=genre_drama, y=imdb_score, col=ifelse("red", "blue"), main="genre_mystery and genre_drama interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~Interaction Term 13~~~~~~~~######################## 

mreg5=lm(imdb_score~genre_animation+budget_in_millions+genre_animation*budget_in_millions) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=budget_in_millions, y=imdb_score, col=ifelse("red", "blue"), main="genre_animation and budget of movie interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

###########~~~~~~Interaction Term 14~~~~~~~~######################## 

mreg5=lm(imdb_score~genre_family+genre_comedy+genre_family*genre_comedy) 
summary(mreg5) 

b0=coef(mreg5)[1] 
b1=coef(mreg5)[2] 
b2=coef(mreg5)[3] 
b3=coef(mreg5)[4] 

plot(x=genre_comedy, y=imdb_score, col=ifelse("red", "blue"), main="genre_comedy and genre_family interaction plot") 
abline(b0+b2,b1+b3, col= "red") 
abline(b0, b1, col= "blue") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#                             KNN - Model 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

set.seed(1) 
fit=glm(imdb_score~ 
          genre_action 
        +genre_documentary 
        +genre_family 
        +genre_filmnoir 
        +genre_horror 
        +poly(total_number_of_actors,2) 
        +main_producer_name 
        +poly(budget_in_millions,6) 
        +genre_drama 
        +genre_history 
        +genre_western 
        +main_actor1_is_female 
        +main_actor2_is_female 
        +main_actor3_is_female 
        +total_number_of_directors 
        +main_director_name 
        +main_production_country 
        +total_number_of_genres 
        +month_of_release 
        +poly(duration_in_hours,4) 
        +(total_number_of_actors*genre_drama) 
        +(duration_in_hours*genre_drama) 
        +(genre_drama*genre_romance)  
        +(genre_fantasy*genre_adventure) 
        +(year_of_release*genre_drama) 
        +(year_of_release*genre_comedy) 
        +(year_of_release*genre_action) 
        +(genre_drama*genre_mystery) 
        +(genre_animation*budget_in_millions) 
        +total_number_languages, 
        data=imdb_films ) 

summary(fit) 
cv.glm(imdb_films,fit,K=10)$delta[1] 


#######################################################################  

############################ Final Model ##############################  

#######################################################################  



model=lm(imdb_score~ 
           genre_action 
         +genre_documentary 
         +genre_family 
         +genre_filmnoir 
         +genre_horror 
         +poly(total_number_of_actors,2) 
         +main_producer_name 
         +poly(budget_in_millions,6) 
         +genre_drama 
         +genre_history 
         +genre_western 
         +main_actor1_is_female 
         +main_actor2_is_female 
         +main_actor3_is_female 
         +total_number_of_directors 
         +main_director_name 
         +main_production_country 
         +total_number_of_genres 
         +month_of_release 
         +poly(duration_in_hours,4) 
         +(total_number_of_actors*genre_drama) 
         +(duration_in_hours*genre_drama) 
         +(genre_drama*genre_romance)  
         +(genre_fantasy*genre_adventure) 
         +(year_of_release*genre_drama) 
         +(year_of_release*genre_comedy) 
         +(year_of_release*genre_action) 
         +(genre_drama*genre_mystery) 
         +(genre_animation*budget_in_millions) 
         +total_number_languages, 
         data=imdb_films )  





#################################################################################  

########################## Final Prediction #####################################  

#################################################################################  

films_pred = read.csv("C:/Users/Gautami Edara/OneDrive - McGill University/Fall Semester/MGSC 661 - Serpa/Midterm/Films.csv") 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#                             Transform Data 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#   

films_pred$total_number_of_genres=films_pred$genre_action +films_pred$genre_animation +films_pred$genre_comedy+ films_pred$genre_documentary + films_pred$genre_family  + films_pred$genre_filmnoir +films_pred$genre_horror + films_pred$genre_musical + films_pred$genre_scifi + films_pred$genre_sport +films_pred$genre_war +  films_pred$genre_adventure + films_pred$genre_biography + films_pred$genre_crime + films_pred$genre_drama +films_pred$genre_fantasy + films_pred$genre_history + films_pred$genre_music + films_pred$genre_mystery +  films_pred$genre_romance + films_pred$genre_thriller + films_pred$genre_western   

films_pred$main_lang = ifelse(films_pred$main_lang== 'English', 'English', ifelse(films_pred$main_lang== 'FranÃ§ais','French',ifelse(films_pred$main_lang=='Deutsch', 'Deutsch', 'Other')))  

films_pred$month_of_release = ifelse(films_pred$month_of_release>8,'Winter','Non Winter')   

films_pred$main_producer_name = ifelse(films_pred$main_producer_name %in% top_list_main_producer_name,paste(films_pred$main_producer_name), 'Other')   

films_pred$editor_name = ifelse(films_pred$editor_name %in% top_list_editor_name,paste(films_pred$editor_name), 'Other')   

films_pred$main_director_name = ifelse(films_pred$main_director_name %in% top_list_main_director_name,paste(films_pred$main_director_name), 'Other')   

films_pred$main_actor1_name = ifelse(films_pred$main_actor1_name %in% top_list_main_actor1_name,paste(films_pred$main_actor1_name), 'Other')   

films_pred$main_production_company = ifelse(films_pred$main_production_company %in% top_list_main_production_company,paste(films_pred$main_production_company), 'Other')   

films_pred$main_production_country = ifelse(films_pred$main_production_country %in% top_list_main_production_country,paste(films_pred$main_production_country), 'Other')   

films_pred$total_number_of_directors = ifelse(films_pred$total_number_of_directors %in% top_list_total_number_of_directors,paste(films_pred$total_number_of_directors), 'Other')   

films_pred$total_number_languages = ifelse(films_pred$total_number_languages %in% top_list_total_number_languages,paste(films_pred$total_number_languages), 'Other')   

films_pred[1,]  

attach(films_pred)  

factorCols <- c('genre_action',  
                'genre_animation', 
                'genre_comedy',  
                'genre_documentary',  
                'genre_family',  
                'genre_filmnoir', 
                'genre_horror',  
                'month_of_release', 
                'genre_musical',  
                'genre_scifi',  
                'genre_sport',  
                'genre_war',  
                'main_actor1_name', 
                'main_actor2_name',  
                'main_actor3_name',  
                'editor_name',  
                'genre_adventure',  
                'genre_biography',  
                'genre_crime',  
                'genre_drama',  
                'genre_fantasy',  
                'genre_history',  
                'genre_music',  
                'genre_mystery',  
                'genre_romance',  
                'genre_thriller',  
                'genre_western',  
                'main_actor1_is_female',  
                'main_actor2_is_female',  
                'main_actor3_is_female',  
                'main_director_name',  
                'main_production_company',  
                'main_production_country',  
                'main_director_is_female',  
                'total_number_of_directors',  
                'total_number_languages',  
                'month_of_release',  
                'total_number_of_producers',  
                'total_number_of_production_companies',  
                'total_number_of_production_countries',  
                'main_producer_name',  
                'main_lang' 
)  


for (factorCol in factorCols) {   
  
  films_pred[, factorCol] <- as.factor(films_pred[, factorCol])   
  
}   

films_pred$pred=predict(model, films_pred)  


View(films_pred)  


# films_pred$res=(films_pred$imdb_score-films_pred$pred) 
# films_pred$res_sq=(films_pred$res)^2 
# MSE=mean(films_pred$res_sq) 
# MSE 
#  

# view_films_pred<-films_pred[,c("imdb_score","pred")] 
# View(view_films_pred) 


##############
for (i in 1:40)
{
#smp_size <- floor(0.75 * nrow(imdb_films))## set the seed to make your partition reproducible
#set.seed(123)
#train_ind <- sample(seq_len(nrow(imdb_films)), size = smp_size)
imdb_films_train <- imdb_films[c(42:2995),]
imdb_films_test <- imdb_films[c(i),]
#attach(imdb_films)
#Better Adjusted R Squared
fit_numeric=lm(imdb_score~ 
           genre_action 
         +genre_documentary 
         +genre_family 
         +genre_filmnoir 
         +genre_horror 
         +poly(total_number_of_actors,2) 
         +main_producer_name 
         +poly(budget_in_millions,6) 
         +genre_drama 
         +genre_history 
         +genre_western 
         +main_actor1_is_female 
         +main_actor2_is_female 
         +main_actor3_is_female 
         +total_number_of_directors 
         +main_director_name 
         +main_production_country 
         +total_number_of_genres 
         +month_of_release 
         +poly(duration_in_hours,4) 
         +(total_number_of_actors*genre_drama) 
         +(duration_in_hours*genre_drama) 
         +(genre_drama*genre_romance)  
         +(genre_fantasy*genre_adventure) 
         +(year_of_release*genre_drama) 
         +(year_of_release*genre_comedy) 
         +(year_of_release*genre_action) 
         +(genre_drama*genre_mystery) 
         +(genre_animation*budget_in_millions) 
         +total_number_languages, 
         data=imdb_films )  


summary(fit_numeric)

imdb_films_test$pred=predict(fit_numeric, imdb_films_test)
print(round(imdb_films_test$pred,1))
}
#imdb_films_test$res=(imdb_films_test$imdb_score-imdb_films_test$pred)
#imdb_films_test$res_sq=(imdb_films_test$res)^2
#MSE=mean(imdb_films_test$res_sq)
#print(MSE)
#}
