library(shiny)
library(shinydashboard)
library(leaflet)
library(collapsibleTree)
library(shinycssloaders)
library(DT)
library(tigris)
library(data.table)
library(leaflet.extras)
library(dplyr)
library(plotly)
library(particlesjs)
library(rintrojs)
library(shinyjs)
library(rgeos)
library(sf)
library(rgdal)
library(dashboardthemes)
library(echarts4r)


#############################################################################################################################################################


# function that build the park card html pop up
park_card <- function (park_Name,wilaya,park_Latitude, park_Longitude,park_Longitude1,users) {
  
  card_content <- paste0(
    '<h1 style="color: #5e9ca0; text-align: center;">',park_Name,"</h1>",
    '<h2 style="color: #2e6c80;">Informations :</h2>',
    '<ul style="list-style-position: initial; list-style-image: initial; font-size: 14px; line-height: 32px; font-weight: bold;">',
    '<li style="clear: both;"><span style="color: #33cccc;">Wilaya</span> :', wilaya,'</li>',
    '<li style="clear: both;"><span style="color: #33cccc;">Nombre de candidats: </span>', park_Latitude,"</li>",
    '<li style="clear: both;"><span style="color: #33cccc;">Pourcentage des admis:</span>', users,'</li>',
    '<li style="clear: both;"><span style="color: #33cccc;">Pourcentage des sessionaires:</span>&nbsp;', park_Longitude,'</li>',
    '<li style="clear: both;"><span style="color: #33cccc;">Pourcentage de triche:</span>&nbsp;', park_Longitude1,'</li>',
    '</ul>',
    '<p>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp;</p>',
    '<p><strong>&nbsp;</strong></p>'
  )
  
  return(card_content)
  
}


##################
# DATA WRANGLING #
##################

# preprocessed parks file:
#   3 records were multi wilaya2_noms parks, only was was attributed
#     DEVA,Death Valley National Park,CA/NV,4740912,36.24,-116.82  --> CA
#     GRSM,Great Smoky Mountains National Park,TN/NC,521490,35.68,-83.53 --> TN
#     YELL,Yellowstone National Park,WY/MT/ID,2219791,44.6,-110.5 --> WY
#   added (U.S.) suffix to Glacier National Park record for wiki disambigaution



# tidy & enrich dataframes
parks <- readRDS("liste_des_ecoles_nktt.RDS")

# tidy & enrich dataframes
parks$name2 <- as.character(parks$Name)
parks$ParkName2<-paste0("Nouakchott:",parks$name2)
parks$wilaya2_nom<-parks$ADM2_EN
icon.school<-makeIcon(parks$icon[1],iconWidth = 32,iconHeight = 32)



################

candidate_details<-readRDS('bd_rimbac.RDS')
decoupage<-readRDS("last_shp_file_ecoupage.RDS")


candidate_details$ajourne<-ifelse(candidate_details$Decision=='Ajourné',1,0)
candidate_details$abscent<-ifelse(candidate_details$Decision=='Abscent',1,0)



summary_wilaya_seul <- candidate_details%>% group_by(Willaya) %>% summarize(admi =sum(admis),candidats=sum(tot)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =3))

summary_serie_seul <- candidate_details%>% group_by(Serie.x) %>% summarize(admi =sum(admis),candidats=sum(tot),triches=sum(triche),sesionnaires=sum(session),ajournes=sum(ajourne)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =3)*100,taux_triche=round(triches/candidats,digits =3)*100,
         taux_echec=round(ajournes/candidats,digits =3)*100,taux_sesionnaires=round(sesionnaires/candidats,digits =3)*100)



#The default order will be alphabetized unless specified as below:
summary_wilaya_seul$Willaya <- factor(summary_wilaya_seul$Willaya, levels = summary_wilaya_seul$Willaya[order(summary_wilaya_seul$candidats,decreasing = T)])
summary_serie_seul$Serie.x <- factor(summary_serie_seul$Serie.x, levels = summary_serie_seul$Serie.x[order(summary_serie_seul$taux_reussite,decreasing = T)])



# Nombre de candidats par wilaya 
candidate_details$Willaya<-as.factor(candidate_details$Willaya)


summary_wilaya <- candidate_details%>% group_by(Willaya,etablissement ) %>% summarize(admi =sum(admis),candidats=sum(tot)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =2))

summary_wilaya_2 <- summary_wilaya%>% group_by(Willaya) %>% summarize(nb =n())



summary_etablissement <- candidate_details %>% group_by(etablissement,nktt,Serie.x ) %>% summarize(admi =sum(admis),candidats=sum(tot)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =2))

summary_centre <- candidate_details %>% group_by(Centre,nktt,Serie.x ) %>% summarize(admi =sum(admis),candidats=sum(tot)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =2))


summary_mouqataa_seul <- candidate_details %>% group_by(moughataa ) %>% summarize(admi =sum(admis),candidats=sum(tot),ajournes=sum(ajourne)) %>% 
  mutate(taux_reussite=round(admi/candidats,digits =2)*100, taux_echec=round(ajournes/candidats,digits =3)*100)

summary_mouqataa_seul2<-summary_mouqataa_seul


Nb_candidats_Total<-length(unique(candidate_details$NumDossier))
Nb_candidats_M<-sum(filter(summary_serie_seul,Serie.x=='M')$candidats)
Nb_candidats_SN<-sum(filter(summary_serie_seul,Serie.x=='SN')$candidats)
Nb_candidats_TM<-sum(filter(summary_serie_seul,Serie.x=='TM')$candidats)
Nb_candidats_LO<-sum(filter(summary_serie_seul,Serie.x=='LO')$candidats)
Nb_candidats_LM<-sum(filter(summary_serie_seul,Serie.x=='LM')$candidats)



Taux_admis<-round(sum(summary_serie_seul$admi)/sum(summary_serie_seul$candidats),digits=3)*100
Taux_Sessionnaire<-round(sum(summary_serie_seul$sesionnaires)/sum(summary_serie_seul$candidats),digits=3)*100
Taux_triche<-round(sum(summary_serie_seul$triches)/sum(summary_serie_seul$candidats),digits=3)*100


Nb_admis<-sum(summary_serie_seul$admi)
Nb_Sessionnaire<-sum(summary_serie_seul$sesionnaires)
Nb_triche<-sum(summary_serie_seul$triches)



Nb_candidats_NKTT<-sum(filter(candidate_details,nktt!='Dakhle')$tot)
Tx_candidats_NKTT<-round(sum(filter(candidate_details,nktt!='Dakhle')$tot)/sum(candidate_details$tot),digits=3)*100


Nb_candidats_LM<-sum(filter(summary_serie_seul,Serie.x=='LM')$candidats)


Age_moyen<-round(mean(candidate_details$age),digits = 0)
Age_min<-min(candidate_details$age)
Age_max<-max(candidate_details$age)


Nb_etablissement<-length(unique(summary_etablissement$etablissement))
Nb_etablissement_nktt<-length(unique(summary_etablissement[summary_etablissement$nktt!='Dakhle',]$etablissement))
Nb_etablissement_other<-length(unique(summary_etablissement[summary_etablissement$nktt=='Dakhle',]$etablissement))
TX_etablissement_nktt<-round((Nb_etablissement_nktt/Nb_etablissement)*100,digits = 0)


Nb_centre<-length(unique(summary_centre$Centre))
Nb_centre_nktt<-length(unique(summary_centre[summary_centre$nktt!='Dakhle',]$Centre))
Nb_centre_other<-length(unique(summary_centre[summary_centre$nktt=='Dakhle',]$Centre))
TX_centre_nktt<-round((Nb_centre_nktt/Nb_centre)*100,digits = 0)



note_moyen<-round(mean(as.numeric(as.character(candidate_details$moyeneGeneral)),na.rm = T),digits = 0)
note_min<-min(as.numeric(as.character(candidate_details$moyeneGeneral)))
note_max<-round(max(as.numeric(as.character(candidate_details$moyeneGeneral))),digits = 2)


#################################################################################################



summary_serie_seul$taux_echec<-summary_serie_seul$taux_echec+summary_serie_seul$taux_triche


summary_serie_seul$Serie.x <- factor(summary_serie_seul$Serie.x, levels = summary_serie_seul$Serie.x[order(summary_serie_seul$taux_echec,decreasing = F)])


summary_wilaya_seul2<-summary_wilaya_seul




#The default order will be alphabetized unless specified as below:
summary_wilaya_seul$Willaya <- factor(summary_wilaya_seul$Willaya, levels = summary_wilaya_seul$Willaya[order(summary_wilaya_seul$candidats,decreasing = T)])
summary_serie_seul$Serie.x <- factor(summary_serie_seul$Serie.x, levels = summary_serie_seul$Serie.x[order(summary_serie_seul$taux_reussite,decreasing = T)])
summary_wilaya_seul2$Willaya <- factor(summary_wilaya_seul2$Willaya, levels = summary_wilaya_seul2$Willaya[order(summary_wilaya_seul2$taux_reussite,decreasing = T)])

pays=c("Maroc",
       "Senegal",
       "Guinée Bisau",
       "Mali",
       "Algerie",
       "Tunisie",
       "Cote d'ivoire",
       "Mauritanie",
       "Tchad")

Taux_admis<-c(81.3,
              45.27,
              25.36,
              29.24,
              61.17,
              57.52,
              41.23,
              8.3,
              45.6)


df<-cbind.data.frame(pays=pays,Taux_admis=Taux_admis)
df$pays<-as.factor(df$pays)
df$pays <- factor(df$pays, levels = df$pays[order(df$Taux_admis,decreasing = F)])
mr<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/mr.png')
ml<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/ml.png')
td<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/td.png')
ci<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/ci.png')
gw<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/gw.png')
dz<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/dz.png')
tn<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/tn.png')
sn<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/sn.png')
ma<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/ma.png')
bf<-paste0("image://",'https://raw.githubusercontent.com/mlemineb/Rimbac/master/www/bf.png')

flags<-c(ma,sn,gw,ml,dz,tn,ci,mr,td)
df$flag<-flags



##################################################################


candidate_details$moyeneGeneral<-as.numeric(as.character(candidate_details$moyeneGeneral))


tab_wilaya <- candidate_details %>% group_by(Willaya,Serie.x ) %>% summarize(candidats=sum(tot),c_admis =sum(admis),sessionnaire=sum(session),
                                                                             triches=sum(triche),ajournes=sum(ajourne),abscents=sum(abscent)) %>% 
  mutate(taux_abscents=round(abscents/candidats),taux_admis=round(c_admis/candidats,digits =2),taux_sessionnaire=round(sessionnaire/candidats,digits =2),
         taux_triche=round(triches/candidats,digits =2),taux_ajourne=round(ajournes/candidats,digits =2))




tab_moukata <- candidate_details %>% group_by(moughataa,Serie.x ) %>% summarize(candidats=sum(tot),c_admis =sum(admis),sessionnaire=sum(session),
                                                                                triches=sum(triche),ajournes=sum(ajourne),abscents=sum(abscent)) %>% 
  mutate(taux_abscents=round(abscents/candidats),taux_admis=round(c_admis/candidats,digits =2),taux_sessionnaire=round(sessionnaire/candidats,digits =2),
         taux_triche=round(triches/candidats,digits =2),taux_ajourne=round(ajournes/candidats,digits =2))




tab_etab <- candidate_details %>% group_by(etablissement,Serie.x ) %>% summarize(candidats=sum(tot),c_admis =sum(admis),sessionnaire=sum(session),
                                                                                 triches=sum(triche),ajournes=sum(ajourne),abscents=sum(abscent)) %>% 
  mutate(taux_abscents=round(abscents/candidats),taux_admis=round(c_admis/candidats,digits =2),taux_sessionnaire=round(sessionnaire/candidats,digits =2),
         taux_triche=round(triches/candidats,digits =2),taux_ajourne=round(ajournes/candidats,digits =2))





tab_global <- candidate_details %>% group_by(Willaya,moughataa,etablissement,Serie.x ) %>% summarize(candidats=sum(tot),c_admis =sum(admis),sessionnaire=sum(session),
                                                                                                     triches=sum(triche),ajournes=sum(ajourne),abscents=sum(abscent),ages=mean(age),
                                                                                                     moyenneG=mean(as.numeric(as.character(moyeneGeneral))),
                                                                                                     note_1=mean(as.numeric(as.character(Note1))),
                                                                                                     note_2=mean(as.numeric(as.character(Note2))),
                                                                                                     note_3=mean(as.numeric(as.character(Note3))),
                                                                                                     note_4=mean(as.numeric(as.character(Note4))),
                                                                                                     note_5=mean(as.numeric(as.character(Note5))),
                                                                                                     note_6=mean(as.numeric(as.character(Note6))),
                                                                                                     note_7=mean(as.numeric(as.character(Note7))),
) %>% 
  mutate(taux_ajourne=round(ajournes/candidats,digits =2)*100,taux_admis=round(c_admis/candidats,digits =2)*100,taux_sessionnaire=round(sessionnaire/candidats,digits =2)*100,
         taux_triche=round(triches/candidats,digits =2)*100,taux_abscents=round(abscents/candidats)*100)

tab_global$moyenneG<-round(tab_global$moyenneG,digits = 2)
tab_global$ages<-round(tab_global$ages,digits = 0)

tab_global[c(13:19)]<-round(tab_global[c(13:19)], digits = 2)


tab_global<-tab_global[c(1:12,20:24,13:19)]

tab_global$taux_abscents<-paste0(as.character(tab_global$taux_abscents),"%")
tab_global$taux_admis<-paste0(as.character(tab_global$taux_admis),"%")
tab_global$taux_ajourne<-paste0(as.character(tab_global$taux_ajourne),"%")
tab_global$taux_sessionnaire<-paste0(as.character(tab_global$taux_sessionnaire),"%")
tab_global$taux_triche<-paste0(as.character(tab_global$taux_triche),"%")


m_willaya <- c("Toutes", as.character(sort(unique(tab_global$Willaya))))
m_mougahtaa <- c("Toutes", as.character(sort(unique(tab_global$moughataa))))
m_etablissement <- c("Toutes", as.character(sort(unique(tab_global$etablissement))))


tab_nktt<- candidate_details %>% group_by(nktt) %>% summarize(candidats=sum(tot),c_admis =sum(admis),sessionnaire=sum(session),
                                                              triches=sum(triche),ajournes=sum(ajourne),abscents=sum(abscent)) %>% 
  mutate(taux_abscents=round(abscents/candidats),taux_admis=round(c_admis/candidats,digits =2),taux_sessionnaire=round(sessionnaire/candidats,digits =2),
         taux_triche=round(triches/candidats,digits =2),taux_ajourne=round(ajournes/candidats,digits =2))



tab_nktt$nktt[1]<-"Autre Willaya"


summary_serie_seul <- summary_serie_seul %>%
  mutate(percent = round(candidats/sum(candidats), 2),
         name = paste(Serie.x, paste0(percent*100,'%',' des Candidats'), sep = ","))

summary_serie_seul2<-summary_serie_seul
summary_serie_seul2$taux_echec<-round(summary_serie_seul2$taux_echec,0)
summary_serie_seul2$taux_sesionnaires<-round(summary_serie_seul2$taux_sesionnaires,0)
summary_serie_seul2$taux_reussite<-round(summary_serie_seul2$taux_reussite,0)


markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> #Nombre de candidats </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "glyphicon") {
      legendHtml<- paste0(
        legendHtml, 
        "<div style='width: auto; height: 45px'>",
        "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker awesome-marker-icon-",
        Icon[["markerColor"]],
        "'>",
        "<i style='margin-left: 4px; margin-top: 11px; color: ",
        Icon[["iconColor"]],
        "' class= 'glyphicon glyphicon-",
        Icon[["icon"]],
        "'></i>",
        "</div>",
        "<p style='position: relative; top: 10px; left: 2px; display: inline-block; ' >", 
        names(IconSet)[n] ,
        "</p>",
        "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

