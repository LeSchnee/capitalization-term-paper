#DATENVISUALISIERUNG_HAUSARBEIT_GROßSCHREIBUNG

#Voreinstellungen--------------------------------------------------------

#Pakete und Working directory einstellen
setwd("C:\\Users\\lenas\\OneDrive\\Dokumente\\Literatur\\Statistik_Korpuslinguisten\\Hausarbeit_Statistik\\Data")
library(data.table)
library(gridExtra)
library(ggplot2)
library(ggmosaic)
library(ggeffects)
#library(tidyverse)
library(lme4)
library(sjPlot)
#library(sjmisc)
#library(sjlabelled)

#library(afex)
library(MuMIn)
library(NCmisc)

#stelle scientificNotation aus(bis sie 999 stellen länger ist als die normale)
options(scipen=999)

#Daten -------------------------

#einlesen, anschauen, Datenstruktur anzeigen
GRO<-read.csv("LogRegData0211.csv", sep=";", header=TRUE, encoding = "utf-8")
attach(GRO)
#View(GRO)
str(GRO)

#Umbenennen von Ausprägungen

##Großsschreibung
GRO$upperCase = as.character(GRO$upperCase)
GRO$upperCase[GRO$upperCase =="WAHR"] <- "gross"
GRO$upperCase[GRO$upperCase =="FALSCH"] <- "klein"
GRO$upperCase=as.factor(GRO$upperCase)
##Gender
GRO$Gender = as.character(GRO$Gender)
GRO$Gender[GRO$Gender =="fem"] <- "weiblich"
GRO$Gender[GRO$Gender =="male"] <- "maennlich"
GRO$Gender = as.factor(GRO$Gender)
##Prestige
GRO$Prestige_Lemma = as.character(GRO$Prestige_Lemma)
GRO$Prestige_Lemma[GRO$Prestige_Lemma == "WAHR"]<- "prestige"
GRO$Prestige_Lemma[GRO$Prestige_Lemma =="FALSCH"]<- "neutral"
GRO$Prestige_Lemma = as.factor(GRO$Prestige_Lemma)


#Datenvisualisierung--------------------------------
## 1. Gender und Großsschreibung--------------------
#Kreuztabelle anzeigen lassen
genderGroschKreuz<-table(GRO$upperCase, GRO$Gender)
addmargins(genderGroschKreuz)
chisq.test(genderGroschKreuz)
#Barplot in personalisiertem Theme erstellen:
genderGrosch2<-ggplot(GRO) +
  aes(x = Gender, fill = upperCase) +
  geom_bar(position = "stack") +
  geom_text(aes(label=..count..),stat = "count", position = position_stack(0.5))+
  theme_classic()+
  ylab("absolute Frequenz")+
  scale_fill_manual(values = alpha(c("lightgrey", "darkgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#Speichern (einmal den path festlegen)
path<-"C:/Users/lenas/OneDrive/Dokumente/Literatur/Statistik_Korpuslinguisten/Hausarbeit_Statistik"

ggsave(filename = paste0(path,"Abbildung1.png"), 
       plot = genderGrosch2,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

## 2. Gender und Moralische Bewertung-------------------------------------

GenderMoralKreuz<-table(GRO$Gender, GRO$Moralische.Bewertung)
addmargins(GenderMoralKreuz)
# Abbildung: absolut
genderMoral<-ggplot(GRO) +
  aes(x = Gender, fill = Moralische.Bewertung) +
  geom_bar(position = "stack") +
  geom_text(aes(label=..count..),stat = "count", position = position_stack(0.5))+
  theme_classic()+
  ylab("absolute Frequenz")+
  scale_fill_manual(values = alpha(c("darkgrey", "grey", "lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#speichern
ggsave(filename = paste0(path,"Abbildung2.png"), 
       plot = genderMoral,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

#Abbildung: relativ
genderMoral2<-ggplot(data=GRO)+
  geom_mosaic(aes(x=product(Gender),fill=Moralische.Bewertung))+
  theme_classic()+
  xlab("Gender")+
  scale_y_continuous(labels= waiver(), "")+
  scale_fill_manual(values = alpha(c("darkgrey", "grey", "lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL
#speichern
ggsave(filename = paste0(path,"Abbildung3.png"), 
       plot = genderMoral2,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

## 3. Genderdiffernzierter Blick auf die moralische Bewertung-------------------------------

#Subsets Maenner und Frauen erstellen
FRAUEN<-subset(GRO, Gender== "weiblich")
#View(FRAUEN)
MAENNER<-subset(GRO,Gender == "maennlich")
#View(MAENNER)

### 3.1 Moralische Bewertung und Grossschreibung bei Frauen-----------------------------
#Kreuztabelle anzeigen lassen:
genderMoralKreuzW<-table(FRAUEN$upperCase, FRAUEN$Moralische.Bewertung)
addmargins(genderMoralKreuzW)


# 3.1.1 Barplot mit absoluten Haeufigkeiten erstellen und speichern:--------------------------------
MoralGroschW<-ggplot(FRAUEN) +
  aes(x = Moralische.Bewertung, fill = upperCase) +
  geom_bar(position = "stack") +
  ylim(0,150)+
  geom_text(aes(label=..count..),stat = "count", position = position_stack(0.5))+
  theme_classic()+
  ylab(" ")+
  xlab("Bewertung Frauen")+
  scale_fill_manual(values = alpha(c("darkgrey", "lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#speichern
ggsave(filename = paste0(path,"Abbildung4_Frauen.png"), 
       plot = MoralGroschW,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

# 3.1.2 Mosaicplot mit Relation anzeigen lassen und speichern:--------------------------------
MoralGroschW2<-ggplot(data=FRAUEN)+
  geom_mosaic(aes(x=product(Moralische.Bewertung),fill=upperCase))+
  theme_classic()+
  xlab("Bewertung Frauen")+
  scale_y_continuous(labels= waiver(), "")+
  scale_fill_manual(values = alpha(c("darkgrey","lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#speichern
ggsave(filename = paste0(path,"Abbildung5_Frauen.png"), 
       plot = MoralGroschW2,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

### 3.2 Moralische Bewertung und Großschreibung bei Männern-------------------------------------------
#Kreuztabelle anzeigen lassen:
genderMoralKreuzM<-table(MAENNER$upperCase, MAENNER$Moralische.Bewertung)
addmargins(genderMoralKreuzM)

# 3.2.1 Barplot mit absoluten Häufigkeiten erstellen und speichern:------------------------
MoralGroschM<-ggplot(MAENNER) +
  aes(x = Moralische.Bewertung, fill = upperCase) +
  geom_bar(position = "stack") +
  geom_text(aes(label=..count..),stat = "count", position = position_stack(0.5))+
  theme_classic()+
  ylim(0,150)+
  ylab("absolute Frequenz")+
  xlab("Bewertung Männer")+
  scale_fill_manual(values = alpha(c("darkgrey", "lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.position = "none")+
  NULL

#speichern
ggsave(filename = paste0(path,"Abbildung6_Maenner.png"), 
       plot = MoralGroschW,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

# 3.2.2 Mosaicplot mit Relation anzeigen lassen und speichern:--------------------------------
MoralGroschM2<-ggplot(data=MAENNER)+
  geom_mosaic(aes(x=product(Moralische.Bewertung),fill=upperCase))+
  theme_classic()+
  xlab("Bewertung Männer")+
  scale_y_continuous(labels= waiver(), "")+
  scale_fill_manual(values = alpha(c("darkgrey","lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.position = "none")+
  NULL

#speichern
ggsave(filename = paste0(path,"Abbildung7_Maenner.png"), 
       plot = MoralGroschM2,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

## 4. LemmaPrestige--------------------------------------
#Kreuztabelle anzeigen lassen LemmaPrestige und Gender
genderPrestige<-table(GRO$Gender,GRO$Prestige_Lemma)
addmargins(genderPrestige)

#Kreuztabelle anzeigen lassen LemmaPrestige und Großschreibung bei Maennern
LemmaGroschKreuzM<-table(MAENNER$upperCase, MAENNER$Prestige_Lemma)
addmargins(LemmaGroschKreuzM)

# 4.1 Barplot mit absoluten Häufigkeiten erstellen und speichern:------------------------
PrestGroschM<-ggplot(MAENNER) +
  aes(x = Prestige_Lemma, fill = upperCase) +
  geom_bar(position = "stack") +
  geom_text(aes(label=..count..),stat = "count", position = position_stack(0.5))+
  theme_classic()+
  ylab("absolute Frequenz")+
  xlab("Lemma")+
  scale_fill_manual(values = alpha(c("darkgrey", "lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#speichern
ggsave(filename = paste0(path,"Abb.MaennerPrestBar.png"), 
       plot = PrestGroschM,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

# 4.2 Mosaicplot mit Verhältnis anzeigen lassen und speichern:--------------------------------
PrestGroschM2<-ggplot(data=MAENNER)+
  geom_mosaic(aes(x=product(Prestige_Lemma),fill=upperCase))+
  theme_classic()+
  xlab("Lemma")+
  scale_y_continuous(labels= waiver(), "")+
  scale_fill_manual(values = alpha(c("darkgrey","lightgrey")))+
  theme(axis.text=element_text(size = 16), axis.title = element_text(size=18, face ="bold"), 
        legend.text = element_text(size = 16), legend.title = element_text(colour = "transparent"))+
  NULL

#speichern
ggsave(filename = paste0(path,"Abb.MaennerPrestMosaic.png"), 
       plot = PrestGroschM2,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

# 4.3 Rollen in denen die prestigebehafteten Lemmata vorkommen-----------

#Subset definieren: Prestige Lemmata
Prestigis<-subset(MAENNER,Prestige_Lemma == "prestige")
#View(Prestigis)
str(Prestigis$Lemma)

#Ähnliche Lemmata zusammenlegen
Prestigis$Lemma = as.character(Prestigis$Lemma)
Prestigis$Lemma[Prestigis$Lemma =="ammann"] <- "amtmann"
Prestigis$Lemma = as.factor(Prestigis$Lemma)

#Label ändern
Prestigis$Rolle..L.S.. = as.character(Prestigis$Rolle..L.S..)
Prestigis$Rolle..L.S..[Prestigis$Rolle..L.S.. == "Justiz&Helfer"] <- "Justiz"
Prestigis$Rolle..L.S.. = as.character(Prestigis$Rolle..L.S..)
Prestigis$Rolle..L.S..[Prestigis$Rolle..L.S.. == "nicht entschieden"] <- "sonstige"

#Reihenfolge der Rollen (levels) ändern
Prestigis$Rolle..L.S..<-factor(Prestigis$Rolle..L.S.., levels = c("sonstige", "Justiz","Geschädigter", "Zeuge", "Denunzierter", "Angeklagter"))
                                                                 
#Barplot anzeigen
PrestRollBunt<-ggplot(data = Prestigis)+
  aes(x = Lemma)+
  aes(fill = Rolle..L.S..)+
  geom_bar()+
  theme_bw()+
  coord_flip()+
  labs(fill = "Rolle")+
  ylab("Anzahl Belege")+
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Purples"))(6)[1:6], guide = guide_legend(reverse=FALSE))
  NULL
  
#speichern
ggsave(filename = paste0(path,"Abb.PrestRollBarBunt.png"), 
       plot = PrestRollBunt,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

# 4.4 Großschreibung Männer nach Rolle, grid: Prestige--------------------------------------
PrestGrid<-ggplot(data = MAENNER)+
  aes(x = Rolle..L.S..)+
  aes(fill = upperCase)+
  geom_bar()+
  theme_bw()+
  coord_flip()+
  facet_grid(.~Prestige_Lemma)+
  labs(fill = "")+
  xlab("Rolle")+
  ylab("Anzahl Belege")+
  scale_fill_manual(values = alpha(c("darkgrey","lightgrey")))
  NULL
  
#speichern  
ggsave(filename = paste0(path,"Abb.GroschRollGridPrest.png"), 
       plot = PrestGrid,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

#5. Mehrere Plots in einer Abbildung------------------------------
#Moralische Bewertung Maenner und Frauen nebeneinander
#absolute Frequenz
MoGroSch<-grid.arrange(MoralGroschM, MoralGroschW, nrow = 1)

ggsave(filename = paste0(path,"Abb.MoGroSch.png"), 
       plot = MoGroSch,
       width = 250, 
       height = 100,
       units = "mm", 
       dpi = 500)
#Verhältnis
MoGroSch2<-grid.arrange(MoralGroschM2, MoralGroschW2 , nrow = 1)

ggsave(filename = paste0(path,"Abb.MoGroSch2.png"), 
       plot = MoGroSch2,
       width = 270, 
       height = 100,
       units = "mm", 
       dpi = 500)


# Schreiberspezifische Unterschiede-------------------------------------------------------
#Factorlevels von Ort (Protokollnamen) umgekehrt sortieren damit sie alphabetisch erscheinen in der Darstellung
Ort1<-fct_rev(Ort)
levels(Ort1)

SchrDiff<-ggplot(data = GRO)+
  aes(x = Ort1)+
  aes(fill = upperCase)+
  geom_bar()+
  theme_bw()+
  coord_flip()+
  labs(fill = "Initiale")+
  ylab("Personenbezeichnungen")+
  xlab("Protokoll")+
  scale_fill_manual(values = colorRampPalette(RColorBrewer::brewer.pal(9, "Greys"))(6)[3:6], guide = guide_legend(reverse=FALSE))
  NULL

#speichern  
  ggsave(filename = paste0(path,"SchreiberUnterschiede.png"), 
         plot = SchrDiff,
         width = 250, 
         height = 100,
         units = "mm", 
         dpi = 500)

# 7. Lemmafrequenz im Korpus-----------------------------------
#Belege nach Lemmafrequenz sortieren (absteigend)    
Lemmafreq<- GRO[order(-Freq),]
#View(Lemmafreq)
#Lege neue Spalte mit (gruppierten) Frequency-Levels an
#summary(Freq)
GRO$FreqGrouped <- cut(GRO$Freq, c(-Inf,23, Inf), c("low", "high"))
FREQUENTESTE<-subset(GRO, FreqGrouped == "high")
head(FREQUENTESTE)

#Stacked Barplot erstellen
#(Groß-/Kleinschreibung bei den am häufigsten im Korpus vorkommenden Lemmata
FreqLemmata<-ggplot(data = FREQUENTESTE)+
  aes(x = FREQUENTESTE$Lemma)+
  aes(fill = FREQUENTESTE$upperCase)+
  geom_bar()+
  theme_bw()+
  coord_flip()+
  labs(fill = "")+
  xlab("Häufigste Lemmata (Freq>23)")+
  ylab("Anzahl Belege")+
  scale_fill_manual(values = alpha(c("darkgrey","lightgrey")))
NULL

ggsave(filename = paste0(path,"FrequentesteLemmata.png"), 
       plot = FreqLemmata,
       width = 150, 
       height = 100,
       units = "mm", 
       dpi = 500)

### GENERALISIERTES MODELL-----------------------------------


##Maximales Modell mit allen Faktoren

##Erster Versuch: maximales Modell ohne Optimierer-Spezifizierung (Konvergenzprobleme)
#fullmodel_old<-glmer(upperCase~Gender*Moralische.Bewertung + Prestige_Lemma + Freq + (1+Gender|Ort), data=GRO, family="binomial")

##Funktion allFit() testet verschiedene Optimierer Algorythmen (Ergebnis: "bobyqa" funktioniert für dieses Datenset)
#model_all <- allFit(fullmodel_old, parallel = "multicore")
#summary(model_all)

##Zweiter Versuch: maximales Modell mit Optimierer "bobyqa" (konvergiert!)
fullmodel<-glmer(upperCase~Gender*Moralische.Bewertung + Prestige_Lemma + Freq + (1+Gender|Ort), data=GRO, family="binomial", control = glmerControl(optimizer = "bobyqa")) 
summary(fullmodel)
isSingular(fullmodel,tol = 1e-05)
r.squaredGLMM(fullmodel)

# #Ausgabe Tabelle mit Odds Ratios und Konfidenz Intervallen und p-Wert
# tab_model(fullmodel)
# summary(fullmodel)

# #Schätze Effekte von Moralischer Bewertung und Gender 
# eff<-ggpredict(fullmodel,c("Moralische.Bewertung", "Gender"))
# setnames(eff, old= c("group"), new= c("Gender"))
# eff
# 
# #Plottet Effekt
# fullmodelVis <-ggplot(eff,aes(x,predicted))+
#   geom_point(aes(color=Gender), alpha=0.5)+ 
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=Gender), width = 0.2,alpha=0.5)+
#   theme_light()+
#   scale_color_manual(values= c ("grey20", "black"))+
#   scale_fill_manual(values=c ("grey20", "black"))+
#   xlab("\n Soziale Bewertung") +
#   ylab("") +
#   scale_y_continuous(limits = c(0, 1))+
#   NULL


#Schätzt Effekt von Lemmaprestige
eff_Prest<-ggpredict(fullmodel,c("Prestige_Lemma"))

#Plottet Effekt von Lemmaprestige
fullmodelVisEffPrest <-ggplot(eff_Prest,aes(x,predicted))+
  geom_point()+
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2,alpha=0.5)+
  theme_light()+
  scale_color_manual(values= c ("grey20", "black"))+
  scale_fill_manual(values=c ("grey20", "black"))+
  xlab("\n Lemma") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1))+
  NULL

##Vergleichsmodel ohne Lemmaprestige
model2<-glmer(upperCase~Gender*Moralische.Bewertung + Freq + (1+Gender|Ort), data=GRO, family="binomial", control = glmerControl(optimizer = "bobyqa"))
summary(model2)
isSingular(model2,tol = 1e-05)
tab_model(model2)

r.squaredGLMM(model2)

#Vergleich fullmodel und model2 in einer Tabelle
tab_model(fullmodel, model2)
#likelihood ratio fullmodel und model2
anova(fullmodel,model2)

##model2 ist besser als das fullmodel (R2, AIC, p value) Schauen, ob die Werte für Gender und Moralische Bewertung abweichen:

#Schätze Effekte von Moralischer Bewertung und Gender
eff2<-ggpredict(model2,c("Moralische.Bewertung", "Gender"))
setnames(eff2, old= c("group"), new= c("Gender"))
eff2

#Plottet Effekt
fullmodelVis2 <-ggplot(eff2,aes(x,predicted))+
  geom_point(aes(color=Gender), alpha=0.5)+ 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color=Gender), width = 0.2,alpha=0.5)+
  theme_light()+
  scale_color_manual(values= c ("grey20", "black"))+
  scale_fill_manual(values=c ("grey20", "black"))+
  xlab("\n Soziale Bewertung") +
  ylab("") +
  scale_y_continuous(limits = c(0, 1))+
  NULL


# ##Vergleichsmodell ohne Frequenz
# model3<-glmer(upperCase~Gender*Moralische.Bewertung + Prestige_Lemma + (1+Gender|Ort), data=GRO, family="binomial",control = glmerControl(optimizer = "bobyqa")) 
# summary(model3)
# anova(fullmodel, model3)



##Abschließend--------------------------------------------
#Prüfen, welche Pakete im Skript verwendet werden
list.functions.in.file("Ha_Statistik_Schnee_final.R", alphabetic = TRUE)
#Wichtigste Pakete zitieren
citation("ggplot2")
citation("lme4")
