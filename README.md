# two term papers on the development of sentence-internal capitalization in Early Modern High German
+ 2017 paper
+ 2020 paper
+ Data and R Script

This repository contains a (bachelor) term paper on capitalization (2017), the data used, a follow-up study conducted for a (master) term paper in statistics (2020) as well as the data used in both papers and the R script for the second paper.

# 2017 paper: "Gender und Großschreibung. Misogyner Einfluss in der Entwicklung der satzinternen Großschreibung im Frühneuhochdeutschen"

I analyzed the interaction between two factors that influence the capitalization of nouns: gender and moral evaluation of the referent.
The results show that terms denoting male referents have a general tendency towards capitalization, uneffected by the respective moral evaluation of the referent. Capitalization of the terms denoting female referents, however, does show a significant effect of the respective moral evaluation of the referent.

This paper has been referenced in the following articles:

Dücker, Lisa (2020): Großschreibung von Personenbezeichnungen in Hexenverhörprotokollen –  eine Analyse von Gender, Lemma und Rolle im Gerichtsprozess. In: Astrid Schütz, Renata Szczepaniak & Mona Hess (eds.): Kolloquium Forschende Frauen 2019. Beiträge Bamberger Nachwuchswissenschaftlerinnen. Band 11. Bamberg: University of Bamberg Press, 31–50. Available at: https://fis.uni-bamberg.de/bitstream/uniba/47039/3/fisba47039.pdf.

Lisa Dücker, Stefan Hartmann & Renata Szczepaniak (2020): Satzinterne Großschreibung von Substantiven und Substantivierungen in Hexenverhörprotokollen. In: Renata Szczepaniak, Lisa Dücker & Stefan Hartmann (eds.): Hexenverhörprotokolle als sprachhistorisches Korpus: Fallstudien zur Erschließung der frühneuzeitlichen Schriftsprache. Berlin, Boston: De Gruyter, 113–144.

# 2020 paper: "Gender und soziale Rolle in der Entwicklung der satzinternen Großschreibung –  eine multifaktorielle Analyse"

I analyzed multiple factors that influenced the development of the sentence-internal capitalization of nouns in Early Modern High German using parts of the SIGS-corpus (https://www.uni-muenster.de/SIGS-Projekt/corpus/datamap.html), which consists of protocols of witch trials, handwritten between 1588 and 1630.

One important factor affecting capitalization is the animacy of the referent (cf. Barteld et al. 2016, among others).
I analyzed terms belonging to only one animacy class, namely terms denoting people, with respect to further factors: gender of the referent, moral evaluation of the referent in the context of the trial, prestige of the term, and frequency of the term. 
These predictors were included in a generalized mixed model, with random intercepts and slopes for each individual protocol to account for idiosyncracies, since each protocol was written by a different scribe.

The results confirm the effect of gender (Szczepaniak/Barteld 2016) as well as frequency (Barteld et al. 2016) on capitalization. 
Prestige ascribed to the term itself did not show an effect. 
Also, although visual inspection suggests an interaction between gender and moral evaluation, this interaction did not prove to be significant.

# data
The dataset used in both papers is taken from the SIGS-corpus, that was formed in the SIGS-project on the emergence of sentence-internal capitalization (funded by the German Research Association DFG, SZ 280/2-1 and KO 909/12-1). The project (2013-2018) was a cooperation between the University of Hamburg (Fabian Barteld, Renata Szczepaniak) and the University of Münster (Klaus-Michael Köpcke, Marc Schutzeichel).
The dataset contains all instances of terms denoting persons (Personenbezeichnungen) that occur in the core corpus (cf. Szczepaniak/Barteld 2016) including meta data as well as annotations for animacy and gender of the referent. It was kindly provided to me by Stefan Hartmann and Renata Szczepaniak. 
I have reviewed the annotations for gender (as is described in the methods chapters of both papers) and added annotations of a) social roll and b) moral evaluation of the referent in the context of the trial as well as c) prestige of the term.

# R script
The statistical models reported in the second paper can be reproduced via this script, the same holds for all data visualization.
