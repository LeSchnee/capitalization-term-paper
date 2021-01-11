# two term papers on the development of sentence internal capitalization in Early High German
+ Data and R Script

This repository contains a (bachelor) term paper on capitalization (2017), the data used, a follow-up study conducted for a (master) term paper in statistics(2020) as well as the data used in both papers and the R script for the second paper.

# data
The dataset used in both papers is taken from the Corpus of witch trials formed in the SIGS project on the emergence of sentence internal capialization (DFG, SZ 280 / 2-1 und KO 909 / 12-1).  Es handelt sich um eine Kooperation der Universitäten Hamburg   (Fabian Barteld, Renata Szczepaniak) und Münster (Klaus-Michael Köpcke, Marc Schutzeichel

# 2017 term paper

# 2020 term paper
I analyzed multiple factors that influenced the development of the sentence-internal capitalization of nouns in Early High German using parts of the SiGS-corpus (https://www.uni-muenster.de/SIGS-Projekt/corpus/datamap.html), which consists of protocols of witch trials, handwritten between 1588 and 1630.

One important factor affecting capitalization is the animacy of the referent (cf. Barteld et al. 2016, among others).
I analysed terms belonging to only one animacy class, namely terms denoting people, with respect to further factors: gender of the referent, moral evaluation of the referent in the context of the trial, prestige of the term, and frequency of the term. 
These predictors were included in a generalised mixed model, with random intercepts for each individual protocol and random slopes for gender and moral evaluation of the referent per protocol to account for idiosyncracies, since each protocol was written by a different author.

The results confirm the effect of gender (Szczepaniak & Barteld 2016: 46) as well as frequency (Barteld et al. 2016: 407) on capitalization. 
Prestige ascribed to the term itself did not show an effect. 
Also, although visual inspection suggests an interaction between gender and moral evaluation, this interaction did not prove to be significant.
