## Aim

The aim of this project is to predict the age of abalone from its physical measurements using the data collected from UCI Machine Learning Repository (http://archive.ics.uci.edu/ml/machine-learningdatabases/abalone).


## Introduction

Data comes from an original (nonmachinelearning) study: Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and Wes B Ford (1994) "The Population Biology of Abalone (_Haliotis_species) in Tasmania. I. Blacklip Abalone (_H. rubra_) from the North Coast and Islands of Bass Strait",Sea Fisheries Division, Technical Report No. 48 (ISSN 10343288).
The aim of the project is to predict the age of abalone from physical measurements. The age of abalone is determined by cutting the shell through the cone, staining it, and counting the number of rings through a microscope -- a boring and time-consuming task. Other measurements, which are easier to
obtain, are used to predict the age. Further information, such as weather patterns and location (hence food availability) may be required to solve the problem. From the original data examples with missing values were removed (the majority having the predicted value missing), and the ranges of the continuous values have been scaled for use with an ANN (by dividing by 200).