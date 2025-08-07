# Thesis_Leiden_Utilitymeasures

Hi!

This repository follows the following structure. There are a number of folders:

Analysis -> This maps contains the data from the survey, and from the indirect measures (for every map type there is one excel file).
The analysis files (e.g. griddata analysis) use the averages from the file "survey data averages".

Maps -> these are the scripts that create the various maps, in order to calculate the measures it is adviced to run these first.
The creation of the areal maps may take some time. The file "Heatmaps without legend" is necessary to calculate all heatmap measures.

Measures -> This file contains the final code for all measures per category (e.g. "spatial association" or "ripley's K variations").
In most cases the functions for the measures, and the application to the maps is separate (a "FINAL" file with the functions, and an "application" file).

Other -> This file contains all intermediate scripts and is likely of little interest.
