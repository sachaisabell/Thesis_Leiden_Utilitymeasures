# Thesis_Leiden_Utilitymeasures

Hi!

This repository follows the following structure. There are a number of folders:

Analysis -> This folder contains the data from the survey, and from the indirect measures (for every map type there is one excel file).
The analysis files (e.g. griddata analysis) use the averages from the file "survey data averages".

Maps -> these are the scripts that create the various maps, in order to calculate the measures it is adviced to run these first.
The creation of the areal maps may take some time. The file "Heatmaps without legend" is necessary to calculate all heatmap measures.

Measures -> This folder contains the final code for all measures per category (e.g. "spatial association" or "ripley's K variations").
In most cases the functions for the measures, and the application to the maps is separate (a "FINAL" file with the functions, and an "application" file).

Survey/interviews -> Contains data on the questions groups, and the Limesurvey source code. The participant response of the interviews are included as an excel file.

Other -> This folder contains all intermediate scripts and is likely of little interest, but included for completeness sake.

--------------------------------------------------------------------------------------------------------------------------------------
Besides this repository, the following links may be of interest ->

Slides used during the qualitative interviews: 
https://docs.google.com/presentation/d/11qTw8iEBm4oM3QlyzkJpn1ADviOT_EEE9dU2WVOuudE/edit?usp=sharing

Link to the collaboard used for the qualitative analysis:
https://web.collaboard.app/share/-jBqDAhHJEpANU9WL64yXw

Link to the survey (responses are not longer collected, but the survey is still active): 
https://sacha-isabel.limesurvey.net/421291?lang=en
