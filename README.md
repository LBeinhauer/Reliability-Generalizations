# Reliability-Generalizations
 Reliability Generalization study for all scales consisting of multiple indicators in multi-site replication studies ManyLabs & RRR



This GitHub Repository contains code for an analysis of variability in reliability estimates (Reliability Generalization). Relevant scales from multi-site replication projects in Psychology were chosen - Many Labs 1, 2, 3, 5, RRR1:10.

Running through the scripts in a specific order is comparable to a pipeline, starting at raw-data supplied by the multi-site replication authors, leading to a meta-analysis of reliability coefficients decomposed variance components. Additionally, a shiny-app is included, which may give a brief overview over results, aids data exploration and visualizes key findings.

### The different R-scripts 
(in order to generate heterogeneity estimates of reliability coefficients):

1. [<Data_Extraction.R>](Data_Extraction.R)
	This script extracts relevant observations from the raw data (raw data is currently NOT supplied, as contact with original authors will be sought first).
	Specifically, this script reads the text-files in the [Extraction_Scripts](Extraction_Scripts) directory. These contain the code to extract and clean data for each specific scale. Runing the R-script will result in separate .csv.files for each scale in the [Extracted (Project) Data](Data/Extracted (Project) Data) subdirectory, within the [Data](Data) directoy (currently not supported).

2. [<Estimating_Reliability.R>](Estimating_Reliability.R)
	This script loads the previously extracted data and estimates reliability coefficients and standard errors for each scale respectively. Coefficients Cronbach's Alpha and McDonald's Omega are supported.
	Reliablity Estimates are stored in the [Reliability Estimates](Reliability Estimates) subdirectory, within the [Data](Data) directory (currently not supported).

3. [<Relability_Meta-Analysis.R>](Relability_Meta-Analysis.R)
	This script loads the previously generated reliability coefficient estimates, and uses them in a random-effects meta-analysis. Thereby, estimates concerning the coefficients' heterogeneity can be made. Results are (currently) stored in the [Shiny Data](Data/Shiny Data) subdirectory, within the <Data> directory.


### Additional R-scripts

- [<app.R>](app.R)
	This script allows for a quick & dirty exploration of data and results using a shiny-(web)application.

- [<RG_function-library.R>](RG_function-library.R)
	This script contains relevant functions for analysis, which will be sourced by the other R-scripts.

- [<Variances_Meta-Analysis.R>](Variances_Meta-Analysis.R)
	This script uses the previously generated reliability coefficients, to deconstruct the sample variance into two parts: true and error variance. These variance-components are (using bootstrapping) meta-analysed, in order to get an idea, whether the heterogeneity in reliability coefficients can be truly attributed to fluctuations in measurement error, or fluctuations in true variance.
	Warning: takes rather long to run!

- [<temp_Graphics_script.R>](temp_Graphics_script.R)
	This script is used to generate graphics, which are used for intermediary presentation of results. This file will likely disappear in the future.

