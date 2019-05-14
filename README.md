# INTEGRAL_winter_cruise_ARANDA_2019
Evaluation of marine biogeochemical and tracegas data from the Baltic Sea gatherer during the INTEGRAL winter cruise on RV Aranda.

# R script labelling

1. read-in data from various sources and store as summarized files. Some preliminary quality checks and plots for correct read-in are generated. Obvious outliers and errornous data are removed.

2. perform calculations on summarized data and merge different data sets

3. plot (merged) data

X. Plots/checks of additional external data sources (SMHI monitoring, VOS Tavastaland, etc)


# Status by parameter

* GPS track: OK
* SST: reconstructed from science pump and ice well reading, consult read-me
* SSS: reconstructed from science pump and ice well reading, consult read-me
* pCO2:
  + HydroC, IOW: no quality control, post-processing or similar done yet
  + HydroC, FMI: no quality control, post-processing or similar done yet
  + Equilibrator system, IOW: AK removed questionable readings and introduced flags, consult read-me. T-correction missing
* pCH4:
  + HydroC, IOW: no quality control, post-processing or similar done yet
  + Equilibrator system, IOW: AK removed questionable readings and introduced flags, consult read-me. T-correction missing
* pH: data post-processed according to Mueller and Rehder (2018)
  + pH reported at measurement temperature, correction to in-situ T (or any other fixed temperature) is missing
  + pH for flowthrough measurements (SW) calculated at S=5, correction to in-situ salinity missing
* CT
  + IOW shipboard and lab data post-processed by Stefan Otto
  + calculated from pH HydroFIA and AT estimate



# Additional information

Some scripts contain old outcommented parts in the end that were originally written to read and plot raw data received during the cruise
