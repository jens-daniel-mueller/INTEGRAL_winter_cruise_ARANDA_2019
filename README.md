# INTEGRAL_winter_cruise_ARANDA_2019
Evaluation of marine biogeochemical and tracegas data from the Baltic Sea gatherer during the INTEGRAL winter cruise on RV Aranda.

# R script labelling

1. read-in data from various sources and store as summarized files. Both, scripts and summarized data files are named according to Parameter_instrument_institution

2. perform calculations on summarized data and merge different data sets

3. plot (merged) data


# Status by parameter

* GPS track: OK
* SST: reconstructed from science pump and ice well reading, consult read-me
* SSS: reconstructed from science pump and ice well reading, consult read-me
* pCO2:
  +HydroC, IOW: no quality control, post-processing or similar done yet
  +HydroC, FMI: no quality control, post-processing or similar done yet
  +Equilibrator system, IOW: removed questionable readings, T-correction missing
* pCH4:
  +HydroC, IOW: no quality control, post-processing or similar done yet
  +Equilibrator system, IOW: removed questionable readings, T-correction missing



# Additional information

Some scripts contain old outcommented parts in the end that were originally written to read and plot raw data received during the cruise
