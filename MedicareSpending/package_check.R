#### installing library if it doesn't already exist ####
##Libraries used in MEPS project. 
# "Foreign" is used to import data files
if("foreign" %in% rownames(installed.packages()) == FALSE) install.packages("foreign")  

# "Hmisc" contains tools for coding quantiles related command
if("Hmisc" %in% rownames(installed.packages()) == FALSE) install.packages("Hmisc")

# "Plyr" contains tools for splitting, applying and combining data
if("plyr" %in% rownames(installed.packages()) == FALSE) install.packages("plyr")

# "nnet" contains tools for running multinomial logit model
if("nnet" %in% rownames(installed.packages()) == FALSE) install.packages("nnet")

# "xlsx" contains tools for saving results to multiple pages in an excel file
if("xlsx" %in% rownames(installed.packages()) == FALSE) install.packages("xlsx")

# "reshape" contains tools for reshaping data frames
if("reshape" %in% rownames(installed.packages()) == FALSE) install.packages("reshape")

# "gglot2" contains tools for graphing
if("ggplot2" %in% rownames(installed.packages()) == FALSE) install.packages("ggplot2")

# "psych" contains tools for summarying data by group
if("psych" %in% rownames(installed.packages()) == FALSE) install.packages("psych")

# "dplyr" contains tools for 
if("dplyr" %in% rownames(installed.packages()) == FALSE) install.packages("dplyr")

# "truncreg" contains tools for Estimation of models for truncated Gaussian variables by maximum likelihood.
if("truncreg" %in% rownames(installed.packages()) == FALSE) install.packages("truncreg")

# "mhurdle" contains tools for Estimation of models with zero left-censored variables. 
if("mhurdle" %in% rownames(installed.packages()) == FALSE) install.packages("mhurdle")

# "RCurl" contains tools for 
if("RCurl" %in% rownames(installed.packages()) == FALSE) install.packages("RCurl")

# "downloader" contains tools for 
if("downloader" %in% rownames(installed.packages()) == FALSE) install.packages("downloader")

# "digest" contains tools for 
if("digest" %in% rownames(installed.packages()) == FALSE) install.packages("digest")

# "gdata" contains tools for 
if("gdata" %in% rownames(installed.packages()) == FALSE) install.packages("gdata")

if("openxlsx" %in% rownames(installed.packages()) == FALSE) install.packages("openxlsx")

# "base" contains tools that provide a low-level interface to the computer's file system.
if("base" %in% rownames(installed.packages()) == FALSE) install.packages("base")

# "rPython" contains tools that allows you to interface with Python (doesn't currently work in Windows)
#if("rPython" %in% rownames(installed.packages()) == FALSE) install.packages("rPython")


# Requiring use of installed packages
library(foreign)
library(Hmisc)
library(plyr)
library(nnet)
library(reshape)
library(ggplot2)

library(psych) 
library(dplyr)

library(truncreg)
library(mhurdle)

library(RCurl)				# load RCurl package (downloads files from the web)
library(foreign) 			# load foreign package (converts data files into R)
library(downloader)			# downloads and then runs the source() function on scripts from github

library(gdata)
library(openxlsx)
library(base)
#library(rPython)