# Name: Conor Glackin
# Date: 06 July 2023
# Description: Final script for Vibrio paper

###### installing packages and libraries

install.packages("tidyverse")
install.packages("bayesbio")
install.packages("plyr")

if (!require("lubridate", quietly = TRUE))
  install.packages("lubridate")
if (!require("tidyverse", quietly = TRUE))
  install.packages("tidyverse")
if (!require("dplyr", quietly = TRUE))
  install.packages("dplyr")
if (!require("ggplot2", quietly = TRUE))
  install.packages("ggplot2")
if (!require("readxl", quietly = TRUE))
  install.packages("readxl")
if (!require("data.table", quietly = TRUE))
  install.packages("data.table")
if (!require("stringr", quietly = TRUE))
  install.packages("stringr")
install.packages("rlang")
if (!require("reshape", quietly = TRUE))
  install.packages("reshape")
if (!require("metrumrg", quietly = TRUE))
  install.packages("metrumrg", repos="http://R-Forge.R-project.org")
devtools::install_github("mrdwab/SOfun")
if (!require("janitor", quietly = TRUE))
  install.packages("janitor")
if (!require("ggtext", quietly = TRUE))
  install.packages("ggtext")
if (!require("ggExtra", quietly = TRUE))
  install.packages("ggExtra")
if (!require("cowplot", quietly = TRUE))
  install.packages("cowplot")
if (!require("ggfittext", quietly = TRUE))
  install.packages("ggfittext")
if (!require("ggh4x", quietly = TRUE))
  install.packages("ggh4x")
if (!require("ReportRs", quietly = TRUE))
  install.packages("ReportRs")
if (!require("devtools", quietly = TRUE))
  install.packages("devtools")
if (!require("openxlsx", quietly = TRUE))
  install.packages("openxlsx")
#if (!require("SOfun", quietly = TRUE))
#  install.packages("SOfun")
if (!require("openxlsx", quietly = TRUE))
  install.packages("readODS")
if (!require("corrr", quietly = TRUE))
  install.packages("corrr")
if (!require("glmnet", quietly = TRUE))
  install.packages("glmnet")
if (!require("mice", quietly = TRUE))
  install.packages("mice")
if (!require("ncdf4", quietly = TRUE))
  install.packages("ncdf4")
if (!require("sf", quietly = TRUE))
  install.packages("sf")
if (!require("ggfortify", quietly = TRUE))
  install.packages("ggfortify")
if (!require("e1071", quietly = TRUE)) 
  install.packages("e1071", dependencies=TRUE)
if (!require("corrplot", quietly = TRUE)) 
  install.packages("corrplot", dependencies=TRUE)
if (!require("funrar", quietly = TRUE)) 
  install.packages("funrar", dependencies=TRUE)
if (!require("e1071", quietly = TRUE)) 
  install.packages("e1071", dependencies=TRUE)
if (!require("ggpubr", quietly = TRUE)) 
  install.packages("ggpubr", dependencies=TRUE)
if (!require("keras", quietly = TRUE)) 
  install.packages("keras", dependencies=TRUE)
if (!require("ISOweek", quietly = TRUE)) 
  install.packages("ISOweek", dependencies=TRUE)
if (!require("knitr", quietly = TRUE)) 
  install.packages("knitr", dependencies=TRUE)
if (!require("patchwork", quietly = TRUE)) 
  install.packages("patchwork", dependencies=TRUE)
if (!require("PRROC", quietly = TRUE)) 
  install.packages("PRROC", dependencies=TRUE)
if (!require("ggnewscale", quietly = TRUE)) 
  install.packages("ggnewscale", dependencies=TRUE)
if (!require("abind", quietly = TRUE)) 
  install.packages("abind", dependencies=TRUE)
if (!require("forecast", quietly = TRUE)) 
  install.packages("forecast", dependencies=TRUE)
if (!require("pheatmap", quietly = TRUE)) 
  install.packages("pheatmap", dependencies=TRUE)


library(bayesbio)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(plyr)
library(readxl)
library(data.table)
library(stringr)
library(devtools)
#library(SOfun)
library(tidyr)
#library("metrumrg")
library(janitor)
library(ggtext)
library(scales)
library(ggExtra)
library(gridExtra)
library(cowplot)
library(ggfittext)
library(ggh4x)
library(openxlsx)
#library(Report)
library(readODS)
library(purrr)
library(reshape2)
library(caret)
library(randomForest)
library(mgcv)
library(vegan)
library(corrr)
library(glmnet)
#library(mice)
library(ncdf4)
library(sf)
library(lattice)
library(ggfortify) 
library(e1071)
library(corrplot)
library(missForest)
library(funrar)
library(RColorBrewer)
library(funrar)
library(pROC) 
library(DMwR)  # For SMOTE
library(e1071)  # For SVM
library(ggpubr)  # For adding correlation text to the plot
library(ISOweek)
library(knitr)
library(patchwork)
library(PRROC)
library(ggnewscale)
library(abind)
library(forecast)
library(pheatmap)
#library(keras)
library(ragg)

# for reactivation
library(reticulate)
use_condaenv("r-conda-tf", required = TRUE)

library(tensorflow)
tf$constant("✅ Hello TensorFlow!")
#install.packages("keras3")
library(keras3)

install.packages("remotes")
remotes::install_github("rstudio/tensorflow")
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")
library(tensorflow)
tf$constant("Hello TensorFlow!") # Just checking
install.packages("keras3")
library(keras3)

##### if above doesnt work then try this and run again for tensorflow problems
library(reticulate)
unlink(paste0(virtualenv_root(), "/r-tensorflow"), force = TRUE, recursive = TRUE)
#py_config()

#virtualenv_create("r-tensorflow")
#install_tensorflow(envname = "r-tensorflow")

###### new: for conda environment installation:

# Step 0 — Install 'reticulate' if needed
if (!requireNamespace("reticulate", quietly = TRUE)) {
  install.packages("reticulate")
}

# Step 1 — Load reticulate and clean up
library(reticulate)

# Step 2 — Remove old broken virtualenv (if it exists)
virtualenv_path <- file.path(path.expand("~"), ".virtualenvs", "r-tensorflow")
if (dir.exists(virtualenv_path)) {
  message("Removing old virtualenv...")
  unlink(virtualenv_path, recursive = TRUE, force = TRUE)
}

# Step 3 — Remove any existing broken Conda env
if ("r-conda-tf" %in% conda_list()$name) {
  message("Removing old Conda environment 'r-conda-tf'...")
  conda_remove("r-conda-tf")
}

# Step 4 — Install TensorFlow in a clean Conda env with Python 3.10
message("Installing TensorFlow 2.18.0 in Conda environment 'r-conda-tf'...")
tensorflow::install_tensorflow(
  method = "conda",
  envname = "r-conda-tf",
  version = "2.18.0",
  python_version = "3.10"
)

# Step 5 — Activate the Conda environment
use_condaenv("r-conda-tf", required = TRUE)

# Step 6 — Verify TensorFlow is working
library(tensorflow)
tf$constant("🎉 TensorFlow works with Conda and Python 3.10!")

# Optional — See full Python setup
py_config()

# for reactivation
library(reticulate)
use_condaenv("r-conda-tf", required = TRUE)

library(tensorflow)
tf$constant("✅ Hello TensorFlow!")
#install.packages("keras3")
library(keras3)



## for davids code

#if (!require("ggstatsplot", quietly = TRUE))
#  install.packages("ggstatsplot")
#if (!require("geosphere", quietly = TRUE))
#  install.packages("geosphere")
#if (!require("igraph", quietly = TRUE))
#  install.packages("igraph")
#if (!require("Rtools", quietly = TRUE))
#  install.packages("Rtools")
#if (!require("maptools", quietly = TRUE))
#  install.packages("maptools")
#if (!require("maps", quietly = TRUE))
# install.packages("maps")
#if (!require("R.utils", quietly = TRUE))
#  install.packages("R.utils")
#if (!require("rgeos", quietly = TRUE))
#  install.packages("rgeos")
#if (!require("rgdal", quietly = TRUE))
#  install.packages("rgdal")


#require("maps")
#require("stringr")
#require("R.utils")
#library(maptools)
#library(sp)
#require(rgdal)
#library(rgeos)
#require(sf)
#require(geosphere)
#library(caret)#

#### Part 3 - Map relative abundance of Vulnificus ####
#library(dplyr)
#library(tidyverse)
#library(data.table)


