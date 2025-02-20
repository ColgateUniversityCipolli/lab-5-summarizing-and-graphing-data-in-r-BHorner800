##########################################################################
#### Lab 5- Summarizing the Data ####
# Ben Horner #
##########################################################################



##############################################################################
#Step 0
#Loading libraries
##############################################################################
library(stringr)
library(jsonlite)
library(tidyverse)


##############################################################################
#Step 1
#Function using tidyverse to determine whether Allentown is out of range, 
#unusual, wor within the range for each band
##############################################################################

#Loading the data
allentown.data = read.csv("./data/essentia.data.allentown.csv")
test.data = read.csv("./data/essentia.data.csv")
  
loud <- allentown.data[["overall_loudness"]]
test.data|>
  #Grouping the data by artist
  group_by(artist)|>
  #Summarizing the data (min, lower fence, upper fence, maximum)
  summarise(min = min(overall_loudness),
            LF = quantile(overall_loudness, 0.25) - 1.5*IQR(overall_loudness),
            UF = quantile(overall_loudness, 0.75) + 1.5*IQR(overall_loudness),
            max = max(overall_loudness)
            ) |>
  mutate(out.of.range = loud < min | loud > max)|>
  mutate(unusual = loud < LF | loud > UF)|>
  mutate(description = case_when(
    out.of.range == TRUE ~ "Out of Range",
    unusual == TRUE ~ "Outlying",
    .default = "Within Range"
  ))

within.range = function(feature){
  '''
  feature -> Out of Range, Outlying, or Within Range
  
  Inputs the feature from all the other songs grouped by artist to find the
  min, max, LF, UF
  Compares it to the allentown values
  Outputs if the data is within range, outlying, or out of range for this
  feature of allentown for each artist
  '''
  summarise(min = min(overall_loudness),
            LF = quantile(overall_loudness, 0.25) - 1.5*IQR(overall_loudness),
            UF = quantile(overall_loudness, 0.75) + 1.5*IQR(overall_loudness),
            max = max(overall_loudness)
  ) |>
    mutate(out.of.range = loud < min | loud > max)|>
    mutate(unusual = loud < LF | loud > UF)|>
    mutate(description = case_when(
      out.of.range == TRUE ~ "Out of Range",
      unusual == TRUE ~ "Outlying",
      .default = "Within Range"))
}



  