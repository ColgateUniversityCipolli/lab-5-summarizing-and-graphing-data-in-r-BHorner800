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
  
feature <- "overall_loudness"
loud <- allentown.data[[feature]]
test.data|>
  #Grouping the data by artist
  group_by(artist)|>
  #Summarizing the data (min, lower fence, upper fence, maximum)
  summarise(min = min(get(feature)),
            LF = quantile(get(feature), 0.25) - 1.5*IQR(get(feature)),
            UF = quantile(get(feature), 0.75) + 1.5*IQR(get(feature)),
            max = max(get(feature))
            ) |>
  mutate(out.of.range = loud < min | loud > max)|>
  mutate(unusual = loud < LF | loud > UF)|>
  mutate(description = case_when(
    out.of.range == TRUE ~ "Out of Range",
    unusual == TRUE ~ "Outlying",
    .default = "Within Range"
  ))


#Loading the data
allentown.data = read.csv("./data/essentia.data.allentown.csv")
other.songs.data = read.csv("./data/essentia.data.csv")



#Function to summarize each feature
within.range = function(feature, allentown.data, test.data){
  '
  feature -> Out of Range, Outlying, or Within Range
  
  Inputs the feature from all the other songs grouped by artist to find the
  min, max, LF, UF
  Compares it to the allentown values
  Outputs if the data is within range, outlying, or out of range for this
  feature of allentown for each artist
  '
  allentown.feature <- allentown.data[[feature]]
  test.data|>
    #Grouping the data by artist
    group_by(artist)|>
    #Summarizing the data (min, lower fence, upper fence, maximum)
    summarise(min = min(get(feature), na.rm = TRUE),
              LF = quantile(get(feature), 0.25, na.rm = TRUE) - 1.5*IQR(get(feature), na.rm = TRUE),
              UF = quantile(get(feature), 0.75, na.rm = TRUE) + 1.5*IQR(get(feature), na.rm = TRUE),
              max = max(get(feature), na.rm = TRUE)
    ) |>
    mutate(out.of.range = allentown.feature < min | allentown.feature > max)|>
    mutate(unusual = allentown.feature < LF | allentown.feature > UF)|>
    mutate(description = case_when(
      out.of.range == TRUE ~ "Out of Range",
      unusual == TRUE ~ "Outlying",
      .default = "Within Range"
    ))
}





##############################################################################
#Step 2
#Applying function to the data to see in which features Allentown differs from
#each artist
##############################################################################

#Looping through each feature of the data
descriptors.df = data.frame(artist = c("All Get Out", 
                              "Manchester Orchestra", 
                              "The Front Bottoms"))

i = 1 #counter
for (feature in colnames(other.songs.data)){
  #Only evaluating numeric features
  if (is.numeric(allentown.data[[feature]])){
    i = i + 1
    #Running the function
    result = within.range(feature, allentown.data, other.songs.data)
    descriptors.df = add_column(descriptors.df, feature = result$description)
    #Renames the column to the correct feature name
    names(descriptors.df)[i] = feature
  }
}


?merge()
?add_column
