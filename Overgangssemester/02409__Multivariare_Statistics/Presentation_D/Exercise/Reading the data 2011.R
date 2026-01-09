# how to read in data for 2011
library(readr)
dat=read_delim("C:/Users/dasth/Downloads/reflexes.cor.csv", 
               delim = ";", escape_double = FALSE, trim_ws = TRUE) # replace with your path

# convert to numeric and data frame
dat[] <- lapply(dat, function(x) {
  if (is.character(x)) as.numeric(gsub(",", ".", x)) else x
})

cor_mat=dat[,-1] # remove redudant column