library(stringi)
library(jsonlite)
library(pracma)
library(ggplot2)
library(reshape2)
library(plotly)


#hey

con <- dbConnect(odbc::odbc(), Driver = "SQL Server", Server = "atx-chemometrics-data.database.windows.net", 
                 Database = "chemometrics-lsx3k-model-repo", 
                 UID = "matlab", PWD = "ATx4355EXcel!!")



dataset <- dbGetQuery(con,'select Timestamp,Id,fuel,SampleId,Region,Result,wavelength_start,wavelength_end,Scores1,Scores2,Scores3,spectra from ghana')


dataset$spectra <- stri_encode(dataset$spectra , "", "UTF-8")
dataset$fuel <- stri_encode(dataset$fuel , "", "UTF-8")
dataset$Result <- stri_encode(dataset$Result , "", "UTF-8")
dataset$Date <- stri_encode(dataset$Date , "", "UTF-8")
dataset$DeviceName <- stri_encode(dataset$DeviceName , "", "UTF-8")

wavelength = seq(dataset$wavelength_start[1],dataset$wavelength_end[1],0.5)

x = zeros(length(wavelength),dim(dataset)[1])
for (i in 1:50){
      
      x[,i] <- fromJSON(dataset$spectra[i])
}

data <- data.frame(wavelength,x)
colnames(data[2,]) <- dataset$SampleId

n <- 20 #dim(dataset)[1]

Intensity <- x[,1]
p <- plot_ly(data, x = ~wavelength,y = ~Intensity, name = dataset$SampleId[1],type='scatter',mode='lines')
for (j in 2:n){
      assign(paste0('s',toString(j)),x[,j])
      p <- p %>% add_trace(x = ~wavelength,y = ~assign(paste0('X',toString(j)),x[,j]), 
                           name = dataset$SampleId[j], mode = 'lines')
      
}
p