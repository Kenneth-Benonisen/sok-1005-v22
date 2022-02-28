
library(rvest)
library(tidyverse)
library(rlist)


# Oppgaven er løst sammens med Tore Birkelund. 


# task: build a procedure that can scrape "many" courses, e.g. the 3 courses you have this semester, from a list

# Oppretter en liste med nettsiden jeg ønsker å scrape. Det er nødt til å stå i paste0, det vil ikke fungere dersom du prøver å opprette i en liste. 
list_url <- 
  paste0("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&View=list",
         "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&View=list",
         "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=BED-2021-1&View=list")


# leser nettsidene jeg scraper ved hjelp av map funksjonen som virker på lik linje som en for loop. 
pages <- list_url[[1]] %>%
  map(read_html)

# omdanner html table fra nettsidene til en data frame.  
tables <- pages[[1]] %>%
  html_nodes('table') %>% 
  html_table(fill=TRUE)

# samler listene i tables til ên dataframe. 
df <- list.stack(tables, fill=FALSE)

# omgjør column names.
colnames(df) <- df[1,]

# Fjerner radene som inneholder Dato.
df <- df %>% filter(!Dato=="Dato")

# Deler dato kolonnen i to. 
df <- df %>% separate(Dato, 
                      into = c("Dag", "Dato"), 
                      sep = "(?<=[A-Za-z])(?=[0-9])")

# formaterer dato kolonnen til dato format. 
df$Dato <- as.Date(df$Dato, format="%d.%m.%Y")

# oppretter en "ukes" variabel.
df$Uke <- strftime(df$Dato, format = "%V")

# Omgjør kolonnen Dag til cha. 
df$Dag <- as.character(df$Dag)

# Utfyller blanke columns innad i Dag med NA
df$Dag[df$Dag ==""] <- NA

# Fyller inn manglende dato i datasettet. 
df <- df %>% fill(c(Dag,Dato,Uke))

# Velger hvordan vi ønsker å fremstille dataframen. 
df <- df %>% select(Dag,Dato,Uke,Tid,Rom,Emnekode,Beskrivelse,Lærer)
df
