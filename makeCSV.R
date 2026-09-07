#This script reads all import....xlsx files
#makes .csv for Design&Analysis import
#contact Alex for bug fixes

pkg <- c("readxl", "purrr", "reshape2", "dplyr")

for (p in pkg) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p, repos = "https://cloud.r-project.org/")
  }
  library(p, character.only = TRUE)
}

#wd <- "C:/Users/LocalAdmin/Documents/Wet/qpcrImport/"
#setwd(wd)

excels <- list.files(pattern="^import.*\\.xlsx$")


sheets <- list("Samples","Targets","Types","Quantities")
read_template <- function(sheet, file=exc){
  dat <- read_excel(file, sheet = sheet, range = "A1:Y17") %>%
    as.data.frame
  rownames(dat) <- dat[,1]
  dat <- dat[,-1]
  dat <- melt(t(as.matrix(dat))) %>%
    mutate(Coor=paste0(Var2,Var1),Value=value) %>%
    select(Coor, Value) %>%
    mutate(Value=ifelse(startsWith(Value, "S="), "", Value))
  colnames(dat) <- c("Coor", sheet)
  return(dat)
}



for(exc in excels){

#exc=excels

#Import

tryCatch(
  {rawin <- lapply(sheets, read_template) %>%
  reduce(left_join, by="Coor")},
  error = function(e){stop(sprintf("Error reading %s - check sheets and formatting", exc))})

#Remove null samples
rawin[which(rawin$Sample==""),] <- ""

#Check correct typing
invalid_types <- unique(rawin$Types[!rawin$Types %in% c("U", "S", "P", "N", "")])
if (length(invalid_types) > 0) {
  cat("Found invalid types:", paste(invalid_types, collapse = ", "), "\n")
  stop("Types must be one of: U, S, P, N, (empty=U). Exiting...")
}

type_tab <- table(rawin$Samples, rawin$Types)
zero_counts <- rowSums(type_tab == 0)
multi_type <- names(zero_counts[zero_counts < (ncol(type_tab) - 1)])
if(length(multi_type>0)){
  err_type <- sprintf("Multiple types specified for %s in %s! Exiting...",
                      paste(multi_type, collapse=" "), exc)
  stop(err_type)
}

#Check standard quantities
standards <- row.names(rawin[rawin$Types=="S",])
if(any(nchar(rawin[standards,"Quantities"])==0)){
  err_std <- sprintf("Standards without quantities found in %s! Exiting...", exc)
  stop(err_std)
}

#Check no quantitites for negatives
negatives <- row.names(rawin[rawin$Types=="N",])
if(any(nchar(rawin[negatives,"Quantities"])>0)){
  err_ntc <- sprintf("NTCs with quantities found in %s! Exiting...", exc)
  stop(err_ntc)
}


#Clean up
clean <- 
  rawin %>%
  mutate(Types=case_when(Types=="U" | Types=="" ~ "UNKNOWN",
                        Types=="S" ~ "STANDARD",
                        Types=="P" ~ "POSITIVE_CONTROL",
                        Types=="N" ~ "NTC")) %>%
  mutate(Well=as.numeric(rownames(.))-1,
         Reporter=ifelse(Samples=="", "", "SYBR"),
         Quencher="") %>%
  select(Well, Coor, Samples, Targets, Types, Reporter, Quencher, Quantities) %>%
  rename(`Well Position`="Coor", `Sample Name`="Samples", `Target Name`="Targets",
         Task="Types", Quantity="Quantities")

#Output
o=gsub("xlsx","csv",exc)

tryCatch(
  {cat("* Block Type = 384-Well Block",
    paste0("* Date Created = ",Sys.time()),
    "* Passive Reference = ROX",
    "* Barcode = ",
    "",
    file=o, sep = "\n")},
  error = function(e){stop(sprintf("Please close %s - it's in the process of generation", o))})
suppressWarnings(
  write.table(clean, sep=",", file = o, append = T, quote = F, row.names = F))

sprintf("%s successully created", exc)

}

message("csv generation finished")






