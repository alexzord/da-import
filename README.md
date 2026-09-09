# da-import

A template and script for importing plate set-ups into Design \& Analysis for qPCR



Running the batch script will locate your Rscript installation and generate a .bat script (GENERATE\_CSV.bat) that launches the included R script.



Please fill in the sample names (targets, sample types, and quantities are optional) in the template **before** running GENERATE\_CSV



The R script will read all files names "import\*.xlsx" in the current directory and transform them into importable csvs



The output file names will match the input names. The original template is not deleted. 



##v1.0.1:
Fixed bug where Sample type import failed with unset Sample names





