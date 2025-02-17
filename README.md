# Replication for Historical Patterns and Trends in Teacher-Student Demographic Changes in the United States

This repository contains replication materials for the paper 'Historical Patterns and Trends in Teacher-Student Demographic Changes in the United States' published in [Socius](https://doi.org/10.1177/23780231251321323).

## Repository Contents

- Data files
- Analysis scripts
- Documentation of data download from IPUMS and replication procedures

## Instructions for replicating the analysis

1. You just need to download the two RDS files from the "Data_to_be_shared" folder and then run the scripts in "02_data_analysis.R" for all the main and supplement plots

## Additional Instructions for Downloading and Cleaning Data from IPUMS to create analytical samples

1. Download the required IPUMS files as shown in the Screenshot_of_IPUMS.png in the repo
2. The files are huge, so we converted the files to parquet files. To do that, you can use the commands in "r code for dta to parquet.rtf"
3. Even the parquet files (included in the repo) are quite large; so you may not be able to directly use them. But if you are able to download them without much trouble, then just run the first "01_data_prep.R" script 
4. #3 above will create the two RDS files needed to replicate the analysis
5. Follow the "Instructions for replicating the analysis" steps after #4

## Contact

For questions about the replication materials, please send an email to [Maithreyi Gopalan](mailto:mgopalan@uoregon.edu) 

