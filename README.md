# R_ETL

In this repository you will find my very first script using R programming. 

# Situation

Acting as responsible of the Business intelligence system of the police of Barcelona I was responsible to ensure the correct load of data. 

Every weekend we received more than 50 files to be transformed and loaded into the database but the problem was that the ETL procedures were redundant, with too much dependencies between them and too hard for the server, and sometimes the system was unable to load the new data.

This fact provoked that the reports wasn't updated on Monday so I decided to create an alternative for my clients. 

# Action

ETLs were designed long time ago and documentation was not available or updated so after understanding what the new ETL should make, I decided to code the ETL on R as an alternative to the current ETL. This code transformed all files to generate several CSV files into the source directory, just ready to be loaded into the corresponding tables through a simple SSIS package that connected every flat file with the corresponding OLE DB destination.

# Result

The coded lasted less than 2 minutes to transform all files and the SSIS package less than 5 whilst the current ETL last almost 5 hours when the process was executed successfully.

By this way, my clients were able to get their reports updated on Monday despite the errors of the old ETL during the weekend.
