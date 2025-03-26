# threatening_for_whom
Replication package for “Threatening for Whom?  National boundary-making, immigration, and support for the welfare state”, Mireia Triguero Roura. Paper forthcoming at the European Sociological Review (2025)

TFW_ESR_cleaningfile.R

It contains the scripts necessary to collect all the Eurostat variables as well as to clean the EVS 2008 data. For true replicability, the original EVS 2008 data should be directly downloaded from the GESIS institute (requires login).

Other publicly available data, as well as intermediate tables (from Eurostat downloads) and the final clean dataset can be found in the data/ directory for direct reproducibility of the figures and tables.

TFW_ESR_figures_tables.R

It uses the data file created in TFW_ESR_cleaningfile.R and contains the scripts necessary to run the analyses presented in the figures and tables in paper, as well as the code to create such figures and tables. The resulting plots can be seen in the Figures/ directory

EVS_nuts_convergence.R

It matches the EVS specific ids for nuts2 to the standard NUTS naming 
