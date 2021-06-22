# Replication Data for Thesis "Measuring Polarization in Parliamentary Debates"

## Data 

### Raw Data
The main data source constitutes the database of the [Open Discourse Project](https://opendiscourse.de/). The organization provides a database of all parliamentary debates from the German Bundestag between 1945 and 2020. They directly retrieve their data from the German Bundestag [Open Data Portal](https://www.bundestag.de/services/opendata), which provides the transcripts in PDF and XML format. The parsed data can be freely downloaded from [Harvard Dataverse](https://dataverse.harvard.edu/dataverse/opendiscourse). For this study, only the `speeches.csv` dataset was used, which holds a total of 899.526 speeches together with some metadata, such as speaker, data, session and direct link to the PDF protocolle from the Bundestag Website. 

>> Citation: Richter, F.; Koch, P.; Franke, O.; Kraus, J.; Kuruc, F.; Thiem, A.; Högerl, J.; Heine, S.; Schöps, K., 2020, "Open Discourse", https://doi.org/10.7910/DVN/FIKIBO, Harvard Dataverse

### Link to documenation and information system (DIP)

In order to filter the speeches for a specific topic (which is a basic condition for the ideological scaling and ML algorithms), it is necessary to also connect data from the German [Dokumentations- und Informationssystem für Parlamentsmaterialien (DIP)](https://dip.bundestag.de/erweiterte-suche?term=covid-19&rows=25), which is the official documenation and information system for parliamentary materials. Here, users can sort parliamentary texts using several filters, such as time period, subject area or transaction type. 

The filters used for the thesis were:
- **Date**: `Between January 1, 2020 to December 31, 2020`
- **Keywords**: `Covid-19` and `TOPIC`
- **

After specifying all the filters, the search results were downloaded in a word document (see Figure).

<p align="center">
<img src="https://github.com/lukasbirki/Thesis/blob/main/Figures/DIP-Export.png" alt="Exporting DIP" width="800" style="float: right;">
</p>

The `Dip-Export.docx` is then saved in the `data` folder. 

### Merging Data

In order to filter the primary `speeches.csv` dataset based on the selection in the `Dip-Export.docx`, the files were parsed by running the the following script. 

```R
Filter_Speeches.R
```
Ultimately, the output of the script provides the filtered dataset, which can be used for further analysis. 

