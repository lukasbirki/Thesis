# Replication Data for Thesis "Measuring Polarization"

## Data

### Raw Data
The main data source constitutes the database of the [Open Discourse Project](https://opendiscourse.de/). The organization provides a database of all parliamentary debates from the German Bundestag between 1945 and 2020. They directly retrieve their data from the German Bundestag [Open Data Portal](https://www.bundestag.de/services/opendata), which provides the transcripts in PDF and XML format. The parsed data can be freely downloaded from [Harvard Dataverse](https://dataverse.harvard.edu/dataverse/opendiscourse).

Richter, F.; Koch, P.; Franke, O.; Kraus, J.; Kuruc, F.; Thiem, A.; Högerl, J.; Heine, S.; Schöps, K., 2020, "Open Discourse", https://doi.org/10.7910/DVN/FIKIBO, Harvard Dataverse

### Link to documenation and information system (DIP)

In order to filter the speeches for a specific topic (which is a basic condition for the ideological scaling and ML algorithms), it is necessary to also connect data from the German [Dokumentations- und Informationssystem für Parlamentsmaterialien (DIP)](https://dip.bundestag.de/erweiterte-suche?term=covid-19&rows=25), which is the official documenation and information system for parliamentary materials. Here, users can sort all parliamentary speeches using several filters. 

The filters used for the thesis were:
-  **Date**: `Between January 1, 2020 to December 31, 2020`
- **Keywords**: `Covid-19` and `TOPIC`
-...

After specifying all the filters, the search results were downloaded in a word document

![alt text](http://url/to/img.png)
