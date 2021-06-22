# Replication Data for Thesis "Measuring Polarization in Parliamentary Debates"

## Data 

### Raw Data
The main data source constitutes the database of the [Open Discourse Project](https://opendiscourse.de/). The organization provides a database of all parliamentary debates from the German Bundestag between 1945 and 2020. They directly retrieve their data from the German Bundestag [Open Data Portal](https://www.bundestag.de/services/opendata), which provides the transcripts in PDF and XML format. The parsed data can be freely downloaded from [Harvard Dataverse](https://dataverse.harvard.edu/dataverse/opendiscourse). For this study, only the `speeches.csv` dataset was used, which holds a total of 899.526 speeches together with some metadata, such as speaker, data, session and direct link to the PDF protocolle from the Bundestag Website. 

>> Citation: Richter, F.; Koch, P.; Franke, O.; Kraus, J.; Kuruc, F.; Thiem, A.; Högerl, J.; Heine, S.; Schöps, K., 2020, "Open Discourse", https://doi.org/10.7910/DVN/FIKIBO, Harvard Dataverse

### Link to documenation and information system (DIP)

In order to filter the speeches for a specific topic (which is a basic condition for the ideological scaling and ML algorithms), it is necessary to also connect data from the German [Dokumentations- und Informationssystem für Parlamentsmaterialien (DIP)](https://dip.bundestag.de/erweiterte-suche?term=covid-19&rows=25), which is the official documenation and information system for parliamentary materials. Here, users can sort parliamentary texts using several filters, such as time period, subject area or transaction type. 

The filters used for the [query](https://dip.bundestag.de/erweiterte-suche?fld.0.0=deskriptor&fld.0.0.term=covid-19&op.0=AND&f.typ=Aktivit%C3%A4t&f.herausgeber_dokumentart=Bundestag-Plenarprotokoll&f.aktivitaetsart_p=05Reden%2C%20Wortmeldungen%20im%20Plenum&f.aktivitaetsart_p=05Reden%2C%20Wortmeldungen%20im%20Plenum~Rede&f.datum.start=2020-01-01&f.datum.end=2020-12-31&rows=25) were:
- **Format**: `Aktivität`
- **Datum**: `Between January 1, 2020 to December 31, 2020`
- **Schlagworte**: `Covid-19` 
- **Dokumentart**: `Bundestag-Plenarprotokoll`
- **Aktivitätsart**: `Reden, Wortmeldungen im Plenum` & `Rede`


After specifying all the filters, the search results were downloaded in a word document and saved under `data/Dip-Export.docx` (see Figure).

<p align="center">
<img src="https://github.com/lukasbirki/Thesis/blob/main/Figures/DIP-Export.png" alt="Exporting DIP" width="800" style="float: right;">
</p>

### Merging Data

In order to filter the primary `data/speeches.csv` dataset based on the selection in the `data/Dip-Export.docx`, the files were parsed by running the the following script. 

```R
Filter_Speeches.R
```
Ultimately, the output of the script provides the filtered dataset, which can be used for further analysis. 

