# Countries where armed conflicts took place - Data package

This data package contains the data that powers the chart ["Countries where armed conflicts took place"](https://ourworldindata.org/grapher/locations-of-ongoing-armed-conflicts?v=1&csvType=full&useColumnShortNames=false) on the Our World in Data website. It was downloaded on November 22, 2024.

## CSV Structure

The high level structure of the CSV file is that each row is an observation for an entity (usually a country or region) and a timepoint (usually a year).

The first two columns in the CSV file are "Entity" and "Code". "Entity" is the name of the entity (e.g. "United States"). "Code" is the OWID internal entity code that we use if the entity is a country or region. For normal countries, this is the same as the [iso alpha-3](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3) code of the entity (e.g. "USA") - for non-standard countries like historical countries these are custom codes.

The third column is either "Year" or "Day". If the data is annual, this is "Year" and contains only the year as an integer. If the column is "Day", the column contains a date string in the form "YYYY-MM-DD".

The final column is the data column, which is the time series that powers the chart. If the CSV data is downloaded using the "full data" option, then the column corresponds to the time series below. If the CSV data is downloaded using the "only selected data visible in the chart" option then the data column is transformed depending on the chart type and thus the association with the time series might not be as straightforward.

## Metadata.json structure

The .metadata.json file contains metadata about the data package. The "charts" key contains information to recreate the chart, like the title, subtitle etc.. The "columns" key contains information about each of the columns in the csv, like the unit, timespan covered, citation for the data etc..

## About the data

Our World in Data is almost never the original producer of the data - almost all of the data we use has been compiled by others. If you want to re-use data, it is your responsibility to ensure that you adhere to the sources' license and to credit them correctly. Please note that a single time series may have more than one source - e.g. when we stich together data from different time periods by different producers or when we calculate per capita metrics using population data from a second source.

## Detailed information about the data


## Country where conflict took place - Conflict type: all
At least one all conflict event took place in this country in a given year.
Last updated: August 26, 2024  
Date range: 1989–2023  


### How to cite this data

#### In-line citation
If you have limited space (e.g. in data visualizations), you can use this abbreviated in-line citation:  
Uppsala Conflict Data Program (2024); Natural Earth (2022) – processed by Our World in Data

#### Full citation
Uppsala Conflict Data Program (2024); Natural Earth (2022) – processed by Our World in Data. “Country where conflict took place - Conflict type: all” [dataset]. Uppsala Conflict Data Program, “Georeferenced Event Dataset v24.1”; Natural Earth, “Natural Earth - Large scale data (1:10m Cultural Vectors) 5.1.1” [original data].
Source: Uppsala Conflict Data Program (2024), Natural Earth (2022) – processed by Our World In Data

### What you should know about this data
* '1' indicates that there was a conflict event in the given country. '0' indicates that there was no conflict event in the given country.
* An armed conflict is a disagreement between organized groups, or between one organized group and civilians, that causes at least 25 deaths during a year. This includes combatant and civilian deaths due to fighting.

### Sources

#### Uppsala Conflict Data Program – Georeferenced Event Dataset
Retrieved on: 2024-08-26  
Retrieved from: https://ucdp.uu.se/downloads/index.html#ged_global  

#### Natural Earth – Natural Earth - Large scale data (1:10m Cultural Vectors)
Retrieved on: 2023-11-28  
Retrieved from: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/10m-admin-0-countries/  

#### Notes on our processing step for this indicator
UCDP provides geographical coordinates of each conflict event. We have mapped these coordinates to countries by means of the Natural Earth dataset.

In some instances, the event's coordinates fall within the borders of a country. Other times, the event's coordinates fall outside the borders of a country. In the latter case, we have mapped the event to the country that is closest to the event's coordinates.

Conflict event with id "53238" and relid "PAK-2003-1-345-88" was assigned to "Siachen Glacier" by Natural Earth. We have mapped it to "Pakistan" following the text in the `where_description` field from the Natural Earth data, which refers to "Giang sector in Siachen, Pakistani Kashmir".


    