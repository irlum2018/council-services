# council-services
This repository contains the code for web-based viz to explore Melbourne City Council services offered to their residents in 2015 and 2016 using the City of Melbourne’s open dataset (https://data.melbourne.vic.gov.au/City-Council/Customer-service-requests-with-resolution-time/ht4h-vqbu)
## Getting Started

### Prerequisites
Install R - http://www.R-project.org/

### Running the Code
1. Download the source code in the chosen directory
2. Cd to the directory
3. Execute R -e "shiny::runApp('council-services')" The console will load the app and eventually display: “Listening on http://ip-address:port” 
4. To view and explore the viz using a standard browser connect to the running application by navigating your browser to the specified address.

```
If you downloaded the source code in ~/test directory
* >cd test
* >R -e "shiny::runApp('council-services')"
* …
Listening on http://127.0.0.1:3006
* To run the viz type the url http://127.0.0.1:3006 into your browser
```

## Built With

* [Shiny](http://shiny.rstudio.com) - The web app framework for R
* [shinyWidgets](https://rdrr.io/cran/shinyWidgets/) 
* [DataTables library](https://rstudio.github.io/DT/) 
* [Plotly](https://rdrr.io/cran/plotly/)
* [Themes for Shiny](http://rstudio.github.io/shinythemes/)

## Authors

* **Irina Lumsden** - *Initial work* 
