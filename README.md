# covid

Extracting, Transforming, and Loading COVID-19 daily report data uploaded by CSSE at John Hopkins University [(found here)](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports).
The package is built in the etl framework developed by Benjamin Baumer and therefore, is an [etl](https://github.com/beanumber/etl) dependent package. The package includes functions with variations in the default methods for etl_extract,
etl_transform, and etl_load in order to work with COVID-19 data.

In sum, the covid package extracts the daily report data from [CSSEGISandData's](https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports) repository on github. The package then transforms the data to be loaded into a specified PostgreSQL database. The package comes with a postgresql init script to create a table inside the database that tracks the following variables: **admin**, **province_state**, **country_region**, **last_update**, **confirmed**, **deaths**, and **recovered**. The sql init script comes with constraints to prevent duplicate data entry, I will cover these constraints more indepth when I discuss the etl_load function. 

To install the package, run the following code in R:

```r
devtools::install_github('https://github.com/madisonvolpe/covid')
```

## How to Use 

To use the package you must load the **etl** and **covid** packages. After loading the **covid** package, you must create an etl covid object after creating the covid object...you can run the etl_extract, etl_transform, and etl_load functions.

```r
library(etl)
library(covid)

# connecting to db 
covid_db <- src_postgres(dbname = "covid_db", host = 'localhost', port = 5432,
                           user = Sys.getenv("user"), password = Sys.getenv("password"))
# creating covid object
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")

# running etl_extract, etl_transform, etl_load
covid_data %>% etl_extract() %>% etl_transform() %>% etl_load(db_con = covid_db$con)
```
The above code connects to a postgresql database, creates an etl_covid object and specifies the directory where data will be stored and the database where data will be loaded. Finally, running etl_extract, etl_transform, and etl_load on covid_data will extract all available data from the github repository, transform it, and then load it into the specified postgresql database. 

## In-depth Explanations of Functions

Etl_extract, etl_transform, and etl_load basically do what their names suggest, however there are arguments you can add to these functions to specify specific subsets of data that you want. Likewise, there is functionality under the hood that should be explained so that you are aware of the data processing behind the scenes. 

### etl_extract

*etl_extract* simply captures the COVID-19 daily report data from the repository linked above and saves it into the directory that you specified when creating the etl_covid object. Specifically, when creating the etl_covid object and specifying the directory, *etl_extract* creates raw and load folders inside the directory. *etl_extract* extracts the daily report data files from the repository and saves them inside the raw folder. No other modifications are made to the data and what you see on github is saved as a csv inside the raw folder. Each daily report is saved as its own csv. Additionally, you can fill in month, day, and year arguments to specify a specific time period of data. 

```{r}
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")
covid_data %>% etl_extract(month = 3, day = 1:31, year = 2020)

# technically this is the same, bc there is no other years of data: 
# covid_data %>% etl_extract(month = 3, day = 1:31)
```

### etl_transform

### etl_load 

### More functions etl_init, etl_update

## Conclusion 

The covid package was written to extract COVID-19 data, transform it into a clean dataset, and load that clean dataset into a postgresql database. Efficiency and usability were important factors kept in mind during *covid's* development. Please check out the vignette .... to see how you can leverage the *covid* package for your data analysis needs. 





