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
The above code connects to a postgresql database, creates a covid_data obkect and specifies the object's database and directory, and finally running etl_extract, etl_transform, and etl_load on covid_data will extract all available data from the github repository, transform it, and then load it into the specified postgresql database. 

## In-depth Explanations of Functions

## etl_extract

## etl_transform

## etl_load 





