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

To use the package you must load the **etl** and **covid** packages. After loading the **covid** package, you must create an etl_covid object after creating the object...you can run the *etl_extract*, *etl_transform*, and *etl_load* functions.

```r
library(etl)
library(covid)

# connecting to db 
covid_db <- src_postgres(dbname = "covid_db", host = 'localhost', port = 5432,
                           user = Sys.getenv("user"), password = Sys.getenv("password"))
# creating covid object
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")

# creating table inside postgresql database
covid_data %>% etl_init(schema_name = "init")

# running etl_extract, etl_transform, etl_load
covid_data %>% etl_extract() %>% etl_transform() %>% etl_load(db_con = covid_db$con)
```
The above code connects to a postgresql database, creates an etl_covid object that specifies the directory where data will be stored, as well as, the database where data will be loaded. By way of the *etl_init* function, it also creates a table to capture data inside the specified database. Finally, running etl_extract, etl_transform, and etl_load on covid_data will extract all available data from the github repository, transform it, and then load it into the specified postgresql database. 

## In-depth Explanations of Functions

Etl_extract, etl_transform, and etl_load basically do what their names suggest, however there are arguments you can add to these functions to specify specific subsets of data that you want. Likewise, there is functionality under the hood that should be explained so that you are aware of the data processing behind the scenes. 

### etl_extract

*etl_extract* simply captures the COVID-19 daily report data from the repository linked above and saves it into the directory that you specified when creating the etl_covid object. Specifically, when you create the etl_covid object and specify the directory, raw and load folders are created inside the directory. *etl_extract* will extract the daily report data files and save them inside the raw folder. No other modifications are made to the data and each daily report is saved as its own csv inside the raw folder. Additionally, you can fill in month, day, and year arguments to capture data for a specific time period. 

```r
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")
covid_data %>% etl_extract(month = 3, day = 1:31, year = 2020)

# technically this is the same, bc there is no other years of data: 
# covid_data %>% etl_extract(month = 3, day = 1:31)
```

The above code shows that after creating the etl_covid object, covid_data, you can run *etl_extract* on covid_data using the pipe. Specifying, month, day, and year will extract data based on that time period only. In this case, all covid daily reports for March 2020 will be extracted and saved in the raw folder inside the directory ("/Users/madisonvolpe/Documents/covid_data"). After running this code, you would expect to find 31 csvs inside the raw folder. They are saved in the following format: 2020-03-01.csv. 

### etl_transform

*etl_transform* 


### etl_load 

### More functions etl_init, etl_update

## Conclusion 

The covid package was written to extract COVID-19 data, transform it into a clean dataset, and load that clean dataset into a postgresql database. Efficiency and usability were important factors kept in mind during *covid's* development. Please check out the vignette .... to see how you can leverage the *covid* package for your data analysis needs. 





