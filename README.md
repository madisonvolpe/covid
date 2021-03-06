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

etl_extract, etl_transform, and etl_load basically do what their names suggest, however there are arguments you can add to these functions to specify specific subsets of data that you want. Likewise, there is functionality under the hood that should be explained so that you are aware of the data processing behind the scenes. 

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

*etl_transform* will take the data saved in the raw folder and transform it in order to be loaded into the specified database. Transform cleans data and only keeps relevant columns. Under the hood, *etl_transform* takes the specified csvs in the raw folder, reads them into R and then applies cleaning procedures. The cleaning procedures include: cleaning column names to be in snake_case, cleaning the date/time (last_update) column so that the formatting is standardized across all observations, removing any unnecessary punctuation inside character variables, and finally selecting only relevant columns (**admin, province_state, country_region, last_update, confirmed, deaths, recovered**).

```r
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")
covid_data %>% etl_extract(month = 3, day = 1:31, year = 2020) %>% etl_transform(month = 3, day = 1:5, year = 2020) 
```
After creating the etl_covid object, and running *etl_extract* on it, we are left with 31 csvs for the month of March in the raw directory. *etl_transform* will read in the data from the raw directory, transform it using the cleaning methods described above and then save the cleaned csvs into the load folder. The transformed csvs are saved in the following format: 2020-03-05.csv. Because we specified month, day, and year parameters, only 5 csvs will be saved into the load directory because we specified that we only wanted to clean data for the first 5 days in March. 

### etl_load 

*etl_load* reads in the transformed csvs from the load folder and manipulates them to be uploaded to the postgresql database that the user specified. Under the hood, prior to uploading the data into the database, *etl_load* performs some operations to create an accurate and efficient database. First, any duplicate values are removed so that only distinct values remain. Second, rows where the original data had NA values for all confirmed, recovered, and deaths are removed. Finally, there were some cases in the original data files from CSSEGISandData, where the last_update column for province_state & country_region had two different sets of values for confirmed, recovered, and deaths. For example, in the table below, we can see that on 3-14-2020 20:13:16, there are two different sets of data for Italy. This is but one example, because in actuality there are many more like this, especially for 3-14-2020. In this case,  before uploading into the database, *etl_load* will keep observations for last_update that have the highest summed value of confirmed, deaths, and recovered. In this case, the second row from the table below is kept, while the first row is discarded. This is done in order to only have one set of values for each last_update time interval. 

| admin | province_state | country_region | last_update        | confirmed | deaths | recovered |
|-------|----------------|----------------|--------------------|-----------|--------|-----------|
|  NA   |      NA        |     Italy      | 3-14-2020 20:13:16 |  21187    |  1441  |   1966    |
|  NA   |      NA        |     Italy      | 3-14-2020 20:13:16 |  24747    |  1809  |   2535    |

After performing these final steps, described above, the *etl_load* function is written as a SQL upsert so that you can continually update your database without adding duplicate values. In this case, there is a UNIQUE constraint on admin, province_state, country_region, and last_update. If there is a conflict then the confirmed, deaths, and recovered columns are updated to match the new data being uploaded. 

```r
covid_data <- etl("covid", db = covid_db, dir = "/Users/madisonvolpe/Documents/covid_data")

covid_data %>% 
  etl_extract(month = 3, day = 1:31, year = 2020) %>% 
  etl_transform(month = 3, day = 1:5, year = 2020) %>% 
  etl_load(db_con = covid_db$con)
```
The above code shows that after creating the covid_data object and running *etl_extract* and *etl_transform* on the object, you can pipe in *etl_load*. Be sure to add the database connection and then the data will be uploaded to your postgresql database. In sum *etl_load*, will read in the files stored in the load folder and modify them before directly uploading them to the database. In this case, only data from March 1st to March 5th will be added to your database because these are the only five files saved into the load folder after running *etl_transform*.

### More functions etl_init, etl_update

*etl_init* from Benjamin Baumer's [etl](https://github.com/beanumber/etl) package will run the postgresql script that will create the covid_stats table inside your specified postgresql database. This is what is being created, you can see the unique constraint on admin, province_state, country_region, and last_update described above. 

```sql
DROP TABLE IF EXISTS covid_stats;

CREATE TABLE covid_stats(
admin text NOT NULL,
province_state text NOT NULL,
country_region text NOT NULL,
last_update timestamp NOT NULL,
confirmed numeric,
deaths numeric,
recovered numeric,
UNIQUE(admin, province_state, country_region, last_update)
);

```
*etl_update*, again from Benjamin Baumer's etl package, allows you to run one line of code that automatically performs the etl_extract, etl_transform, and etl_load pipeline. *etl_update* is useful when you want to add data to an already populated database.  

```r
# initial download [downloaded data up until may 4th] 

covid_data %>% etl_extract() %>% etl_transform %>% etl_load(db_con = covid_db$con)

# etl update [next day]

covid_data %>% etl_update(month = 5, day = 5, year = 2020, db_Con = covid_db$con)
```

For example, let's say you run the initial pipe on May 5th, which will get you all data up until May 4th. The next day instead of having to write the entire pipe, you can run *etl_update* with month, day, and year arguments. It will supply these arguments to *etl_extract*, *etl_transform*, and *etl_load* so only data for May 5th is extracted, transformed, and loaded into the specified database. This allows you to update your database daily :smile:

## Conclusion 

The covid package was written to extract COVID-19 data, transform it into a clean dataset, and load that clean dataset into a postgresql database. Efficiency and usability were important factors kept in mind during *covid's* development. Please check out the vignette to see how you can leverage the *covid* package for your data analysis needs. 





