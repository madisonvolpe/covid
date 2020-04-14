DROP TABLE IF EXISTS covid_stats;

CREATE TABLE covid_stats(
province_state text NOT NULL,
country_region text NOT NULL,
last_update timestamp NOT NULL,
confirmed numeric,
deaths numeric,
recovered numeric,
UNIQUE(province_state, country_region, last_update)
);
