# SEIFA-RA-app
A Shiny app that assigns SEIFA scores and Remoteness Areas to cases based upon year and postcode.

Values are assigned from the most appropriate Australian census given each case's year value (2006, 2011, or 2016 census).

See the Australian Bureau of Statistics website for information about [SEIFA](https://www.abs.gov.au/websitedbs/censushome.nsf/home/seifa) and [Remoteness Areas](https://www.abs.gov.au/websitedbs/D3310114.nsf/home/remoteness+structure).

The app was designed for use by the South Australian Child Death and Serious Injury Committee but can be adapted to any purpose where cases include a year (e.g. year of birth or year of death) and a 4-digit Australian postcode. There are no constraints on other fields. The app will process some variations of variable names and handles both .xlsx and .csv files (outputting processed data in the same format).
