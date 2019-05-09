
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveymonkey

<!-- badges: start -->

<!-- badges: end -->

This package allows the user to access the SurveyMonkey API from R. You
can browse your surveys, pick one to fetch, and then, most importantly,
parse the fetched JSON result into a tidy data.frame.

The columns are even factors when appropriate\!

**This package is still in development and may not work on 100% of use
cases**, but will fetch the typical survey correctly and is worth a shot
before you go to wrangle yet another .csv or .xlsx export.

## Installation

This package is not on CRAN. Install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("sfirke/surveymonkey")
```

## Usage

### Authentication

First you’ll have to set up an OAuth token for your SurveyMonkey
account. Once you obtain it, put it in your .Renviron file like this:
`sm_oauth_token = "dslkajalsgjfaskgfjaslknfel"`. *Editor’s note: If
someone has guidance for how to do this, either their own or a guide
they found useful, please submit a pull request.*

### Browsing your surveys

You’ll need the ID number of the survey you wish to fetch. Find it by
browsing your surveys like this:

``` r
surveys <- browse_surveys() # see your latest 100 surveys
```

Then run `View(surveys)` and start typing the name of the survey you
want into the search window at the top right of the RStudio data.frame
viewer. That will get you the survey’s ID number. Copy it.

### Fetching a survey

Get the survey data like this:

``` r
a_survey_obj <- fetch_survey_obj(123456789) # your survey's ID goes here
```

### Parsing the survey into a data.frame

This is the actual good part.

``` r
survey_df <- parse_survey(a_survey_obj)
```

That will give you a tidy data.frame with all of your responses.

## API considerations

Your account will likely be limited to 500 hits per day to the API. This
package will print reminders of how many calls you have left in the day.
The main thing to keep an eye on is respondent counts, as only 100
responses can be fetched per API call.

## What’s missing

Not all data is yet pulled from the API. E.g., custom variables aren’t
currently pulled, nor Date Created or Date Modified. All question types
should be currently implemented, but it’s possible that some obscure
type isn’t.

# Contact

Something may not work, because this is an in-development package. Or
you may have an idea for something that hasn’t been implemented yet. In
either case, please create an issue in the GitHub issue tracker\!
