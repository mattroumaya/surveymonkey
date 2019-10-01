
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveymonkey

<!-- badges: start -->

<!-- badges: end -->

This package provides access the SurveyMonkey API from R. You can browse
your surveys, pick one to fetch, and then, most importantly, parse the
fetched JSON result into a tidy data.frame.

## Why this is useful

Compared to downloading .csv files manually:

  - No fussing with the web browser or logging in
  - Column names are handled appropriately - not split over two rows
  - The columns are factors when appropriate, with the levels ordered
    based on the sequence the answers appear in the survey.
      - And they have knowledge of all choices that were offered on the
        survey, even if they’re missing from the data because no one
        selected them. These values would be absent from a .csv
        download.

**This package is still in development and may not work on 100% of use
cases**, but will fetch the typical survey correctly and is worth a shot
before you go to wrangle yet another .csv or .xlsx export.

## Installation

This package is not yet on CRAN. Install from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("tntp/surveymonkey")
```

## Usage

### Authentication

Add the SurveyMonkey account’s OAuth token to your .Rprofile file. To
open and edit that file, run `usethis::edit_r_profile()`, then add a
line like this: `options(sm_oauth_token =
"kmda332fkdlakfld8am7ml3dafka-dafknalkfmalkfad-THIS IS NOT THE REAL KEY
THOUGH")`

Except that’s not the real OAuth token. You’ll get the real OAuth token
from this URL after you log in:
<https://developer.surveymonkey.com/apps/8804/settings/> where it’s
listed as your “Access Token”.

*Developer’s note: That’s how a user can get a single account’s OAuth
token. It might be preferable if users could authenticate from with R.
If someone has guidance for how users should obtain their OAuth token
more generally, please submit a pull request or comment in an issue.*

If this is set up successfully, the token will print when you run
`getOption("sm_oauth_token")`. Guard this token: don’t share it and
don’t commit it in any repository.

*Whew, you’re set up\! Give yourself a treat.*

### Browsing your surveys

You’ll need the ID number of the survey you wish to fetch. Find it by
browsing your surveys like this:

``` r
surveys <- browse_surveys(200) # see your most recent 200 surveys
```

Then run `View(surveys)` and start typing the name of the survey you
want into the search window at the top right of the RStudio data.frame
viewer. That will get you the survey’s ID number. Copy it.

### Fetching a survey

Get the survey data like this:

``` r
a_survey_obj <- fetch_survey_obj(123456789) # your survey's ID goes here
```

This returns a list of data about the survey. It’s useful for developers
to explore, but not so much for most users. Keep going to the next step.

### Parsing the survey into a data.frame

This is the actual good part.

``` r
survey_df <- parse_survey(a_survey_obj)
```

That will give you a tidy data.frame with all of your responses.

In the future you can run it all as one command:

``` r
survey_df <- 123456789 %>%
  fetch_survey_obj %>%
  parse_survey
```

## API considerations

Your account will likely be limited to 500 hits per day to the API. This
package will print reminders of how many calls you have left in the day.
The main thing to keep an eye on is respondent counts; as only 100
responses can be fetched per API call, a survey with X respondents will
make at least X/100 calls to the API.

## What’s missing

Not all data is yet pulled from the API. E.g., custom variables aren’t
currently pulled - if you have a use case for anything that isn’t
pulled, open an issue. All question types should be currently
implemented, but it’s possible that some obscure type isn’t.

# Contact

Something may not work, because this is an in-development package. Or
you may have an idea for something that hasn’t been implemented yet. In
either case, please create an issue in the GitHub issue tracker\!
