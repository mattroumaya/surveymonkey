
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveymonkey

<!-- badges: start -->

<!-- badges: end -->

This package allows the user to access the SurveyMonkey API from R. You
can browse your surveys, pick one to fetch, and then, most importantly,
parse the fetched JSON result into a tidy data.frame.

The columns are even factors when appropriate\! They have knowledge of
missing levels that were asked on the survey, even if they’re missing
from the data because no one selected them\!

**This package is still in development and may not work on 100% of use
cases**, but will fetch the typical survey correctly and is worth a shot
before you go to wrangle yet another .csv or .xlsx export.

## Installation

This package is not on CRAN. And as this package is currently internal
to TNTP, you’ll need to take some extra steps to install from our
private GitHub repository This will probably take about 20 minutes.

#### 1\) Make a GitHub.com account.

This is good to have, if you don’t already have one. Even if you don’t
store personal code on GitHub yourself, you can open issues and comment
on them in other people’s repositories. Most of us use some variation of
our names as our
IDs.

#### 2\) Request access to the [tntp GitHub organization](https://github.com/tntp/).

Let Sam or Zay know your GitHub username and we will invite
you.

#### 3\) Get a GitHub access token (PAT) and put it in your .Renviron file.

Follow these [step-by-step
instructions](https://happygitwithr.com/github-pat.html#step-by-step) to
get a PAT and add it to your .Renviron file. If you did it right, after
you restart RStudio the token should print when you run
`Sys.getenv("GITHUB_PAT")`.

Now your RStudio instance can access TNTP’s private GitHub repos\! And
at this point you can install via the much more typical:

``` r
# install.packages("devtools")
devtools::install_github("tntp/surveymonkey")
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
