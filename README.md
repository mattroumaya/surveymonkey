
<!-- README.md is generated from README.Rmd. Please edit that file -->

# surveymonkey

<!-- badges: start -->

<!-- badges: end -->

This package allows the user to access the SurveyMonkey API from R. You
can browse your surveys, pick one to fetch, and then, most importantly,
parse the fetched JSON result into a tidy data.frame.

## Why this is useful

Compared to downloading .csv files manually:

  - No fussing with the web browser or logging in
  - Column names are handled appropriately
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

This package is not on CRAN. And as this package is currently internal
to TNTP, you’ll need to take some extra steps to install from our
private GitHub repository. This requires hopping through some tedious
hoops and may take about 20-30 minutes. I’ve tried to document these to
make it less painful.

### Install development dependencies

This package uses the `pivot_wide()` function from tidyr 0.8.3.9000, the
development version of tidyr. Check your version number with
`devtools::session_info()`; if your version is 0.8.3 or earlier, you
will need to update. You can install the development version of tidyr
with `devtools::install_github("tidyverse/tidyr")`…

… be aware that this may set off a cascade of necessary installations,
as the development version of `tidyverse/tidyr` requires the development
version of `r-lib/vctrs`, which requires the development version of
`r-lib/rlang` and `r-lib/ellipsis` and `r-lib/pillar`. So if
installation of one fails, note what dependency is mentioned in the
error message and try installing it from GitHub. Eventually, working
your way up from the bottom, they should install. You are done here when
`devtools::install_github("tidyverse/tidyr")` succeeds.

(This annoyance will be moot when these packages go to CRAN,
eventually.)

### Set up account access to GitHub and SurveyMonkey

#### 1\) Make a GitHub.com account.

This is good to have in general, if you don’t already have one. Even if
you don’t store personal code on GitHub yourself, you can open issues
and comment on them in other people’s repositories. Most of us use some
variation of our names as our
IDs.

#### 2\) Request access to the [tntp GitHub organization](https://github.com/tntp/).

Let Sam or Zay know your GitHub username and we will invite
you.

#### 3\) Get a GitHub access token (PAT) and put it in your .Renviron file.

Follow these [step-by-step
instructions](https://happygitwithr.com/github-pat.html#step-by-step) to
get a PAT and add it to your .Renviron file. This is set up correctly
if, after you restart RStudio, the token prints when you run
`Sys.getenv("GITHUB_PAT")`.

Now your RStudio instance can access TNTP’s private GitHub repos\! And
at this point you can install via the much more typical:

``` r
# install.packages("devtools")
devtools::install_github("tntp/surveymonkey")
```

## Usage

### Authentication

Add the SurveyMonkey account’s OAuth token to your .Rprofile file. To
open and edit that file, run `usethis::edit_r_profile()`, then add a
line like this: `sm_oauth_token =
"kmda332fkdlakfld8am7ml3dafka-dafknalkfmalkfad-THIS IS NOT THE REAL KEY
THOUGH"`

Except that’s not the real OAuth token. You’ll get the real OAuth token
from this URL after you log in:
<https://developer.surveymonkey.com/apps/8804/settings/> where it’s
listed as your “Access Token”.

*Editor’s note: I believe this ^^ is a specific to a single TNTP
SurveyMonkey account. If someone has guidance for how users should
obtain their OAuth token, more generally, please send to Sam or submit a
pull request.*

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
