Access the SurveyMonkey API
====================================

Setup
-------------
Get an OAuth token for your SurveyMonkey account and add it to your .Renviron file like this:
`sm_oauth_token = "dslkajalsgjfaskgfjaslknfel`


Usage
---------
Workflow for retrieving survey data:
* `browse_surveys()` returns a data.frame of your surveys
* `about_survey` returns information about a survey
* `get_survey()` returns a survey's responses. You can specify an ID or name.
