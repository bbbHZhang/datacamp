![banner](https://s3.amazonaws.com/assets.datacamp.com/img/github/content-engineering-repos/datacamp_banner.png)

[![Build Status](https://api.travis-ci.org/datacamp/datacamp.svg?branch=master)](https://travis-ci.org/datacamp/datacamp)
[![codecov.io](https://codecov.io/github/datacamp/datacamp/coverage.svg?branch=master)](https://codecov.io/github/datacamp/datacamp?branch=master)

Everybody can teach on DataCamp with the `datacamp` R package. It helps you in geting started on creating DataCamp course files, which are simple Markdown files. Next, you can upload these course files to DataCamp's servers with easy-to-use functions, making the course available for everybody on DataCamp.

## Installation

```R
# Required
install.packages("devtools")
library(devtools)
install_github("datacamp/datacamp")

# Recommended
install_github("datacamp/datacampAPI")
install_github("datacamp/testwhat")
```

## Getting Started

```
library(datacamp)

# Log in to DataCamp through R
datacamp_login()

# Author a new course in your current working directory
author_course(lang = "r")

# Upload your course to DataCamp
upload_course()
```

## Wiki Docs

Detailed documentation on using the `datacamp` package can be found in the [wiki](https://github.com/datacamp/datacamp/wiki).

## Questions or problems?

For more details, questions and suggestions, you can contact support@datacamp.com.