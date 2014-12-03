[![Build Status](https://api.travis-ci.org/Data-Camp/datacamp.svg?branch=master)](https://travis-ci.org/Data-Camp/datacamp)
<p align="center">
<img src="https://s3.amazonaws.com/assets.datacamp.com/img/logo/logo_blue_full.svg" width="250">
</p>

# Build your own interactive course(s)!
## Introduction
Creating interactive courses for data analysis with R can and should be easy. This tutorial explains **how you can create interactive courses** on the DataCamp platform. Furthermore, the tutorial explains the **Submission Correctness Test** - the key concept to add interactivity to your course or tutorial.

## Creating and managing interactive courses
Courses are created locally using R Markdown files and managed with a complementary web interface on datacamp.com.

- **The R interface - Use R Markdown and the datacamp R package to create interactive courses:**<br>
The datacamp R package enables you to write interactive R courses in R Markdown (e.g. using RStudio locally) and to then upload them to the datacamp.com platform. The DataCamp package is essentially a wrapper around the [slidify](http://slidify.org/) package. This way, you can also use existing R Markdown files as the basis of your interactive courses and collaborate easily with others on courses (e.g. by sharing the Markdown files on GitHub). Have a look at the [R Markdown files](https://github.com/data-camp/introduction_to_R) of the "Introduction to R" course.
- **The web interface - Course management:**<br>
After uploading chapters to datacamp.com, you can use the [webinterface](https://teach.datacamp.com) to view and to organize your courses with their chapters. You can use this interface to make sure that your course was uploaded correctly, to make various changes to the course's state and to get a preview on how the user will see the course.

## Getting started

###Install the datacamp R package
```ruby
install.packages("devtools")
library("devtools")
install_github("datacampSCT", "data-camp")
install_github("datacamp", "data-camp")
install_github("slidify", "ramnathv", ref = "dev")
install_github("slidifyLibraries", "ramnathv")
library("datacamp")
```
The code above will download and install the datacamp R package. Alternatively, you can also download the source code above.

###Scaffold a course with chapter
   ```ruby
author_course("course_name")
```
The `author_course` function will:
  1. create a folder in your current working directory with the name "course_name"
  2. create a `course.yml` file, a scaffold to create your course
  3. create a `chapter1.Rmd` file, a scaffold for creating your first chapter
The generated template contains extra instructions on the building blocks of chapters. We strongly advise you to put your folder under git source control at this point.

###Directory structure
Every course consists of a `course.yml` file along with various `.Rmd` chapter files. An example course is structured as follows:
   ```
.
|__ course.yml
|__ chapter1.Rmd
|__ chapter2.Rmd
|__ chapter3.Rmd
|__ ...
```
Note that the chapter `.Rmd` files can have any chosen name.

###Log in to datacamp.com
   ```ruby
datacamp_login()
```
This will prompt you for your username and password to log into the datacamp server. Note that you must also log into datacamp.com with your browser.

###The `course.yml` file
This file contains information about your course and the structure of its chapters. After running `author_course()`, a `course.yml` file is created in your course directory. This `course.yml` file has the following scaffold:
```yml
# course.yml scaffold
title: Insert the course title here
author_field: The author(s) name(s)
description: Insert course description here
```
Change the fields and call the function `upload_course()`. This will create your course and add its ID to the `course.yml` file. This way, your local course is linked to the one on datacamp.com.
```yml
# course.yml after creation on server
id: 314
title: Hello world
author_field: Your name
description: A first course
```

###Uploading chapters to datacamp.com
Every chapter is described in its corresponding `chapterX.Rmd` file. The first time that you create a chapter, it is a good idea to start from the `chapter1.Rmd` template that was generated using `author_course("course_name")`. This template contains information about the markup language, the different types of exercises, etc.
```yml
# chapter1.Rmd scaffold
--- 
title_meta  : Insert chapter preposition here
title       : Insert the chapter title here
description : What is this chapter about? Add here your description
framework   : datacamp
mode        : selfcontained

---
## The title of the first exercise
...
```
Adapt the scaffold and use the function `upload_chapter("chapter1.Rmd")` to add the chapter to your course on datacamp.com. This will create the chapter and add it to the `course.yml` file. Every time you add a new chapter to a course, it is added to the `course.yml` file. An example:
   ```yml
# e.g. course.yml
id: 314
title: Hello world
author_field: Your name
description: A first course
chapters:
  - chapter1.Rmd: 753
  - chapter2.Rmd: 760
```
As you can see, the file contains a list of chapters with a mapping of their file names on their ID. The order of the list dictates the order of the chapters within the course. This helps you to organize your course and to avoid overwriting chapters. For example, suppose we want chapter 2 to appear first on datacamp.com:
```yml
# course.yml
...
chapters:
  - chapter2.Rmd: 760
  - chapter1.Rmd: 753
```
Any changes to this list require the course to be uploaded with `upload_course()` for the changes to take effect.

###Share the love
A course is just a collection of chapters, and a chapter is essentially just a simple R Markdown document. Therefore, you can easily collaborate with others on a course. For example, by sharing the [R Markdown documents](https://github.com/data-camp/introduction_to_R) on GitHub. We believe this "open-course" type of collaboration for interactive R and statistics courses will benefit everyone. Check out the open-source repo of our "Introduction to R" course and help us improve it.


## Submission Correctness Tests
The key ingredient to an interactive course is the Submission Correctness Test (SCT). For all information regarding SCTs, please refer to the [datacampSCT package](https://github.com/Data-Camp/datacampSCT).

##Most frequently asked questions

#####How can I create a new chapter?
First, make sure that the current working directory matches with the course directory for which you want to create a chapter. Next, add a new chapter to the course with the help of the `author_chapter("chapter_name")` function. This will create and open an R Markdown file named `chapter_name.Rmd`.  
```
# Make sure to set the correct working directory

# Add a new chapter "R for dummies". This will open an R Markdown file.
author_chapter("R_for_dummies")
```
#####My chapter is finished, how do I upload it to the DataCamp platform?
If you already placed your course on the online DataCamp platform, you can upload a new or existing chapter via the function `upload_chapter("chapter_name")`. If your course is not yet on the online DataCamp platform, you first need to run the command `upload_course()`.
```
# Upload the chapter "R for dummies"
upload_chapter("R_for_dummies.Rmd")
```
#####I want to remove a chapter in my course. Can I do this?
Yes you can! To remove chapters out of a course, you need to go to your `course.yml` file. Here you will see a list of all the chapters included in your course (in this case two), with their chapter ID: 
```
# e.g. course.yml
id: 314
title: Hello world
author_field: Your name
description: A first course
chapters:
  - chapter1.Rmd: 753
  - chapter2.Rmd: 760
```
To remove a chapter from a course, you just need to remove the corresponding chapter name and ID in the Yaml file. So suppose you want to remove `chapter1.Rmd` from the course, your Yaml file needs to look like this:
```
# e.g. course.yml
id: 314
title: Hello world
author_field: Your name
description: A first course
chapters:
  - chapter2.Rmd: 760
```
To make these changes visible on the DataCamp platform, you need to re-upload the course via `upload_course()`.

<i>Note that removing the first chapter from the Yaml file does not remove the R Markdown file `chapter1.Rmd` in your course map. So chapter content will not get lost, and you can always re-add the chapter in a later phase.  

##### How can I add exercises to a chapter?  
Adding exercises to a chapter is easy. In every R Markdown file of a chapter, the start of a new exercise is indicated by `---[identifier]`, followed by the different components of an exercise: `## Exercise Title`, `*** =instructions`, `*** =hint`, `*** =pre_exercise_code`, `*** =solution`, `*** =sample_code`, and `*** =sct`. 

By now, there are three types of exercises:
- Interactive exercises, where the user is expect to produce code that will be checked for correctness when submitted. The start of this type of exercises is indicated simply by `---`. An example of the result can be found [here](https://www.datacamp.com/courses/introduction-to-r/chapter-1-intro-to-basics?ex=4). The code of the [_Introduction to R_ course](https://github.com/data-camp/introduction_to_R) provides tons of examples.
- Multiple choice exercises, where the user is given a list of options where he/she has to choose from. More information will be soon available.
- VideoExercises, where you can set the link to a Vimeo video. More information will be soon available.

You have an R Markdown with the following exercise (for clarity, only the components `## Exercise Title` and `*** =sct` are displayed):
```
## First Exercise
.
.
.  
*** =sct
```
Now you want to add a second exercise. You simply do this by adding the `---` sign after the `*** =sct` component of the first exercise:
```
## First Exercise
.
.
.
.
*** =sct

# AFTER SCT OF EXERCISE ONE, PLACE THE `---` SIGN TO INDICATE START EXERCISE TWO:
---
## Second Exercise
.
.
.
.
*** =sct
```
##### I need to delete one of the exercises in my chapter. How can I do this?
To delete an exercise from a chapter, you need to delete all the related components of this exercise. The following R Markdown has two exercises and you would like to delete the second one: 
```
## First Exercise
.
.
.
.
*** =sct
---
## Second Exercise
.
.
.
.
*** =sct
```
Now just delete all components of the second exercise. Make sure to delete the `---` sign that indicated the start of the second exercise: 
```
## First Exercise
.
.
.
.
*** =sct
```

###### If you still did not find the answer that you were looking for, just send an e-mail to <b>teach@datacamp.com</b>.  
