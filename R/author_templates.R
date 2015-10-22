build_templates <- function() {
  normal <- 
"--- type:NormalExercise lang:%s%s
## Normal Exericse Title

Assignment comes here. Use Markdown for text formatting.

*** =instructions
- instruction 1
- instruction 2

*** =hint
hint comes here
%s%s
*** =solution
```{%s}
# Solution code
```
%s
"
  
  multiple <- 
"--- type:MultipleChoiceExercise lang:%s%s
## Multiple Choice Exercise Title

Assignment comes here. Use Markdown for text formatting.

*** =instructions
- option 1
- option 2
- option 3

*** =hint
hint comes here
%s
*** =sct
```{%s}
test_mc(2) # if 2 is the correct option.
```
"
  
  video <- 
"--- type:VideoExercise lang:%s%s
## Video Title

*** =video_link
```{%s}
//player.vimeo.com/video/108225030
```
"
  
  pec <- 
"
*** =pre_exercise_code
```{%s}
# pec comes here
```
"
  
  sample <- 
"
*** =sample_code
```{%s}
# sample code comes here
```
"
  
  sct <- 
"
*** =sct
```{%s}
# sct code comes here
```
"
  
  normal_r_simple <- sprintf(normal, "r", "", "", "", "r", "")
  normal_r_full <- sprintf(normal, "r", " xp:100 skills:1", 
                           sprintf(pec, "r"), sprintf(sample, "r"), "r", sprintf(sct, "r"))
  mce_r_simple <- sprintf(multiple, "r", "", "", "r")
  mce_r_full <- sprintf(multiple, "r", " xp:50 skills:1", sprintf(pec, "r"), "r")
  video_r_simple <- sprintf(video, "r", "", "r")
  video_r_full <- sprintf(video, "r", " xp:50 skills:1", "r")
  
  normal_python_simple <- sprintf(normal, "python", "", "", "", "python", "")
  normal_python_full <- sprintf(normal, "python", " xp:100 skills:2", 
                                sprintf(pec, "python"), sprintf(sample, "python"), "pyhon", sprintf(sct, "python"))
  mce_python_simple <- sprintf(multiple, "python", "", "", "python")
  mce_python_full <- sprintf(multiple, "python", " xp:50 skills:2", sprintf(pec, "python"), "python")
  video_python_simple <- sprintf(video, "python", "", "python")
  video_python_full <- sprintf(video, "python", " xp:50 skills:2", "python")
  
  lang_col <- c(rep("r",6), rep("python", 6))
  type_col <- c(rep("NormalExercise", 2), rep("MultipleChoiceExercise", 2), rep("VideoExercise", 2))
  simple_col <- c(T,F)
  template_col <- c(normal_r_simple, normal_r_full, mce_r_simple, mce_r_full, video_r_simple, video_r_full,
                    normal_python_simple, normal_python_full, mce_python_simple, mce_python_full, video_python_simple, video_python_full)
  data.frame(lang_col, type_col, simple_col, template_col, stringsAsFactors = FALSE)
}

templates <- build_templates()

course_yml_template <- 
"title: insert course title here
author_field: insert author name here
description: insert course description here
"

chapter_yml_template <-
"---
title_meta  : Chapter 1
title       : Insert the chapter title here
description : What is this chapter about? Add here your description
"
