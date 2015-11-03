normal_body <- 
"
*** =instructions
- instruction 1
- instruction 2

*** =hint
hint comes here

*** =pre_exercise_code
```{prog_lang}
# pec comes here
```
%s
*** =solution
```{prog_lang}
# Solution code
```
%s
"

mce_body <- 
"
*** =instructions
- option 1
- option 2
- option 3

*** =hint
hint comes here

*** =pre_exercise_code
```{prog_lang}
# pec comes here
```

*** =sct
```{prog_lang}
test_mc(2) # if 2 is the correct option.
```
"
  
video_body <- 
"
*** =video_link
//player.vimeo.com/video/108225030
"

sample <- 
"
*** =sample_code
```{prog_lang}
# sample code comes here
```
"
  
sct <- 
"
*** =sct
```{prog_lang}
# sct code comes here
```
"
  
course_yaml_template <- 
"title: insert course title here
author_field: insert author name here
description: insert course description here
"

chapter_yaml_template <-
"---
title_meta  : Chapter %s
title       : %s
description : %s
"
