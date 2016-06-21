normal_body <- 
'
*** =instructions
- instruction 1
- instruction 2

*** =hint
hint comes here

*** =pre_exercise_code
```{%1$s}
# pec
```

*** =sample_code
```{%1$s}
# sample code
```

*** =solution
```{%1$s}
# solution code
```

*** =sct
```{%1$s}
success_msg("Great work!")
```
'

mce_body <- 
"
*** =instructions
- option 1
- option 2
- option 3

*** =hint
hint

*** =pre_exercise_code
```{%1$s}
# pec
```

*** =sct
```{%1$s}
test_mc(2) # if 2 is the correct option.
```
"

rstudio_mce_body <- 
"
*** =instructions
- option 1
- option 2
- option 3

*** =hint
hint

*** =attachments
first_file: www.example.com/firstfile
second_file: www.example.com/secondfile

*** =sct
```{%1$s}
test_mc(2) # if 2 is the correct option.
```
"
  
video_body <- 
"
*** =video_link
//player.vimeo.com/video/154783078

*** =video_hls
//videos.datacamp.com/transcoded/000_placeholders/v1/hls-temp.master.m3u8
"
  
course_yaml_template <- 
"title: insert course title here
author_field: insert author name here
description: insert course description here
"

chapter_yaml_template <-
"---
title       : %s
description : %s
"
