normal_body <- 
"
*** =instructions
- instruction 1
- instruction 2

*** =hint
hint comes here

*** =pre_exercise_code
```{%1$s}
# pec comes here
```

*** =sample_code
```{%1$s}
# sample code comes here
```

*** =solution
```{%1$s}
# Solution code
```

*** =sct
```{%1$s}
# sct code comes here
```
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
```{%1$s}
# pec comes here
```

*** =sct
```{%1$s}
test_mc(2) # if 2 is the correct option.
```
"
  
video_body <- 
"
*** =video_link
//player.vimeo.com/video/108225030
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
