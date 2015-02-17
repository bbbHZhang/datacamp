context("course_yml")

clean_up = function() {
  unlink("course.yml")
  unlink("chapter1.Rmd")
  unlink("chapter2.Rmd")
  unlink("chapter3.Rmd")
}

test_that("load_course_yml works as expected", {
  # clean up before you start
  clean_up()
  
  # no course yml present -> throw errr
  expect_that(load_course_yml(), throws_error())
  
  # not enough names
  write("title: test\nauthor_field: testauthor\n", file = "course.yml")
  expect_that(load_course_yml(), throws_error())
  
  # some names don't contain anything
  write("title: test\nauthor_field: testauthor\ndescription:\n", file = "course.yml")
  expect_that(load_course_yml(), throws_error())
  
  # incorrect chapters (no id)
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd:\n", file = "course.yml")
  expect_that(load_course_yml(), throws_error())
  
  # incorrect chapters (same ids)
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter2.Rmd: 1\n", file = "course.yml")
  expect_that(load_course_yml(), throws_error())
  
  # incorrect chapters (same file names)
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter1.Rmd: 2\n", file = "course.yml")
  expect_that(load_course_yml(), throws_error())
  
  # should be fine
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter2.Rmd: 2\n", file = "course.yml")
  course = datacamp:::load_course_yml()
  expect_that(course$title, equals("test"))
  expect_that(course$author_field, equals("testauthor"))
  expect_that(course$description, equals("testdescription"))
  expect_that(names(course$chapters[[1]]), equals("chapter1.Rmd"))
  expect_that(course$chapters[[1]]$chapter1.Rmd, equals(1))
  expect_that(names(course$chapters[[2]]), equals("chapter2.Rmd"))
  expect_that(course$chapters[[2]]$chapter2.Rmd, equals(2))
  
  # clean up before you stop
  clean_up()
})

test_that("add_id_to_course_yml works as expected", {
  # clean up before you start
  clean_up()
  
  # no id there yet
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\n", file = "course.yml")
  result = try(suppressMessages(add_id_to_course_yml(1)))
  expect_that(inherits(result,"try-error"), is_false())
  expect_that(load_course_yml()$id, equals(1))
  
  # add other id than the one already available (course.yml contains id: 1)
  expect_that(add_id_to_course_yml(2), throws_error())
  
  # clean up before you stop
  clean_up()
})

test_that("get_chapter_id works as expeced", {
  # clean up before you start
  clean_up()
  
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter2.Rmd: 2\n", file = "course.yml")
  expect_that(get_chapter_id("chapter1.Rmd"), equals(1))
  expect_that(get_chapter_id("chapter2.Rmd"), equals(2))
  expect_that(length(get_chapter_id("chapter3.Rmd")), equals(0))
  
  clean_up()
})


test_that("add_chapter_to_course_yml works as expeced", {
  # clean up before you start
  clean_up()
  
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter2.Rmd: 2\n", file = "course.yml")
  
  # it's already in there
  expect_that(is.null(add_chapter_to_course_yml("chapter1.Rmd",5)), is_true())
  
  # not yet in there
  write("testchapter3", file = "chapter3.Rmd")
  expect_that(add_chapter_to_course_yml("chapter3.Rmd",3), shows_message())
  course = load_course_yml()
  expect_that(names(course$chapters[[3]]), equals("chapter3.Rmd"))
  expect_that(course$chapters[[3]]$chapter3.Rmd, equals(3))
  
  clean_up()
})