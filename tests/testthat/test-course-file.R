context("course_file")

clean_up = function() {
  unlink("course.yml")
  unlink("chapter1.Rmd")
  unlink("chapter2.Rmd")
  unlink("chapter3.Rmd")
}

test_that("load_course_file works as expected", {
  # clean up before you start
  clean_up()
  
  # no course yml present -> throw errr
  expect_error(load_course_file())
  
  # not enough names
  write("title: test\nauthor_field: testauthor\n", file = "course.yml")
  expect_error(load_course_file())
  
  # some names don't contain anything
  write("title: test\nauthor_field: testauthor\ndescription:\n", file = "course.yml")
  expect_error(load_course_file())
  
  # incorrect chapters (no id)
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n chapter1.Rmd:\n", file = "course.yml")
  expect_error(load_course_file())
  
  # incorrect chapters (same ids)
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n chapter1.Rmd: 1\n chapter2.Rmd: 1\n", file = "course.yml")
  expect_error(load_course_file())
  
  # incorrect chapters (same file names)
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n chapter1.Rmd: 1\n chapter1.Rmd: 2\n", file = "course.yml")
  expect_error(load_course_file())
  
  # should be fine
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n chapter1.Rmd: 1\n chapter2.Rmd: 2\n", file = "course.yml")
  course = datacamp:::load_course_file()
  expect_equal(course$title, "test")
  expect_equal(course$author_field, "testauthor")
  expect_equal(course$description, "testdescription")
  expect_equal(names(course$chapters)[1], "chapter1.Rmd")
  expect_equal(course$chapters$chapter1.Rmd, 1)
  expect_equal(names(course$chapters)[2], "chapter2.Rmd")
  expect_equal(course$chapters$chapter2.Rmd, 2)
  
  # clean up before you stop
  clean_up()
})

test_that("add_id_to_course_file works as expected", {
  # clean up before you start
  clean_up()
  
  # no id there yet
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\n", file = "course.yml")
  add_id_to_course_file(1)
  expect_equal(load_course_file()$id, 1)
  
  # add other id than the one already available (course.yml contains id: 1)
  expect_error(add_id_to_course_file(2))
  
  # clean up before you stop
  clean_up()
})

test_that("get_chapter_index works as expeced", {
  # clean up before you start
  clean_up()
  
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n- chapter1.Rmd: 1\n- chapter2.Rmd: 2\n", file = "course.yml")
  expect_equal(get_chapter_index("chapter1.Rmd"), 1)
  expect_equal(get_chapter_index("chapter2.Rmd"), 2)
  expect_equal(length(get_chapter_index("chapter3.Rmd")), 0)
  
  clean_up()
})


test_that("add_chapter_to_course_file works as expeced", {
  # clean up before you start
  clean_up()
  
  write("testchapter1", file = "chapter1.Rmd")
  write("testchapter2", file = "chapter2.Rmd")
  write("title: test\nauthor_field: testauthor\ndescription: testdescription\nchapters:\n chapter1.Rmd: 1\n chapter2.Rmd: 2\n", file = "course.yml")
  
  # it's already in there
  expect_true(is.null(add_chapter_to_course_file("chapter1.Rmd",5)))
  
  # not yet in there
  write("testchapter3", file = "chapter3.Rmd")
  expect_message(add_chapter_to_course_file("chapter3.Rmd",3))
  course = load_course_file()
  expect_equal(names(course$chapters)[3], "chapter3.Rmd")
  expect_equal(course$chapters$chapter3.Rmd, 3)
  
  clean_up()
})