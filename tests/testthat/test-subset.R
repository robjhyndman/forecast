# A unit test for subset function
if(require(fpp) & require(testthat))
{
  context("Tests on input")
  
  mtsobj<-ts(matrix(rnorm(200),ncol=2),freq=4)
	test_that("tests specifying correct argument", {
	  sub<-subset(a10,month="September")
	  expect_that(length(sub), equals(tsp(sub)[2]-tsp(sub)[1]+1))
	  expect_that(round(sum(sub)), equals(174))
	  sub2<-subset(a10,month="SEPT")
	  expect_identical(sub,sub2)
	  sub2<-subset(a10,month=9)
	  expect_identical(sub,sub2)
	  sub2<-subset(a10,season=9)
	  expect_identical(sub,sub2)
	  sub<-subset(woolyrnq,quarter=1)
	  expect_that(length(sub), equals(tsp(sub)[2]-tsp(sub)[1]+1))
	  expect_that(sum(sub), equals(153142))
	  sub2<-subset(woolyrnq,season=1)
	  expect_identical(sub,sub2)
	  sub<-subset(a10,subset=a10<10)
	  expect_that(round(sum(sub)), equals(670))
	  expect_that(length(sub), equals(109))
	  sub<-subset(mtsobj, c(1,1,rep(0,98))==1)
	  expect_that(ncol(sub), equals(2))
	  expect_that(nrow(sub), equals(2))
	  sub<-subset(mtsobj, quarter=1)
	  expect_that(ncol(sub), equals(2))
	  expect_that(nrow(sub), equals(25))
	})
	
	test_that("tests specifying wrong argument", {
	  expect_error(subset(a10,quarter=1), "Data is not quarterly")
	  expect_error(subset(woolyrnq,month="January"), "Data is not monthly")
	})
	
	test_that("test for bad input", {
	  expect_error(subset.ts(mtcars,quarter=1), "Data must be seasonal")
	  expect_error(subset(a10,subset=c(1,2)), "subset must be the same length as x")
	  expect_error(subset(mtsobj, mtsobj<.5), "subset must be a vector of rows to keep")
	  expect_error(subset(a10,month="Jaan"), "No recognizable months")
	  expect_error(subset(a10,season=1:14), "Seasons must be between 1 and 12")
	  expect_error(subset(a10,month=1:14), "Months must be between 1 and 12")
	  expect_error(subset(woolyrnq,quarter="qq1"), "No recognizable quarters")
	  expect_error(subset(woolyrnq,quarter=1:6), "Quarters must be between 1 and 4")
	})
}
