# Using RUnit: 
# https://cran.fhcrc.org/web/packages/RUnit/index.html

library(RUnit)

test.suite <- defineTestSuite("life", getwd(), testFileRegexp = "lifeTests\\.R")
printTextProtocol(runTestSuite(test.suite))