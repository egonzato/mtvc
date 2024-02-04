data(simwide)
cp.dataframe=mtvc(data=simwide,
                 origin='1970-01-01',
                 dates=c(FIRST_CHRONIC,FIRST_ACUTE,FIRST_RELAPSE),
                 complications=c(CHRONIC,ACUTE,RELAPSE),
                 start=DATETRAN,
                 stop=DLASTSE,
                 event=EVENT)
#
test_that(desc="Check real values",
          code ={tevent=as.numeric(cp.dataframe[42,'tevent'])
            tdep_acute=as.numeric(cp.dataframe[63,'tdep_acute'])
            stop=as.numeric(cp.dataframe[81,'stop'])
            start=as.numeric(cp.dataframe[56,'start'])
  expect_equal(object=tevent,expected=0);
  expect_equal(object=tdep_acute,expected=0);
  expect_equal(object=is.numeric(tdep_acute),expected=TRUE);
  expect_equal(object=stop,expected=66);
  expect_equal(object=start,expected=11);
} )
