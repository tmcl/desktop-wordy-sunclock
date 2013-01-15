
import Test.HUnit
import Data.Time
import Data.Time.Calendar.OrdinalDate
--import TimeToHoratime

main = runTestTT tests

tests = TestList [ TestLabel "sunday before 13.01.13 is  6.01.13" before13,
                   TestLabel "sunday before 14.01.13 is 13.01.13" before14, 
                   TestLabel "sunday before 15.01.13 is 13.01.13" before15, 
                   TestLabel "sunday before 16.01.13 is 13.01.13" before16, 
                   TestLabel "sunday before 17.01.13 is 13.01.13" before17, 
                   TestLabel "sunday before 18.01.13 is 13.01.13" before18, 
                   TestLabel "sunday before 19.01.13 is 13.01.13" before19 
                 ]

sundayBefore day = addDays (-(toInteger . snd . mondayStartWeek) day) day


before13 = TestCase $ sundayBefore (fromGregorian 2013 01 13) @?= (fromGregorian 2013 01 06)
before14 = TestCase $ sundayBefore (fromGregorian 2013 01 14) @?= (fromGregorian 2013 01 13)
before15 = TestCase $ sundayBefore (fromGregorian 2013 01 15) @?= (fromGregorian 2013 01 13)
before16 = TestCase $ sundayBefore (fromGregorian 2013 01 16) @?= (fromGregorian 2013 01 13)
before17 = TestCase $ sundayBefore (fromGregorian 2013 01 17) @?= (fromGregorian 2013 01 13)
before18 = TestCase $ sundayBefore (fromGregorian 2013 01 18) @?= (fromGregorian 2013 01 13)
before19 = TestCase $ sundayBefore (fromGregorian 2013 01 19) @?= (fromGregorian 2013 01 13)
   
