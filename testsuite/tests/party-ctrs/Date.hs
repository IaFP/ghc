module Date where

-- | Convert day of year in the Gregorian or Julian calendars to month and day.
-- First arg is leap year flag.
dayOfYearToMonthAndDay :: Bool -> Int -> (Int,Int)
dayOfYearToMonthAndDay isLeap yd = findMonthDay (monthLengths isLeap) (clip 1 (if isLeap then 366 else 365) yd)

clip :: (Ord t) => t -> t -> t -> t
clip a _ x | x < a = a
clip _ b x | x > b = b
clip _ _ x = x

findMonthDay :: [Int] -> Int -> (Int,Int)
findMonthDay (n:ns) yd | yd > n = (\(m,d) -> (m + 1,d)) (findMonthDay ns (yd - n))
findMonthDay _ yd = (1,yd)

-- | The length of a given month in the Gregorian or Julian calendars.
-- First arg is leap year flag.
monthLength :: Bool -> Int -> Int
monthLength isLeap month' = monthLength' isLeap (clip 1 12 month')

monthLength' :: Bool -> Int -> Int
monthLength' isLeap month' = (monthLengths isLeap) !! (month' - 1)

monthLengths :: Bool -> [Int]
monthLengths True =
    [31,29,28,31,30,31,30,31,31,30,31,30,31]
    --J  F  M  A  M  J  J  A  S  O  N  D
monthLengths False =
    [31,28,31,30,31,30,31,31,30,31,30,31]
    --J  F  M  A  M  J  J  A  S  O  N  D



-- monthLengths isleap =
--     [31,if isleap then 29 else 28,31,30,31,30,31,31,30,31,30,31]
    --J        F                   M  A  M  J  J  A  S  O  N  D

-- | The Modified Julian Day is a standard count of days, with zero being the day 1858-11-17.
newtype Day = ModifiedJulianDay {toModifiedJulianDay :: Integer} deriving (Eq,Ord)

toJulianYearAndDay :: Day -> (Integer,Int)
toJulianYearAndDay (ModifiedJulianDay mjd) = (year,yd) where
    a = mjd + 678577
    quad = div a 1461
    d = mod a 1461
    y = min (div d 365) 3
    yd = fromInteger (d - (y * 365) + 1)
    year = quad * 4 + y + 1

toJulian :: Day -> (Integer,Int,Int)
toJulian date = (year,month,day) where
    (year,yd) = toJulianYearAndDay date
    (month,day) = dayOfYearToMonthAndDay (isJulianLeapYear year) yd

-- | Is this year a leap year according to the proleptic Julian calendar?
isJulianLeapYear :: Integer -> Bool
isJulianLeapYear year = (mod year 4 == 0)
