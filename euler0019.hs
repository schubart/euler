isLeapYear year = year `mod` 4 == 0 &&
                year `mod` 100 /= 0 || year `mod` 400 == 0

isMonthEnd [year, month, day]
    | month == 2 && not (isLeapYear year) = day == 28
    | month == 2 && isLeapYear year       = day == 29
    | month `elem` [4, 6, 9, 11]          = day == 30
    | otherwise                           = day == 31

isYearEnd date@[_, month, _] = month == 12 && isMonthEnd date

next date@[year, month, day]
     | isYearEnd date  = [year + 1, 1,               1]
     | isMonthEnd date = [year,     month + 1,       1] 
     | otherwise       = [year,     month,     day + 1]

main = print $ length $ filter isSolution $ zipWith (:) weekdays dates
    where
      dates = takeWhile (\(y:_) -> y <= 2000) $ iterate next [1900, 1, 1]
      weekdays = cycle [0..6] -- 0: Monday, 6: Sunday
      isSolution [wday, year, _, day] = wday == 6 &&
                                        year > 1900 &&
                                        day == 1

