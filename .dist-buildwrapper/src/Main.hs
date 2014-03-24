
bmiTell :: (RealFloat a) => a -> IO[String]  
bmiTell bmi
       | bmi > 0 = "okay"
       | otherwise = "not okay"

bmiTell 5.0