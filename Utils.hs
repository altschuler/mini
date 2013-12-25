module Utils where

bin2dec = bin2dec' 0
bin2dec' acc "" = acc
bin2dec' acc (x:xs) = bin2dec' (2 * acc + if x == '0' then 0 else 1) xs
