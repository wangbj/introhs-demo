import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import           Data.ByteString (ByteString)

import           Test.QuickCheck
-- ^reverse lines (first line become last, and vice versa)
-- and reverse each line
reverseReverse :: [ByteString] -> [ByteString]
reverseReverse = fmap C.reverse . reverse

reverseReverse' :: [ByteString] -> [ByteString]
reverseReverse' = reverse . fmap C.reverse

instance Arbitrary ByteString where
  arbitrary = fmap C.pack arbitrary

prop_reverser_a_b_equals_reverse_b_a :: [ByteString] -> Bool
prop_reverser_a_b_equals_reverse_b_a xs = reverseReverse xs == reverseReverse' xs

-- quickCheck prop_reverser_a_b_equals_reverse_b_a
-- +++ OK, passed 100 tests.

main = S.getContents >>= mapM_ C.putStrLn . reverseReverse . C.lines

-- cat /etc/motd | stack exec rr
