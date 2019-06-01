
module Howard.Util where

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

eithers :: (a -> a -> a) -> Either b a -> Either b a -> Either b a
eithers f (Right x) (Right y) = Right $ f x y
eithers _ (Left x)  (Left y)  = Left x
eithers _ (Left _)  r         = r
eithers _ r         (Left _)  = r

plusEither :: Either a b -> Either a b -> Either a b
plusEither (Right b) _ = Right b
plusEither _         r = r
