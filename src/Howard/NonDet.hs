{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Howard.NonDet where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Class
import           Data.Foldable
-- import           Data.List.NonEmpty

-- data NonDet e a = Error [e]
--                 | Some (NonEmpty a)
--     deriving (Functor)

-- instance Semigroup (NonDet e a) where
--     Error xs <> Error ys = Error $ xs <> ys
--     Error _  <> Some ys  = Some ys
--     Some xs  <> Error _  = Some xs
--     Some xs  <> Some ys  = Some $ xs <> ys

-- instance Monoid (NonDet e a) where
--     mempty = Error []

-- instance Applicative (NonDet e) where
--     pure a = Some $ pure a
--     (<*>) = ap
--     -- Error xs <*> Error ys = Error $ xs <> ys
--     -- Error xs <*> Some _   = Error xs
--     -- Some _   <*> Error ys = Error ys
--     -- Some fs  <*> Some as  = Some $ fs <*> as

-- instance Monad (NonDet e) where
--     Error xs >>= _ = Error xs
    -- Some as  >>= f = _ $ f <$> as



newtype NonDetT e m a = NonDetT (m (Either [e] [a]))
    deriving Functor

runNonDetT :: NonDetT e m a -> m (Either [e] [a])
runNonDetT (NonDetT m) = m

mapNonDetT :: (m (Either [e] [a]) -> n (Either [e] [a]))
           -> NonDetT e m a -> NonDetT e n a
mapNonDetT f = NonDetT . f . runNonDetT

catchAmbiguity :: Monad m
               => NonDetT e m a -> ([a] -> NonDetT e m a) -> NonDetT e m a
catchAmbiguity (NonDetT m) h = NonDetT $ m >>= \case
    Left es  -> return $ Left es
    Right as -> runNonDetT $ h as

returns :: Monad m => [a] -> NonDetT e m a
returns [] = NonDetT . return $ Left []
returns as = NonDetT . return $ Right as

instance Monad m => Applicative (NonDetT e m) where
    pure a = NonDetT . pure $ Right [a]
    (<*>) = ap

instance Monad m => Alternative (NonDetT e m) where
    empty = NonDetT . return $ Left []
    NonDetT ma <|> NonDetT mb = NonDetT $ liftM2 plus ma mb
      where
        Left  xs `plus` Left  ys = Left $ xs <> ys
        Left  _  `plus` Right ys = Right ys
        Right xs `plus` Left  _  = Right xs
        Right xs `plus` Right ys = Right $ xs <> ys

instance Monad m => Monad (NonDetT e m) where
    NonDetT m >>= f = NonDetT $ m >>= \case
        Left  es -> return $ Left es
        Right as -> runNonDetT . asum $ f <$> as

instance Monad m => MonadPlus (NonDetT e m) where

instance MonadTrans (NonDetT e) where
    lift m = NonDetT $ Right . (:[]) <$> m

instance Monad m => MonadError e (NonDetT e m) where
    throwError = NonDetT . return . Left . (:[])
    catchError m h = NonDetT $ runNonDetT m >>= \case
        Left  es -> runNonDetT . asum $ map h es
        Right as -> return $ Right as

instance MonadReader r m => MonadReader r (NonDetT e m) where
    ask    = lift ask
    local  = mapNonDetT . local
    reader = lift . reader
