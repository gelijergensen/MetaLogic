{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module RApplicative where

import GHC.Exts (Constraint)
import Prelude hiding (fmap, liftA2, (<$>))

class RApplicative f where
  type RApplicativeCtxt f a :: Constraint
  type RApplicativeCtxt f a = ()
  pure :: RApplicativeCtxt f a => a -> f a
  fmap :: (RApplicativeCtxt f a, RApplicativeCtxt f b) => (a -> b) -> f a -> f b
  (<$>) ::
    (RApplicativeCtxt f a, RApplicativeCtxt f b) => (a -> b) -> f a -> f b
  (<$>) = fmap
  liftA2 ::
    ( RApplicativeCtxt f a,
      RApplicativeCtxt f b,
      RApplicativeCtxt f c
    ) =>
    (a -> b -> c) ->
    f a ->
    f b ->
    f c
  {-# MINIMAL pure, fmap, liftA2 #-}
