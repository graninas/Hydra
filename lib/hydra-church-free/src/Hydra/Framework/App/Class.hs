{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Hydra.Framework.App.Class where


-- TODO: this is awful
-- class (C.Logger l, C.Random r, C.ControlFlow cf, C.State' s,
--   C.Lang l r cf s lang, C.Process lang proc, Monad m)
--   => App l r cf s lang proc m
--   | m -> l, m -> r, m -> cf, m -> s, m -> lang, m -> proc  where
--   evalLang    :: lang a -> m a
--   evalProcess :: proc a -> m a
