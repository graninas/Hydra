-- "hedis" library has significant design flaws which prevent it
-- from being abstracted properly.

-- 1. `Queued` & `RedisTx` is a misconception. Cannot be abstracted.
-- 2. Context monads offer a particular design (mtl) and do not allow
--    or make it very hard to use another design.
-- 3. The design of transactions forces you to know that `Queued` is `Applicative`, `Monad`, etc.
--    which is an unnecessary accidental complexity from the library.
--    It may look nice and seem finely fitable to the hedis architecture
--    but it's neither testable nor abstractable.

-- This module does not abstract hedis / Redis but provides a raw access to the implementation.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Hydra.Core.KVDB.Redis.Language where



