{-# LANGUAGE FlexibleInstances #-}

module Path.Extra
  ( isRoot,
    dirComponents,
    fileComponents,

    -- * Re-exports
    module Path,
  )
where

import Path


-- |
-- Determines if the directory is the root
isRoot :: Path b Dir -> Bool
isRoot p = parent p == p

parents ::
  Path b t ->
  (Path b Dir, [Path Rel Dir])
parents path = go [] path
  where
    go ::
      [Path Rel Dir] ->
      Path b t ->
      (Path b Dir, [Path Rel Dir])
    go acc p =
      let p' = parent p
       in if isRoot p'
            then (p', acc)
            else go (dirname p' : acc) p'

-- |
-- Returns the components of a directory path.
--
-- @
-- dirComponents $(mkAbsDir "/a/b/")
--   = ($(mkAbsDir "/"), [$(mkRelDir "a/"), $(mkRelDir "b/")])
-- dirComponents $(mkRelDir  "a/b/")
--   = ($(mkRelDir "./"), [$(mkRelDir "a/"), $(mkRelDir "b/")])
-- @
dirComponents ::
  Path b Dir ->
  (Path b Dir, [Path Rel Dir])
dirComponents p | isRoot p = (p, [])
dirComponents p =
  case parents p of
    (r, ps) ->
      (r, ps ++ [dirname p])

-- |
-- Returns the components of a file path.
--
-- @
-- fileComponents $(mkAbsFile "/a/b")
--   = ($(mkAbsDir "/"), [$(mkRelDir "a/")], $(mkRelFile "b"))
-- fileComponents $(mkRelFile "a/b")
--   = ($(mkRelDir "./"), [$(mkRelDir "a/")], $(mkRelFile "b"))
-- @
fileComponents ::
  Path b File ->
  (Path b Dir, [Path Rel Dir], Path Rel File)
fileComponents p =
  case parents p of
    (r, ps) ->
      (r, ps, filename p)
