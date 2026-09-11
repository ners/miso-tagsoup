-----------------------------------------------------------------------------
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-orphans       #-}
-----------------------------------------------------------------------------
module Miso.Html.Parse (rawSVG, rawHTML) where
-----------------------------------------------------------------------------
import           Miso (View(..), Namespace(..), MisoString, textProp, ms)
#ifndef VANILLA
import qualified Miso.String as JS
import           Text.StringLike (StringLike(..))
#endif
import           Text.HTML.TagSoup (Tag(..))
import           Text.HTML.TagSoup.Tree (parseTree, TagTree(..))
-----------------------------------------------------------------------------
-- | Used to support RawText, inlining of HTML.
-- Filters tree to only branches and leaves w/ Text tags.
-- converts to `View m a`. Note: if HTML is malformed,
-- (e.g. closing tags and opening tags are present) they will
-- be removed.
parseView :: Namespace -> MisoString -> [View context props model action]
parseView ns html = reverse (go (parseTree html) [])
  where
    go [] xs = xs
    go (TagLeaf (TagText s) : next) views =
      go next (VText Nothing s : views)
    go (TagLeaf (TagOpen name attrs) : next) views =
      go (TagBranch name attrs [] : next) views
    go (TagBranch name_ attrs kids : next) views =
      let
        attrs' = [ textProp (ms k) (ms v)
                 | (k, v) <- attrs
                 ]
        newNode =
          VNode ns (ms name_) attrs' (reverse (go kids [])) mempty
      in
        go next (newNode:views)
    go (TagLeaf _ : next) views =
      go next views
-----------------------------------------------------------------------------
rawSVG :: MisoString -> [View context props model action]
rawSVG = parseView SVG
-----------------------------------------------------------------------------
rawHTML :: MisoString -> [View context props model action]
rawHTML = parseView HTML
-----------------------------------------------------------------------------
#ifndef VANILLA
instance StringLike MisoString where
  uncons = JS.uncons
  toString = JS.unpack
  fromChar = JS.singleton
  strConcat = JS.concat
  empty = JS.empty
  strNull = JS.null
  cons = JS.cons
  append = JS.append
  strMap = JS.map
#endif
-----------------------------------------------------------------------------
