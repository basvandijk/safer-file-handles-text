{-# LANGUAGE UnicodeSyntax, NoImplicitPrelude, CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Lazy.IO.SaferFileHandles
-- Copyright   :  (c) 2010-2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This module lifts the lazy 'Text' IO operations into the region monad.
--
-------------------------------------------------------------------------------

module Data.Text.Lazy.IO.SaferFileHandles
    ( hGetLine
    , hGetContents

    , hPutStr
    , hPutStrLn
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from transformers:
import Control.Monad.IO.Class ( MonadIO )

-- from text:
import Data.Text.Lazy ( Text )

#ifdef __HADDOCK__
import qualified Data.Text.Lazy.IO as T ( hGetLine
                                        , hGetContents

                                        , hPutStr
                                        , hPutStrLn
                                        )
#endif

-- from explicit-iomodes-text:
import qualified Data.Text.Lazy.IO.ExplicitIOModes as E ( hGetLine
                                                        , hGetContents

                                                        , hPutStr
                                                        , hPutStrLn
                                                        )
-- from regions:
import Control.Monad.Trans.Region ( AncestorRegion )

-- from safer-file-handles:
import System.IO.SaferFileHandles        ( FileHandle, ReadModes, WriteModes )
import System.IO.SaferFileHandles.Unsafe ( wrap, wrap2 )


-------------------------------------------------------------------------------
-- Lazy Text I/O with regional file handles
-------------------------------------------------------------------------------

-- | Wraps: @Data.Text.Lazy.IO.'T.hGetLine'@.
hGetLine ∷ ( FileHandle handle, ReadModes ioMode
           , pr `AncestorRegion` cr, MonadIO cr
           )
         ⇒ handle ioMode pr → cr Text
hGetLine = wrap E.hGetLine

-- | Wraps: @Data.Text.Lazy.IO.'T.hGetContents'@.
hGetContents ∷ ( FileHandle handle, ReadModes ioMode
               , pr `AncestorRegion` cr, MonadIO cr
               )
             ⇒ handle ioMode pr → cr Text
hGetContents = wrap E.hGetContents


-- | Wraps: @Data.Text.Lazy.IO.'T.hPutStr'@.
hPutStr ∷ ( FileHandle handle, WriteModes ioMode
          , pr `AncestorRegion` cr, MonadIO cr
          )
        ⇒ handle ioMode pr → Text → cr ()
hPutStr = wrap2 E.hPutStr

-- | Wraps: @Data.Text.Lazy.IO.'T.hPutStrLn'@.
hPutStrLn ∷ ( FileHandle handle, WriteModes ioMode
            , pr `AncestorRegion` cr, MonadIO cr
            )
          ⇒ handle ioMode pr → Text → cr ()
hPutStrLn = wrap2 E.hPutStrLn


-- The End ---------------------------------------------------------------------
