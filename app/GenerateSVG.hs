{-# LANGUAGE OverloadedStrings #-}

module GenerateSVG where

import qualified Data.ByteString.Char8 as C
import qualified Data.Map as Map
import qualified Text.XML as XML

import XMLTemplate


writeSVGFile :: [(C.ByteString, Double)] -> FilePath -> IO ()
writeSVGFile districtValList filePath =
    XML.writeFile XML.def filePath $ XML.Document (XML.Prologue [] Nothing []) root []
    where
        root = XML.Element "svg" (Map.fromList svgAttributes) $ generateXMLCode districtValList colorGradient
        svgAttributes = [
            ("version","1.1")
            ,("xmlns", "http://www.w3.org/2000/svg")
            ,("x", "0px")
            ,("y", "0px")
            ,("width", "506.3px")
            ,("height", "600px")
            ,("viewBox", "0 0 732.4 1023")]
            