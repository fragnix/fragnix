{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Fragnix.Slice ( Slice
                     , Language
                     , Fragment
                     , Use
                     , Instance
                     , InstancePart
                     , Reference
                     , UsedName
                     , Name
                     , SliceID
                     , InstanceID
                     , TypeName
                     , SourceCode
                     , Qualification
                     , OriginalModule
                     , GHCExtension
                     )
import Api

import Elm.Derive   ( defaultOptions
                    , deriveElmDef
                    )

import Servant.Elm  ( DefineElm (DefineElm)
                    , Proxy (Proxy)
                    , defElmImports
                    , defElmOptions
                    , generateElmModuleWith
                    )

deriveElmDef defaultOptions ''Slice

deriveElmDef defaultOptions ''Language

deriveElmDef defaultOptions ''Fragment

deriveElmDef defaultOptions ''Use

deriveElmDef defaultOptions ''Instance

deriveElmDef defaultOptions ''InstancePart

deriveElmDef defaultOptions ''Reference

deriveElmDef defaultOptions ''UsedName

deriveElmDef defaultOptions ''Name

-- deriveElmDef defaultOptions ''InstanceID 

deriveElmDef defaultOptions ''SliceID

-- deriveElmDef defaultOptions ''TypeName

deriveElmDef defaultOptions ''SourceCode

deriveElmDef defaultOptions ''Qualification

deriveElmDef defaultOptions ''OriginalModule

deriveElmDef defaultOptions ''GHCExtension

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated"
    , "Api"
    ]
    defElmImports
    "elm"
    [ DefineElm (Proxy :: Proxy Slice)
    , DefineElm (Proxy :: Proxy Language)
    , DefineElm (Proxy :: Proxy Fragment)
    , DefineElm (Proxy :: Proxy Use)
    , DefineElm (Proxy :: Proxy Instance)
    , DefineElm (Proxy :: Proxy InstancePart)
    , DefineElm (Proxy :: Proxy Reference)
    , DefineElm (Proxy :: Proxy UsedName)
    , DefineElm (Proxy :: Proxy Name)
    --, DefineElm (Proxy :: Proxy InstanceID)
    , DefineElm (Proxy :: Proxy SliceID)
    --, DefineElm (Proxy :: Proxy TypeName)
    , DefineElm (Proxy :: Proxy SourceCode)
    , DefineElm (Proxy :: Proxy Qualification)
    , DefineElm (Proxy :: Proxy OriginalModule)
    , DefineElm (Proxy :: Proxy GHCExtension)
    ]
    (Proxy :: Proxy API)
