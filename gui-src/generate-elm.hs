{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
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

import Elm ( Spec (Spec)
           , specsToDir
           , toElmDecoderSource
           , toElmTypeSource
           )

import Servant.Elm  ( ElmType
                    , Proxy (Proxy)
                    , defElmImports
                    , generateElmForAPI
                    )

instance ElmType Slice

instance ElmType Language

instance ElmType Fragment

instance ElmType Use

instance ElmType Instance

instance ElmType InstancePart

instance ElmType Reference

instance ElmType UsedName

instance ElmType Name

-- instance ElmType InstanceID

-- instance ElmType SliceID

-- instance ElmType TypeName

-- instance ElmType SourceCode

-- instance ElmType Qualification

-- instance ElmType OriginalModule

-- instance ElmType GHCExtension

spec :: Spec
spec = Spec ["Generated", "Api"]
            ( [defElmImports]
            ++ defineElm (Proxy :: Proxy Slice)
            ++ defineElm (Proxy :: Proxy Language)
            ++ defineElm (Proxy :: Proxy Fragment)
            ++ defineElm (Proxy :: Proxy Use)
            ++ defineElm (Proxy :: Proxy Instance)
            ++ defineElm (Proxy :: Proxy InstancePart)
            ++ defineElm (Proxy :: Proxy Reference)
            ++ defineElm (Proxy :: Proxy UsedName)
            ++ defineElm (Proxy :: Proxy Name)
            {-++ defineElm (Proxy :: Proxy InstanceID)
            ++ defineElm (Proxy :: Proxy SliceID)
            ++ defineElm (Proxy :: Proxy TypeName)
            ++ defineElm (Proxy :: Proxy SourceCode)
            ++ defineElm (Proxy :: Proxy Qualification)
            ++ defineElm (Proxy :: Proxy OriginalModule)
            ++ defineElm (Proxy :: Proxy GHCExtension) -}
            ++ generateElmForAPI (Proxy :: Proxy API)
            )

defineElm p = [ toElmTypeSource p
              , toElmDecoderSource p
              ]

main :: IO ()
main = specsToDir [spec] "elm"
