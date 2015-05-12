{-# LANGUAGE NoImplicitPrelude, DeriveDataTypeable, BangPatterns,
  MultiParamTypeClasses, TypeFamilies, FlexibleContexts,
  StandaloneDeriving, NondecreasingIndentation, ExplicitForAll,
  PatternGuards #-}
module MVectorFamily where

data family MVector s a