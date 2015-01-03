{-# LINE 1 "./Test/Framework/Runners/Console/Table.hs" #-}
{-# LINE 1 "dist/dist-sandbox-235ea54e/build/autogen/cabal_macros.h" #-}
                                                                

                                   






                                    






                          






                                






                          






                                






                        






                                






                      






                        






                     






                       






                  






                    






                        






                         






                       






                   






                      






                          







{-# LINE 2 "./Test/Framework/Runners/Console/Table.hs" #-}
{-# LINE 1 "./Test/Framework/Runners/Console/Table.hs" #-}
module Test.Framework.Runners.Console.Table (
        Cell(..), Column(..), renderTable
    ) where

import Test.Framework.Utilities

import Text.PrettyPrint.ANSI.Leijen hiding (column)


data Cell = TextCell Doc
          | SeperatorCell

data Column = Column [Cell]
            | SeperatorColumn

type ColumnWidth = Int

renderTable :: [Column] -> Doc
renderTable = renderColumnsWithWidth . map (\column -> (findColumnWidth column, column))


findColumnWidth :: Column -> Int
findColumnWidth SeperatorColumn = 0
findColumnWidth (Column cells)  = maximum (map findCellWidth cells)

findCellWidth :: Cell -> Int
findCellWidth (TextCell doc) = maximum (0 : map length (lines (shows doc "")))
findCellWidth SeperatorCell  = 0


renderColumnsWithWidth :: [(ColumnWidth, Column)] -> Doc
renderColumnsWithWidth columns
  | all (columnFinished . snd) columns
  = empty
  | otherwise
  = first_cells_str <> line <>
    renderColumnsWithWidth (map (onRight columnDropHead) columns)
  where
    first_cells_str = hcat $ zipWith (uncurry renderFirstColumnCell) columns (eitherSideSeperator (map snd columns))


eitherSideSeperator :: [Column] -> [Bool]
eitherSideSeperator columns = zipWith (||) (False:column_is_seperator) (tail column_is_seperator ++ [False])
  where
    column_is_seperator = map isSeperatorColumn columns

isSeperatorColumn :: Column -> Bool
isSeperatorColumn SeperatorColumn = False
isSeperatorColumn (Column cells)  = case cells of
    []       -> False
    (cell:_) -> isSeperatorCell cell

isSeperatorCell :: Cell -> Bool
isSeperatorCell SeperatorCell = True
isSeperatorCell _             = False


renderFirstColumnCell :: ColumnWidth -> Column -> Bool -> Doc
renderFirstColumnCell column_width (Column cells) _ = case cells of
    []                    -> text $ replicate (column_width + 2) ' '
    (SeperatorCell:_)     -> text $ replicate (column_width + 2) '-'
    (TextCell contents:_) -> char ' ' <> fill column_width contents <> char ' '
renderFirstColumnCell _ SeperatorColumn either_side_seperator 
  = if either_side_seperator then char '+' else char '|'

columnFinished :: Column -> Bool
columnFinished (Column cells)  = null cells
columnFinished SeperatorColumn = True

columnDropHead :: Column -> Column
columnDropHead (Column cells)  = Column (drop 1 cells)
columnDropHead SeperatorColumn = SeperatorColumn
