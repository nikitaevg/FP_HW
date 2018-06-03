{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module TH where

import           Language.Haskell.TH
import qualified Data.Text as T

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices n indices = do
    let names = map (\x -> mkName ("x" ++ show x)) [0..n - 1]
    let vars = map varP names
    lamE [tupP vars] (tupE (map (\x -> varE $ names !! x) indices))

-- Кажется, идея заключается в том, что не надо паттерн-матчиться,
-- а надо принять список как переменную. Также надо бы написать функцию getIth,
-- которая брала бы из тупла итый элемент. А потом написать на темплейтах
-- map (getIth tuple) indices. Не успел.

class (Show a) => TextShow a where
    textShow :: a -> T.Text

genTextShow :: Name -> Q [Dec]
genTextShow name = [d|instance TextShow $(conT name) where
                        textShow = T.pack . show |]
