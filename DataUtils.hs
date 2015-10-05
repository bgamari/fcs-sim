module DataUtils (save, load) where

import Data.Csv
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import Data.Foldable as Foldable

save :: (Foldable f, ToRecord a) => FilePath -> f a -> IO ()
save fname xs = do
    BS.writeFile fname $ encode $ Foldable.toList xs

load = undefined
