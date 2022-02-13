import Data.Maybe (isJust)

-- Yet Another RPG

data Item = Sword | Bow | MagicWand
    deriving (Eq)

instance Show Item where
    show Sword = "sword"
    show Bow = "bow"
    show MagicWand = "magic wand"

-- The Mobs

data Mob = Mummy | Skeleton Item | Witch (Maybe Item)
    deriving (Eq)

createMummy :: Mob
createMummy = Mummy

createArcher :: Mob
createArcher = Skeleton Bow

createKnight :: Mob
createKnight = Skeleton Sword

createWitch :: Mob
createWitch = Witch Nothing

createSorceress :: Mob
createSorceress = Witch (Just MagicWand)

create :: String -> Maybe Mob
create str = case str of
    "mummy" -> Just createMummy
    "doomed archer" -> Just createArcher
    "dead knight" -> Just createKnight
    "witch" -> Just createWitch
    "sorceress" -> Just createSorceress
    _ -> Nothing

equip :: Item -> Mob -> Maybe Mob
equip _ Mummy = Nothing
equip item (Witch _) = Just (Witch (Just item))
equip item (Skeleton _) = Just (Skeleton item)

instance Show Mob where
    show Mummy = "mummy"
    show (Skeleton Bow) = "doomed archer"
    show (Skeleton Sword) = "dead knight"
    show (Skeleton x) = "skeleton holding a " ++ show x
    show (Witch Nothing) = "witch"
    show (Witch (Just MagicWand)) = "sorceress"
    show (Witch (Just x)) = "witch holding a " ++ show x

class HasItem a where
    getItem :: a -> Maybe Item
    hasItem :: a -> Bool
    hasItem obj = isJust (getItem obj)

instance HasItem Mob where
    hasItem Mummy = False 
    hasItem (Witch Nothing) = False
    hasItem _ = True

    getItem Mummy = Nothing 
    getItem (Skeleton item) = Just item
    getItem (Witch Nothing) = Nothing 
    getItem (Witch (Just item)) = Just item