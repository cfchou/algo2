--
data HeapT prio val
    = Empty  -- ^ An empty 'HeapT'.
    | Tree { _rank     :: {-# UNPACK #-} !Int -- ^ Rank of the leftist heap.
           , _size     :: {-# UNPACK #-} !Int -- ^ Number of elements in the heap.  
           , _priority :: !prio               -- ^ Priority of the entry.
           , _value    :: val                 -- ^ Value of the entry.
           , _left     :: !(HeapT prio val)   -- ^ Left subtree.
           , _right    :: !(HeapT prio val)   -- ^ Right subtree.
    } -- ^ A tree node of a non-empty 'HeapT'.
    deriving (Typeable)

type Heap pol item = HeapT (Prio pol item) (Val pol item)
type MaxPrioHeap prio val 
    = Heap FstMaxPolicy (prio, val)
    -- HeapT prio val
    = HeapT (Prio FstMaxPolicy (prio, val)) (Val FstMaxPolicy (prio, val))

-- associated type "Prio pol item" fits HeaT's 'prio' type parameter
-- associated type "Val pol item" fits HeaT's 'val' type parameter
<????
Val seems trivial?
although it's given pol(policy) and item(prio, val)
????>


class Ord (Prio pol item) => HeapItem pol item where
-- Prio's type parameters 'pol item' is HeapItem instance
'pol item' ==> 'FstMaxPolicy (prio, val)' ==> HeapItem instance

--
data FstMaxPolicy -- instance of HeapItem

instance (Ord prio) => HeapItem FstMaxPolicy (prio, val) where
    newtype Prio FstMaxPolicy (prio, val) = FMaxP { unFMaxP :: prio } deriving (Eq)
    type    Val  FstMaxPolicy (prio, val) = val
 
    -- Translate an item into a priority-value pair.
    split :: item -> (Prio pol item, Val pol item)
    split (p,       v) = (FMaxP p, v)

    -- Restore the item from a priority-value pair.
    merge :: (Prio pol item, Val pol item) -> item
    merge (FMaxP p, v) = (p,       v)

-- "Prio FstMaxPolicy (prio, val)" is an instance of Ord, Read, Show

-- class Eq a => Ord a where
instance (Ord prio) => Ord (Prio FstMaxPolicy (prio, val)) where
    compare (FMaxP x) (FMaxP y) = compare y x
-- class Read a where
instance (Read prio) => Read (Prio FstMaxPolicy (prio, val)) where
    readPrec     = fmap FMaxP readPrec
    readListPrec = fmap (fmap FMaxP) readListPrec
-- class Show a where
instance (Show prio) => Show (Prio FstMaxPolicy (prio, val)) where
    show        = show . unFMaxP
    showsPrec d = showsPrec d . unFMaxP
    showList    = showList . (fmap unFMaxP)





