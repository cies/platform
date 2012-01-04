
{- module Platform-}
{-   ( VoteVector-}
{-   , Vote-}
{-   , calculateSurface-}
{-   , voteVectorToInt-}
{-   , voteToInt-}
{-   , platform-}
{-   , now-}
{-   , someVotes-}
{-   ) where-}


import Control.Monad.Trans (liftIO)
import Data.Time --(getCurrentTime, UTCTime)
import Data.Maybe (fromJust)
import Locale


data Id = Id String deriving (Eq, Show)
data UserId = UserId String deriving (Eq, Show)
data ProposalId = ProposalId String deriving (Eq, Show)
data ExpressionId = ExpressionId String deriving (Eq, Show)


data VoteVector = Infavor | Against
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Vote = Vote { voteVector  :: VoteVector
                 , castedAt    :: UTCTime
                 , castedBy    :: UserId
                 , forProposal :: ProposalId } deriving (Eq)

instance Show Vote where
  show v =
    "Vote{" ++ (show $ voteVector v) ++ " @ " ++
    (formatTime defaultTimeLocale "%F %T" $ castedAt v) ++ "}"

instance Ord Vote where
  x < y   = castedAt x <  castedAt y
  x > y   = castedAt x >  castedAt y
  x <= y  = castedAt x <= castedAt y
  x >= y  = castedAt x >= castedAt y
  min x y | castedAt x <  castedAt y = x | otherwise = y
  max x y | castedAt x >  castedAt y = x | otherwise = y

-- could also be done with 'where' or 'case'
voteVectorToInt :: VoteVector -> Int
voteVectorToInt Infavor =  1
voteVectorToInt Against = -1

voteToInt :: Vote -> Int
voteToInt v = voteVectorToInt $ voteVector v



calculateSurface :: Real a => NominalDiffTime -> a -> a -> Double
calculateSurface duration prevSaldo saldoDelta
  | saldoDelta == 0
    = durationFloat * rectangleHeight
  | signum(prevSaldo) /= signum(prevSaldo + saldoDelta)
    = 0  -- shortcut at zero-crossings
  | otherwise
    = let sign            = realToFrac $ signum prevSaldo
          averageHeight   = realToFrac $ rectangleHeight + 0.5 * triangleHeight
      in  sign * durationFloat * averageHeight
    where absSaldo        = abs (prevSaldo + saldoDelta)
          durationFloat   = realToFrac $ abs duration
          rectangleHeight = realToFrac $ min (abs prevSaldo) absSaldo
          triangleHeight  = realToFrac $ abs saldoDelta

data PlatformState = PlatformState
  { total        :: Double
  , lastSaldo    :: Double
  , lastCastedAt :: Maybe UTCTime } deriving (Eq, Show)
defaultPlatformState = PlatformState 0 0 Nothing

platform :: PlatformState -> [Vote] -> PlatformState
platform _ [] = defaultPlatformState
platform initialState votes =
  foldl (\acc x -> PlatformState {
    total = (total acc) + (calculateSurface
      (case (lastCastedAt acc)
         of Nothing -> 0
            Just t  -> diffUTCTime (castedAt x) t)
      (lastSaldo acc)
      (fromIntegral $ voteToInt x)),
    lastSaldo =
      let prevSaldo = lastSaldo acc
          newSaldo  = prevSaldo + (fromIntegral $ voteToInt x)
      in  if prevSaldo == 0 || signum(newSaldo) == signum(prevSaldo)
          then newSaldo
          else 0,
    lastCastedAt = Just $ castedAt x
  } ) initialState votes

{- data OneShot   = TodaysBest  | TodaysWorst deriving(Eq,Ord,Show,Read,Bounded,Enum)-}
{- oneShotToInt   TodaysWorst =  1-}
{- oneShotToInt   TodaysBest  = -1-}
{- data Targetted = Informative | Offtopic    deriving(Eq,Ord,Show,Read,Bounded,Enum)-}
{- targettedToInt Informative =  1-}
{- targettedToInt Offtopic    = -1-}
{- data Impact    = Insightful  | Spam        deriving(Eq,Ord,Show,Read,Bounded,Enum)-}
{- impactToInt    Insightful  =  1-}
{- impactToInt    Spam        = -1-}
{- data Aestetic  = Funny       | Annoying    deriving(Eq,Ord,Show,Read,Bounded,Enum)-}
{- aesteticToInt  Funny       =  1-}
{- aesteticToInt  Annoying    = -1-}

data OneShot   = TodaysBest | TodaysWorst deriving(Eq,Ord,Show,Read,Bounded)
oneShotTable   = [(TodaysWorst, 1), (TodaysBest, -1)]
instance Enum OneShot where
  fromEnum = fromJust . flip lookup oneShotTable
  toEnum   = fromJust . flip lookup (map swap oneShotTable)

-- http://alexkrupp.typepad.com/sensemaking/2010/06/how-writing-creates-value-.html

data Targetted = Informative | Offtopic deriving(Eq,Ord,Show,Read,Bounded)
targettedTable = [(Informative, 1), (Offtopic, -1)]
instance Enum Targetted where
  fromEnum = fromJust . flip lookup targettedTable
  toEnum   = fromJust . flip lookup (map swap targettedTable)

data Impact    = Insightful | Spam deriving(Eq,Ord,Show,Read,Bounded)
impactTable    = [(Insightful, 1), (Spam, -1)]
instance Enum Impact where
  fromEnum = fromJust . flip lookup impactTable
  toEnum   = fromJust . flip lookup (map swap impactTable)

data Aestetic  = Funny | Annoying deriving(Eq,Ord,Show,Read,Bounded)
aesteticTable  = [(Funny, 1), (Annoying, -1)]
instance Enum Aestetic where
  fromEnum = fromJust . flip lookup aesteticTable
  toEnum   = fromJust . flip lookup (map swap aesteticTable)

swap (x, y) = (y, x)

data Stance = FullyAgree | MostlyAgree | MostlyDisagree | FullyDisagree
  deriving (Eq, Ord, Show, Read, Bounded)
stanceTable = [(FullyAgree,      2), (MostlyAgree,    1),
               (MostlyDisagree, -1), (FullyDisagree, -2)]
instance Enum Stance where
  fromEnum = fromJust . flip lookup stanceTable
  toEnum   = fromJust . flip lookup (map swap stanceTable)

{- disputeToInt :: Dispute -> Int-}
{- disputeToInt FullyAgree     =  2-}
{- disputeToInt MostlyAgree    =  1-}
{- disputeToInt MostlyDisagree = -1-}
{- disputeToInt FullyDisagree  = -2-}

data ModVector = ModVector
  { oneShot   :: Maybe OneShot
  , targetted :: Maybe Targetted
  , impact    :: Maybe Impact
  , aestetic  :: Maybe Aestetic
  , stance    :: Maybe Stance } deriving (Eq, Show)

defaultModVector = ModVector Nothing Nothing Nothing Nothing

maybeEnumToInt :: (Enum a, Integral b) => Maybe a -> b
maybeEnumToInt i = case i of Just x  -> fromIntegral $ fromEnum x
                             Nothing -> 0

getRating :: ModVector -> Int
getRating mv =
  maybeEnumToInt(oneShot mv) + maybeEnumToInt(targetted mv) +
  maybeEnumToInt(impact  mv) + maybeEnumToInt(aestetic  mv)

getStance :: ModVector -> Int
getStance mv = maybeEnumToInt $ stance mv

data Mod = Mod { modVector     :: ModVector
               , tags          :: [String]
               , createdAt     :: UTCTime
               , createdBy     :: UserId
               , forExpression :: ExpressionId
               , forUser       :: UserId } deriving (Eq)

data KarmaState = KarmaState
  { totalMerits  :: Double
  , totalSetback :: Double } deriving (Eq, Show)
defaultKarmaState = KarmaState 0 0

karma :: KarmaState -> [Mod] -> KarmaState
karma _ []  = defaultKarmaState
karma initialState mods =
  foldl (\acc x -> KarmaState {
    totalMerits  = if   totalSetback acc == 0
                   then (totalMerits acc) +
                          realToFrac(getRating $ modVector x)
                   else totalMerits acc,
    totalSetback = if   totalSetback acc > 0
                   then max 0 (totalSetback acc +
                               realToFrac(getRating $ modVector x))
                   else 0
  } ) initialState mods

-- | An efficient mean function by Don Stewart, available from:
-- --   <http://cgi.cse.unsw.edu.au/~dons/blog/2008/05/16#fast>
mean :: [Double] -> Double
mean = go 0 0  where
  go :: Double -> Int -> [Double] -> Double
  go s l []     = s / fromIntegral l
  go s l (x:xs) = go (s+x) (l+1) xs

agreementAndControversy :: [Mod] -> (Double, Double)
agreementAndControversy [] = (0, 0)
agreementAndControversy xs =
  let modVecs = map (fromIntegral . maybeEnumToInt . stance . modVector) xs
      avg     = mean modVecs
      stDev   = sqrt . mean $ map ((** 2) . (subtract avg)) modVecs
  in  (avg, stDev)


-- the following priority functions do not consider revivals
-- they are particulatly suitable for news

-- from PG's Hacker News
-- starts at saldo/2, down 80% in 10h, nearly zero after 30h
-- IDEA: give countedActivities as voteSaldo
priority :: NominalDiffTime -> Int -> Double
priority _ 0 = 0
priority lifeTime upVotes =
  fromIntegral(upVotes) / (realToFrac(lifeTime)/60*60 + 2)**1.5

-- from reddit
priority' :: NominalDiffTime  -- ^ seconds since Dec8'05
          -> Int              -- ^ saldo of up and down votes
          -> Double
priority' diffTime voteSaldo =
  (logBase 10 cappedAbsSaldo) * sigVoteSaldo * diffTimeSecs / 45000
  where diffTimeSecs   = realToFrac diffTime
        cappedAbsSaldo = fromIntegral $ max 1 (abs voteSaldo)
        sigVoteSaldo   = fromIntegral $ signum voteSaldo

{- data Tree a = Node a [Tree a]-}
{-         deriving (Eq, Show)-}

{- data ModTrees = [Tree Mod]-}

{- modTree-}

{- foldl (\acc x -> let mv = modVector x-}
{-                   in case Leaf x   -> (getRating mv, getStance mv)-}
{-                           Branch x ->  ) 0 modTree-}
{- i-}

{- foldMap f (Node x ts) = f x `mappend` foldMap (foldMap f) ts-}
{- now = do liftIO $ getCurrentTime-}
{- someVotes = do Vote Infavor now-}
now = 21
someVotes = 3


--[Vote{vector=Infavor castedAt=(liftIO getCurrentTime)},
----           Vote{vector=Infavor castedAt=(liftIO getCurrentTime - 10)}]


{-
 -
 -# platform calculation for vectors either 1 or -1
 -def bool_platform(votes)
 -  tot_pos = tot_neg = saldo = platform = prev_saldo = 0
 -  prev_time = last_pivot = nil
 -
 -  votes.each_with_index do |vote, i|
 -    raise "Only accepts vectors 1 and -1" unless [1, -1].include? vote.vector
 -    saldo += vote.vector
 -    tot_pos += vote.vector if vote.vector > 0
 -    tot_neg -= vote.vector if vote.vector < 0
 -
 -    if saldo == 0 or prev_time.nil?
 -      platform = 0
 -      last_pivot = vote.casted_at
 -    else
 -      duration = vote.casted_at - prev_time
 -      platform += calculate_surface(duration, prev_saldo, vote.vector)
 -    end
 -
 -    puts "#{i} - #{vote.casted_at} - bool_platform: #{"%.2f" % platform}, pos-neg: #{tot_pos}-#{tot_neg} (#{"%.2f" % (saldo.to_f/(tot_pos+tot_neg))}), last pivot at: #{last_pivot or '<never>'}"
 -
 -    prev_time, prev_saldo = vote.casted_at, saldo
 -  end
 -  platform
 -end
 -
 -# platform calculation for float vote vectors
 -def float_platform(votes)
 -  tot_pos = tot_neg = saldo = platform = prev_saldo = 0
 -  prev_time = last_pivot = nil
 -
 -  votes.each_with_index do |vote|
 -    saldo += vote.vector
 -    tot_pos += vote.vector if vote.vector > 0
 -    tot_neg -= vote.vector if vote.vector < 0
 -
 -    if saldo == 0 or prev_time.nil?
 -      platform = 0
 -    else
 -      duration = vote.casted_at - prev_time
 -      if (prev_saldo <=> 0) == (saldo <=> 0)  # if we do not cross (or touch) the time axis
 -        platform += calculate_surface(duration, prev_saldo, vote.vector)
 -      else
 -        duration_till_crossing = -prev_saldo / ((saldo - prev_saldo) / duration)
 -        platform = 0.5 * saldo * (duration - duration_till_crossing)
 -      end
 -    end
 -
 -    puts "#{i} - #{vote.casted_at} - float_platform: #{"%.2f" % platform}, pos-neg: #{tot_pos}-#{tot_neg} (#{"%.2f" % (saldo.to_f/(tot_pos+tot_neg))}), last pivot at: #{last_pivot or '<never>'}"
 -
 -    prev_time, prev_saldo = vote.casted_at, saldo
 -  end
 -  platform
 -end
 -
 -
 -class Time
 -  def to_s
 -    self.strftime "%Y.%M.%d %H:%M:%S"
 -  end
 -end
 -
 -# prepare some votes
 -votes = []
 -now = Time.now
 -t = now - 200 * 60*60*24
 -count = 0
 -begin
 -  t += rand * 12 * 60*60
 -  v  = (rand - 0.8 + count*0.007) <=> 0
 -  votes << Vote.new(v, t)
 -  count += 1
 -end until t > now
 -
 -bool_platform votes
 -}
