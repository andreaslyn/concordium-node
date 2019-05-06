{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, RecordWildCards, ScopedTypeVariables, GeneralizedNewtypeDeriving, RankNTypes, OverloadedStrings, LambdaCase #-}
{- |Asynchronous Binary Byzantine Agreement algorithm -}
module Concordium.Afgjort.ABBA(
    Phase,
    ABBAMessage(..),
    ABBAInstance(ABBAInstance),
    ABBAState(..),
    initialABBAState,
    ABBAMonad(..),
    ABBAOutputEvent(..),
    ABBA,
    runABBA,
    beginABBA,
    justifyABBAChoice,
    receiveABBAMessage,
    Choice
) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (|>))
import Data.Maybe
import Data.Word
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Lens.Micro.Platform
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS

-- |A phase in the ABBA protocol
type Phase = Word32

-- |A message in the ABBA protocol
data ABBAMessage
    = Justified Phase Choice TicketProof            -- ^Party's input for a given phase
    | CSSSeen Phase Party Choice                    -- ^CSS seen message for a phase
    | CSSDoneReporting Phase (Map Party Choice)     -- ^CSS done reporting message for a phase
    | WeAreDone Choice                              -- ^Message that indicates consensus should be reached
    deriving (Eq, Ord, Show)

-- |An @ABBAInstance@ consists of:
--
-- * The instance identifier for this instantiation of the protocol
-- * The total weight of all parties
-- * The maximum weight of corrupt parties (must be less than @totalWeight/3@)
-- * The weight of each party
-- * The public key of each party
-- * My party
-- * My VRF key
data ABBAInstance = ABBAInstance {
    -- |The instance identifier for this instantiation of the protocol
    baid :: BS.ByteString,
    -- |The total weight of all parties
    totalWeight :: Int,
    -- |The maximum weight of corrupt parties (must be less than @totalWeight/3@).
    corruptWeight :: Int,
    -- |The weight of each party
    partyWeight :: Party -> Int,
    -- |The public key of each party
    pubKeys :: Party -> VRF.PublicKey,
    -- |My party
    me :: Party,
    -- |My VRF key
    privateKey :: VRF.KeyPair
}

-- |The state of a phase in the protocol.
--
-- This includes the lottery tickets submitted by parties, the CSS state for the current phase,
-- and the total weight and set of parties that have nominated each input.  Once the weight
-- exceeds the threshold, we record it as @Nothing@, forgetting the exact weigth and parties.
data PhaseState = PhaseState {
    _lotteryTickets :: Map (Double, Party) Ticket,
    _phaseCSSState :: Either (Maybe Choices, Seq (Party, CSSMessage)) CSSState,
    _topInputWeight :: Maybe (Int, Set Party),
    _botInputWeight :: Maybe (Int, Set Party)
} deriving (Show)
makeLenses ''PhaseState

-- |The total weight and set of parties nominating a particular choice.
inputWeight :: Choice -> Lens' PhaseState (Maybe (Int, Set Party))
inputWeight True = topInputWeight
inputWeight False = botInputWeight

-- |The initial state of a phase
initialPhaseState :: PhaseState
initialPhaseState = PhaseState {
    _lotteryTickets = Map.empty,
    _phaseCSSState = Left (Nothing, Seq.empty),
    _topInputWeight = Just (0, Set.empty),
    _botInputWeight = Just (0, Set.empty)
}

-- |The state of the ABBA protocol.
--
-- This includes the current phase, the state of all phases, the current grade,
-- and the set and weight of parties that have claimed we are done with each
-- possible output choice.
data ABBAState = ABBAState {
    _currentPhase :: Phase,
    _phaseStates :: Map Phase PhaseState,
    _currentGrade :: Word8,
    _topWeAreDone :: Set Party,
    _topWeAreDoneWeight :: Int,
    _botWeAreDone :: Set Party,
    _botWeAreDoneWeight :: Int,
    _completed :: Bool
} deriving (Show)
makeLenses ''ABBAState

-- |The state of a particular phase
phaseState :: Phase -> Lens' ABBAState PhaseState
phaseState p = lens (\s -> fromMaybe initialPhaseState (_phaseStates s ^. at p))
    (\s t -> s & phaseStates . at p ?~ t)

-- |The set of parties claiming we are done with a given choice
weAreDone :: Choice -> Lens' ABBAState (Set Party)
weAreDone True = topWeAreDone
weAreDone False = botWeAreDone

-- |The weight of parties claiming we are done with a given choice
weAreDoneWeight :: Choice -> Lens' ABBAState Int
weAreDoneWeight True = topWeAreDoneWeight
weAreDoneWeight False = botWeAreDoneWeight

-- |The initial state of the ABBA protocol.
initialABBAState :: ABBAState
initialABBAState = ABBAState {
    _currentPhase = 0,
    _phaseStates = Map.singleton 0 (initialPhaseState {_phaseCSSState = Right initialCSSState}),
    _currentGrade = 0,
    _topWeAreDone = Set.empty,
    _topWeAreDoneWeight = 0,
    _botWeAreDone = Set.empty,
    _botWeAreDoneWeight = 0,
    _completed = False
}

-- |The @ABBAMonad@ class defines the events associated with the ABBA protocol.
class (MonadState ABBAState m, MonadReader ABBAInstance m) => ABBAMonad m where
    -- |Sign and broadcast an ABBA message to all parties, __including__ our own 'ABBAInstance'.
    sendABBAMessage :: ABBAMessage -> m ()
    -- |Determine the result
    aBBAComplete :: Choice -> m ()

-- |Representation of (output) events associated with the ABBA protocol.
data ABBAOutputEvent
    = SendABBAMessage ABBAMessage   -- ^Sign and broadcast a message
    | ABBAComplete Choice                   -- ^Determine result

-- |A concrete implementation of the ABBA monad.
newtype ABBA a = ABBA {
    runABBA' :: RWS ABBAInstance (Endo [ABBAOutputEvent]) ABBAState a
} deriving (Functor, Applicative, Monad)

-- |Run part of the ABBA protocol, given an 'ABBAInstance' and 'ABBAState'.
-- The result includes the updated state and a list of 'ABBAOutputEvent's that occurred during the execution.
{-# INLINE runABBA #-}
runABBA :: ABBA a -> ABBAInstance -> ABBAState -> (a, ABBAState, [ABBAOutputEvent])
runABBA z i s = runRWS (runABBA' z) i s & _3 %~ (\(Endo f) -> f [])

instance MonadState ABBAState ABBA where
    get = ABBA get
    put = ABBA . put
    state = ABBA . state

instance MonadReader ABBAInstance ABBA where
    ask = ABBA ask
    reader = ABBA . reader
    local f = ABBA . local f . runABBA'

instance ABBAMonad ABBA where
    sendABBAMessage msg = ABBA $ tell $ Endo (SendABBAMessage msg :)
    aBBAComplete c = ABBA $ tell $ Endo (ABBAComplete c :)

liftCSSReceiveMessage :: (ABBAMonad m) => Phase -> Party -> CSSMessage -> m ()
liftCSSReceiveMessage phase src msg = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (justif, msgs |> (src, msg))
            Right cssstate -> do
                let (_, cssstate', evs) = runCSS (receiveCSSMessage src msg) (CSSInstance totalWeight corruptWeight partyWeight) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

liftCSSJustifyChoice :: (ABBAMonad m) => Phase -> Choice -> m ()
liftCSSJustifyChoice phase c = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Left (justif, msgs) -> phaseState phase . phaseCSSState .= Left (addChoice c justif, msgs)
            Right cssstate -> do
                let (_, cssstate', evs) = runCSS (justifyChoice c) (CSSInstance totalWeight corruptWeight partyWeight) cssstate
                phaseState phase . phaseCSSState .= Right cssstate'
                handleCSSEvents phase evs

handleCSSEvents :: (ABBAMonad m) => Phase -> [CSSOutputEvent] -> m ()
handleCSSEvents _ [] = return ()
handleCSSEvents phase (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleCSSEvents phase evs
    where
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen p c) = CSSSeen phase p c
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs
handleCSSEvents phase (SelectCoreSet cs : evs) = handleCoreSet phase cs >> handleCSSEvents phase evs


-- |Lift a CSS operation to the ABBA monad in a given phase.
-- {-# SPECIALIZE liftCSS :: (Ord party, Show party) => Phase -> CSS party a -> ABBA party a #-}
{-
liftCSS :: (ABBAMonad party m, Ord party, Show party) => Phase -> CSS party a -> m a
liftCSS phase a = do
        ABBAInstance{..} <- ask
        use (phaseState phase . phaseCSSState) >>= \case
            Right cssstate -> do
                let (r, cssstate', evs) = runCSS a (CSSInstance totalWeight corruptWeight partyWeight) cssstate
                phaseState phase . phaseCSSState .= cssstate'
                cs <- handleEvents evs
                forM_ cs $ \cs' -> do
                    cp <- use currentPhase
                    unless (phase == cp) $ do
                        st <- get
                        error $ "liftCSS on phase " ++ show phase ++ " but current phase is " ++ show cp ++ "\n" ++ show st
                    assert (phase == cp) $ handleCoreSet phase cs'
                return r
    where
        handleEvents [] = return Nothing
        handleEvents (SendCSSMessage m : evs) = sendABBAMessage (liftMsg m) >> handleEvents evs
        handleEvents (SelectCoreSet cs : evs) = handleEvents evs >> return (Just cs)
        liftMsg (Input _) = undefined -- Should not happen
        liftMsg (Seen p c) = CSSSeen phase p c
        liftMsg (DoneReporting cs) = CSSDoneReporting phase cs
-}

-- |Deal with a core set being generated by CSS.  The phase should always be the current phase.
{-# SPECIALIZE handleCoreSet :: Phase -> CoreSet -> ABBA () #-}
handleCoreSet :: (ABBAMonad m) => Phase -> CoreSet -> m ()
handleCoreSet phase cs = do
        ABBAInstance{..} <- ask
        cp <- use currentPhase
        if (phase /= cp) then do
            st <- get
            error $ "handleCoreSet on phase " ++ show phase ++ " but current phase is " ++ show cp ++ "\n" ++ show st
        else do
            let
                csTop = fromMaybe Set.empty (coreTop cs)
                csBot = fromMaybe Set.empty (coreBot cs)
                csRes p = if p `Set.member` csTop then Just True else
                            if p `Set.member` csBot then Just False else Nothing
                topWeight = sum $ partyWeight <$> Set.toList csTop
                botWeight = sum $ partyWeight <$> Set.toList csBot
            lid <- view $ lotteryId phase
            tkts <- filter (\((_,party),tkt) -> checkTicket lid (pubKeys party) tkt) . Map.toDescList <$> use (phaseState phase . lotteryTickets)
            let (nextBit, newGrade) =
                    if Set.null csBot then
                        (True, 2)
                    else if Set.null csTop then
                        (False, 2)
                    else if topWeight >= totalWeight - corruptWeight then
                        (True, 1)
                    else if botWeight >= totalWeight - corruptWeight then
                        (False, 1)
                    else if botWeight <= corruptWeight then
                        (True, 0)
                    else if topWeight <= corruptWeight then
                        (False, 0)
                    else case catMaybes $ (\((_,party), _) -> csRes party) <$> tkts of
                        (res:_) -> (res, 0)
                        [] -> error "Finalization failure: no lottery ticket could be verified" -- This should not be possible under standard assumptions
            oldGrade <- currentGrade <<.= newGrade
            when (newGrade == 2 && oldGrade /= 2) $
                sendABBAMessage (WeAreDone nextBit)
            currentPhase .= phase + 1
            beginPhase (phase + 1)
            tkt <- view $ myTicket (phase + 1)
            sendABBAMessage (Justified (phase+1) nextBit tkt)

beginPhase :: (ABBAMonad m) => Phase -> m ()
beginPhase phase = use (phaseState phase . phaseCSSState) >>= \case
        Left (justif, msgs) -> do
            phaseState phase . phaseCSSState .= Right initialCSSState
            case justif of
                Nothing -> return ()
                Just (Just c) -> liftCSSJustifyChoice phase c
                Just Nothing -> liftCSSJustifyChoice phase False >> liftCSSJustifyChoice phase True
            forM_ msgs $ uncurry (liftCSSReceiveMessage phase)
        Right _ -> return ()
{-# INLINE beginPhase #-}

-- |Get the lottery identifier string for the given phase.
lotteryId :: Phase -> SimpleGetter ABBAInstance BS.ByteString
lotteryId phase = to $ \a ->
        Ser.runPut $ Ser.put (baid a) >> Ser.put phase

-- |Get my lottery ticket for the given phase.
myTicket :: Phase -> SimpleGetter ABBAInstance TicketProof
myTicket phase = to $ \a ->
        makeTicketProof (a ^. lotteryId phase) (privateKey a)

{-# INLINE unlessCompleted #-}
unlessCompleted :: (ABBAMonad m) => m () -> m ()
unlessCompleted a = do
        c <- use completed
        unless c a

-- |Called to indicate that a given choice is justified.
{-# SPECIALIZE justifyABBAChoice :: Choice -> ABBA () #-}
justifyABBAChoice :: (ABBAMonad m) => Choice -> m ()
justifyABBAChoice c = unlessCompleted $ liftCSSJustifyChoice 0 c

-- |Called when an 'ABBAMessage' is received.
{-# SPECIALIZE receiveABBAMessage :: Party -> ABBAMessage -> ABBA () #-}
receiveABBAMessage :: (ABBAMonad m) => Party -> ABBAMessage -> m ()
receiveABBAMessage src (Justified phase c ticketProof) = unlessCompleted $ do
    ABBAInstance{..} <- ask
    liftCSSReceiveMessage phase src (Input c)
    let ticket = proofToTicket ticketProof (partyWeight src) totalWeight
    phaseState phase . lotteryTickets . at (ticketValue ticket, src) ?= ticket
    inputw <- use $ phaseState phase . inputWeight c
    forM_ inputw $ \(w, ps) -> unless (src `Set.member` ps) $
        if w + partyWeight src > corruptWeight then do
            phaseState phase . inputWeight c .= Nothing
            liftCSSJustifyChoice (phase + 1) c
        else
            phaseState phase . inputWeight c .= Just (w + partyWeight src, Set.insert src ps)
receiveABBAMessage src (CSSSeen phase p c) =
    unlessCompleted $ liftCSSReceiveMessage phase src (Seen p c)
receiveABBAMessage src (CSSDoneReporting phase m) =
    unlessCompleted $ liftCSSReceiveMessage phase src (DoneReporting m)
receiveABBAMessage src (WeAreDone c) = unlessCompleted $ do
    ABBAInstance{..} <- ask
    alreadyDone <- weAreDone c <<%= Set.insert src
    unless (src `Set.member` alreadyDone) $ do
        owadw <- weAreDoneWeight c <<%= (+ partyWeight src)
        when (owadw + partyWeight src >= totalWeight - corruptWeight && owadw < totalWeight - corruptWeight) $ do
            completed .= True
            aBBAComplete c

-- |Called to start the ABBA protocol
{-# SPECIALIZE beginABBA :: Choice -> ABBA () #-}
beginABBA :: (ABBAMonad m) => Choice -> m ()
beginABBA c = unlessCompleted $ do
    cp <- use currentPhase
    when (cp == 0) $ do
        tkt <- view $ myTicket 0
        sendABBAMessage (Justified 0 c tkt)

