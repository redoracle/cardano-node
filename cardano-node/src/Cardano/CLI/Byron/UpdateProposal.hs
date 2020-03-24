{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.UpdateProposal
  ( ParametersToUpdate(..)
  , convertProposalToGenTx
  , createUpdateProposal
  , deserialiseByronUpdateProposal
  , serialiseByronUpdateProposal
  , submitByronUpdateProposal
  ) where

import           Cardano.Prelude

import           Control.Monad.Trans.Except.Extra
                   (firstExceptT, handleIOExceptT, hoistEither, left)
import           Control.Tracer (nullTracer, stdoutTracer, traceWith)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.Directory
                   (canonicalizePath, getModificationTime,
                    listDirectory, makeAbsolute)
import           System.FilePath ((</>))
import           Data.Time.Clock (UTCTime)

import qualified Cardano.Binary as Binary
import           Cardano.Chain.Block
                   (AHeader(..), ABlockOrBoundaryHdr(..),
                    abobHdrFromBlock, fromCBORABlockOrBoundary)
import           Cardano.Chain.Common (LovelacePortion, TxFeePolicy(..))
import           Cardano.Chain.Epoch.File (mainnetEpochSlots)
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Slotting (EpochNumber(..), SlotNumber(..))
import           Cardano.Chain.Update
                   (AProposal(..), ProtocolParametersUpdate(..),
                    InstallerHash(..), Proposal, ProposalBody(..), ProtocolVersion(..),
                    SoftforkRule(..), SoftwareVersion(..), SystemTag(..), recoverUpId,
                    signProposal)
import           Cardano.Config.Types
import           Ouroboros.Consensus.Util.Condense (condense)
import           Cardano.Crypto.Signing (SigningKey, noPassSafeSigner)
import           Cardano.Node.Submission (submitGeneralTx)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Consensus.Node.ProtocolInfo (pInfoConfig)
import           Ouroboros.Network.IOManager (AssociateWithIOCP)

import           Cardano.CLI.Ops (CliError(..), decodeCBOR, readGenesis, withRealPBFT)
import           Cardano.Common.LocalSocket

data ParametersToUpdate =
    ScriptVersion Word16
  | SlotDuration Natural
  | MaxBlockSize Natural
  | MaxHeaderSize Natural
  | MaxTxSize Natural
  | MaxProposalSize Natural
  | MpcThd LovelacePortion
  | HeavyDelThd LovelacePortion
  | UpdateVoteThd LovelacePortion
  -- ^ UpdateVoteThd: This represents the minimum percentage of the total number of genesis
  -- keys that have to endorse a protocol version to be able to become adopted.
  | UpdateProposalThd LovelacePortion
  -- ^ UpdateProposalTTL: If after the number of slots specified the proposal
  -- does not reach majority of approvals, the proposal is simply discarded.
  | UpdateProposalTTL SlotNumber
  | SoftforkRuleParam SoftforkRule
  | TxFeePolicy TxFeePolicy
  | UnlockStakeEpoch EpochNumber
  deriving Show

createProtocolParametersUpdate
  :: ProtocolParametersUpdate
  -> [ParametersToUpdate]
  -> ProtocolParametersUpdate
createProtocolParametersUpdate init paramsToUpdate = go init paramsToUpdate
 where go i [] = i
       go i (paramToUpdate : rest) =
         case paramToUpdate of
           ScriptVersion val -> go i{ppuScriptVersion = Just val} rest
           SlotDuration val -> go i{ppuSlotDuration = Just val} rest
           MaxBlockSize val -> go i{ppuMaxBlockSize = Just val} rest
           MaxHeaderSize val -> go i{ppuMaxHeaderSize = Just val} rest
           MaxTxSize val -> go i{ppuMaxTxSize = Just val} rest
           MaxProposalSize val -> go i{ppuMaxProposalSize = Just val} rest
           MpcThd val -> go i{ppuMpcThd = Just val} rest
           HeavyDelThd val -> go i{ppuHeavyDelThd = Just val} rest
           UpdateVoteThd val -> go i{ppuUpdateVoteThd = Just val} rest
           UpdateProposalThd val -> go i{ppuUpdateProposalThd = Just val} rest
           UpdateProposalTTL val -> go i{ppuUpdateProposalTTL = Just val} rest
           SoftforkRuleParam val -> go i{ppuSoftforkRule = Just val} rest
           TxFeePolicy val -> go i{ppuTxFeePolicy = Just val} rest
           UnlockStakeEpoch val -> go i{ppuUnlockStakeEpoch = Just val} rest

convertProposalToGenTx :: AProposal ByteString -> Mempool.GenTx ByronBlock
convertProposalToGenTx prop = Mempool.ByronUpdateProposal (recoverUpId prop) prop

createUpdateProposal
  :: DbFile
  -> ConfigYamlFilePath
  -> SigningKey
  -> [ParametersToUpdate]
  -> ExceptT CliError IO Proposal
createUpdateProposal dbFile configFile sKey paramsToUpdate = do

  latestBlockBS <- getMostRecentBlock dbFile

  nc <- liftIO $ parseNodeConfigurationFP configFile
  (genData, _) <- readGenesis $ ncGenesisFile nc

  let metaData :: M.Map SystemTag InstallerHash
      metaData = M.empty
      noPassSigningKey = noPassSafeSigner sKey
      pmId = gdProtocolMagicId genData
      protocolParamsUpdate = createProtocolParametersUpdate emptyProtocolParametersUpdate paramsToUpdate

  (protocolVersion', softwareVersion') <- hoistEither $ getBlockProtocolAndSoftwareVersion latestBlockBS

  let proposalBody = ProposalBody protocolVersion' protocolParamsUpdate softwareVersion' metaData

  let proposal = signProposal pmId proposalBody noPassSigningKey

  pure proposal

getFileModificationTime :: FilePath -> IO (FilePath, UTCTime)
getFileModificationTime fp = getModificationTime fp >>= \timeModded -> pure (fp, timeModded)

getBlockProtocolAndSoftwareVersion
  :: LByteString
  -> Either CliError (ProtocolVersion, SoftwareVersion)
getBlockProtocolAndSoftwareVersion latestBlockBS = do
  latestBlock <- case decodeCBOR latestBlockBS (fromCBORABlockOrBoundary mainnetEpochSlots) of
                   Right (_, blk) -> Right blk
                   Left err -> Left err

  let header' = abobHdrFromBlock latestBlock
  protocolVersion' <- getProtocolVersion header'
  softwareVersion' <- getSoftwareVersion header'
  pure (protocolVersion', softwareVersion')


-- | Gets the most recent block in the volatile database.
getMostRecentBlock :: DbFile -> ExceptT CliError IO LByteString
getMostRecentBlock (DbFile dbFp) = do
 let volatileDbDir = dbFp </> "volatile"

 blockFps <- liftIO $ listDirectory volatileDbDir

 let adjustedBlockFps = map (\fp -> volatileDbDir </> fp) blockFps

 absFps <- liftIO $ mapM (makeAbsolute >=> canonicalizePath) adjustedBlockFps

 -- Creates a list of tuples containing the time that block was modified and the block filepath in question.
 blocksFpsAndModTime <- handleIOExceptT
                          (UpdateProposalFileModificationError absFps . T.pack . displayException)
                          $ mapM getFileModificationTime absFps

 -- Sorts the blocks in the volatile db in descending order based on the time it was modified.
 mRb <- case sortDescending blocksFpsAndModTime of
          [] -> left $ NoBlocksFound dbFp
          ((mostRecentBlock, _) : _) -> pure mostRecentBlock

 handleIOExceptT (UpdateProposalBlockReadError dbFp . T.pack . displayException) $ LB.readFile mRb

emptyProtocolParametersUpdate :: ProtocolParametersUpdate
emptyProtocolParametersUpdate =
  ProtocolParametersUpdate
    { ppuScriptVersion = Nothing
    , ppuSlotDuration = Nothing
    , ppuMaxBlockSize = Nothing
    , ppuMaxHeaderSize = Nothing
    , ppuMaxTxSize = Nothing
    , ppuMaxProposalSize = Nothing
    , ppuMpcThd = Nothing
    , ppuHeavyDelThd = Nothing
    , ppuUpdateVoteThd = Nothing
    , ppuUpdateProposalThd = Nothing
    , ppuUpdateProposalTTL = Nothing
    , ppuSoftforkRule = Nothing
    , ppuTxFeePolicy = Nothing
    , ppuUnlockStakeEpoch = Nothing
    }

getProtocolVersion :: ABlockOrBoundaryHdr a -> Either CliError ProtocolVersion
getProtocolVersion (ABOBBlockHdr aHeader) = Right $ headerProtocolVersion aHeader
getProtocolVersion (ABOBBoundaryHdr _) =
  Left . UpdateProposalEpochBoundaryBlockError
       $ "Cardano.CLI.Byron.UpdateProposal.getProtocolVersion: "
       <> "encountered an epoch boundary block which does not have a ProtocolVersion."
       <> " Wait a moment to download an additional block and try again."

getSoftwareVersion :: ABlockOrBoundaryHdr a -> Either CliError SoftwareVersion
getSoftwareVersion (ABOBBlockHdr aHeader) = Right $ headerSoftwareVersion aHeader
getSoftwareVersion (ABOBBoundaryHdr _) =
    Left . UpdateProposalEpochBoundaryBlockError
         $ "Cardano.CLI.Byron.UpdateProposal.getSoftwareVersion: "
         <> "encountered an epoch boundary block which does not have a SoftwareVersion."
         <> " Wait a moment to download an additional block and try again."

serialiseByronUpdateProposal :: Proposal -> LByteString
serialiseByronUpdateProposal = Binary.serialize

deserialiseByronUpdateProposal :: LByteString -> Either CliError (AProposal ByteString)
deserialiseByronUpdateProposal bs =
  case Binary.decodeFull bs :: Either Binary.DecoderError Proposal of
    Left deserFail -> Left $ UpdateProposalDecodingError deserFail
    Right proposal -> Right $ annotateProposal proposal
 where
  annotateProposal :: Proposal -> AProposal ByteString
  annotateProposal (AProposal bdy issuer' sig _) =
    let annotatedBody = Binary.reAnnotate bdy
    in AProposal annotatedBody issuer' sig (Binary.annotation annotatedBody)

sortDescending :: [(FilePath, UTCTime)] -> [(FilePath, UTCTime)]
sortDescending ls = sortBy (\(firMod, _) (secMod, _) -> compare secMod firMod) ls

submitByronUpdateProposal
  :: AssociateWithIOCP
  -> ConfigYamlFilePath
  -> FilePath
  -> DbFile
  -> Maybe CLISocketPath
  -> ExceptT CliError IO ()
submitByronUpdateProposal iocp config proposalFp dbFp mSocket = do
    nc <- liftIO $ parseNodeConfigurationFP config

    let genFile = ncGenesisFile nc
        ptcl = ncProtocol nc
        sigThresh = ncPbftSignatureThresh nc
        nMagic = ncReqNetworkMagic nc

    proposalBs <- liftIO $ LB.readFile proposalFp
    aProposal <- hoistEither $ deserialiseByronUpdateProposal proposalBs
    let genTx = convertProposalToGenTx aProposal


    mostRecentBlock <- getMostRecentBlock dbFp

    ((ProtocolVersion major minor alt), (SoftwareVersion appName sNumber)) <- hoistEither $ getBlockProtocolAndSoftwareVersion mostRecentBlock

    let lastKnownBlockVersion = LastKnownBlockVersion {lkbvMajor = major, lkbvMinor = minor, lkbvAlt = alt}

    let update = Update appName sNumber $ lastKnownBlockVersion
        skt = chooseSocketPath (ncSocketPath nc) mSocket

    firstExceptT UpdateProposalSubmissionError $ withRealPBFT genFile nMagic sigThresh Nothing Nothing update ptcl $
                \p@Consensus.ProtocolRealPBFT{} -> liftIO $ do
                   traceWith stdoutTracer ("Update proposal TxId: " ++ condense (Mempool.txId genTx))
                   submitGeneralTx iocp skt
                                   (pInfoConfig (Consensus.protocolInfo p))
                                   genTx
                                   nullTracer -- stdoutTracer
