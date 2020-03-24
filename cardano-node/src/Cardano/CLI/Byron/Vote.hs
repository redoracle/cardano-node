{-# LANGUAGE GADTs #-}

module Cardano.CLI.Byron.Vote
  ( createByronVote
  , submitByronVote
  ) where

import           Cardano.Prelude

import           Codec.CBOR.Read (deserialiseFromBytes)
import qualified Data.ByteString.Lazy as LB

import qualified Cardano.Binary as Binary
import           Cardano.Config.Types
import           Cardano.Chain.Genesis (GenesisData(..))
import           Cardano.Chain.Update
                   (AVote(..), ProtocolVersion(..), SoftwareVersion(..),
                    UpId, Vote(..), mkVote)
import           Cardano.Crypto.Signing (SigningKey)
import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)
import qualified Ouroboros.Consensus.Byron.Ledger.Mempool as Mempool
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Mempool as Mempool
import           Ouroboros.Network.IOManager (AssociateWithIOCP)


import           Cardano.CLI.Ops (CliError(..), readGenesis)


createByronVote
  :: ConfigYamlFilePath
  -> SigningKey
  -> UpId
  -> Bool
  -> ExceptT CliError IO (AVote ByteString)
createByronVote config sKey upId voteChoice = do

  nc <- liftIO $ parseNodeConfigurationFP config
  (genData, _) <- readGenesis $ ncGenesisFile nc

  let pmId = gdProtocolMagicId genData

  pure . annotateVote $ mkVote pmId sKey upId voteChoice
 where
  annotateVote :: Vote -> AVote ByteString
  annotateVote (UnsafeVote voterVKey proposalId sig _) =
    let annotatedPropId = Binary.reAnnotate proposalId
    in UnsafeVote voterVKey annotatedPropId sig (Binary.annotation annotatedPropId)


deserialiseByronVote :: LByteString -> Either CliError (Mempool.GenTx ByronBlock)
deserialiseByronVote bs =
  case deserialiseFromBytes Mempool.decodeByronGenTx bs of
    Left deserFail -> Left $ UpdateProposalDecodingError deserFail
    Right (_, genTx) -> Right genTx

submitByronVote
  :: AssociateWithIOCP
  -> ConfigYamlFilePath
  -> DbFile
  -> FilePath
  -> Maybe CLISocketPath
  -> ExceptT CliError IO ()
submitByronVote iocp config dbFp voteFp mSocket = do
  nc <- liftIO $ parseNodeConfigurationFP config

  let genFile = ncGenesisFile nc
      ptcl = ncProtocol nc
      sigThresh = ncPbftSignatureThresh nc
      nMagic = ncReqNetworkMagic nc

  voteBs <- liftIO $ LB.readFile voteFp
  vote <- hoistEither $ deserialiseByronVote voteBs


  mostRecentBlock <- getMostRecentBlock dbFp

  ((ProtocolVersion major minor alt), (SoftwareVersion appName sNumber)) <- hoistEither $ getBlockProtocolAndSoftwareVersion mostRecentBlock

  let lastKnownBlockVersion = LastKnownBlockVersion {lkbvMajor = major, lkbvMinor = minor, lkbvAlt = alt}

  let update = Update appName sNumber $ lastKnownBlockVersion
      skt = chooseSocketPath (ncSocketPath nc) mSocket

  firstExceptT UpdateProposalSubmissionError $ withRealPBFT genFile nMagic sigThresh Nothing Nothing update ptcl $
              \p@Consensus.ProtocolRealPBFT{} -> liftIO $ do
                 traceWith stdoutTracer ("Update proposal TxId: " ++ condense (Mempool.txId proposal))
                   submitGeneralTx iocp skt
                                   (pInfoConfig (Consensus.protocolInfo p))
                                   vote
                                   nullTracer -- stdoutTracer
