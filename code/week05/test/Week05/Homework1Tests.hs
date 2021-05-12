{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Week05.Homework1Tests where

import           Plutus.V1.Ledger.Ada ( lovelaceValueOf )
import           Plutus.V1.Ledger.Api ( PubKeyHash )
import           Plutus.V1.Ledger.Contexts ( ScriptContext (..),
                   ScriptPurpose (Minting), TxInfo (..), pubKeyHash )
import           Plutus.V1.Ledger.Interval
import           Plutus.V1.Ledger.Slot
import           Plutus.V1.Ledger.TxId
import qualified Plutus.V1.Ledger.Value as Value
import           PlutusTx.Prelude
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Wallet.Emulator ( Wallet (..), walletPubKey )

import           Week05.Homework1 ( mkPolicy )


tests :: TestTree
tests = testGroup "week05 Homework1 policy tests"
  [ testProperty "transaction is entirely within deadline" prop_TransactionWithinDeadline
  , testProperty "transaction may be past deadline" prop_TransactionPastDeadline
  , testProperty "transaction pkh in signatories" prop_PkhInSignatories
  , testProperty "transaction pkh not in signatories" prop_PkhNotInSignatories
  ]

prop_TransactionWithinDeadline :: Integer -> Integer -> Integer -> Property
prop_TransactionWithinDeadline rangeLow rangeHigh deadline =
  let pkh = mkWallet 1
      signatories = [mkWallet 1]
      txRange = interval (Slot rangeLow) (Slot rangeHigh)
  in
      (rangeLow < rangeHigh) && (rangeHigh <= deadline) ==>
        mkPolicy pkh (Slot deadline) $ mkContext signatories txRange

prop_TransactionPastDeadline :: Integer -> Integer -> Integer -> Property
prop_TransactionPastDeadline rangeLow rangeHigh deadline =
  let pkh = mkWallet 1
      signatories = [mkWallet 1]
      txRange = interval (Slot rangeLow) (Slot rangeHigh)
  in
      (rangeLow < rangeHigh) && (rangeHigh > deadline) ==>
        not $ mkPolicy pkh (Slot deadline) $ mkContext signatories txRange

prop_PkhInSignatories :: Integer -> Property
prop_PkhInSignatories walletId =
  let pkh = mkWallet walletId
      signatories = [mkWallet walletId, mkWallet (walletId + 1)]
      deadline = 10
      txRange = interval (Slot (deadline - 2)) (Slot deadline)
  in
      walletId > 0 ==>
        mkPolicy pkh (Slot deadline) $ mkContext signatories txRange

prop_PkhNotInSignatories :: Integer -> Property
prop_PkhNotInSignatories walletId =
  let pkh = mkWallet walletId
      signatories = [mkWallet (walletId + 2), mkWallet (walletId + 1)]
      deadline = 10
      txRange = interval (Slot (deadline - 2)) (Slot deadline)
  in
      walletId > 0 ==>
        not $ mkPolicy pkh (Slot deadline) $ mkContext signatories txRange

-- Convenience function to make a wallet pub key hash from an emulator wallet id
mkWallet :: Integer -> PubKeyHash
mkWallet = pubKeyHash . walletPubKey . Wallet

-- Construct a minimal ScriptContext with some values relevant to our policy script tests
mkContext :: [PubKeyHash] -> Interval Slot -> ScriptContext
mkContext signatories txRange = ScriptContext
  { scriptContextTxInfo = TxInfo
    { txInfoInputs = []
    , txInfoInputsFees = []
    , txInfoOutputs = []
    , txInfoFee = lovelaceValueOf 10
    , txInfoForge = Value.singleton "a8ff" "ABC" 1
    , txInfoDCert = []
    , txInfoWdrl = []
    , txInfoValidRange = txRange
    , txInfoSignatories = signatories
    , txInfoData = []
    , txInfoId = TxId "b9dd"
    }
  , scriptContextPurpose = Minting "a8ff"
  }
