module Test.PlutusExample.Gen where

import           Cardano.Api
import           Cardano.Api.Shelley
import           Prelude

import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxInfo as Alonzo
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.PlutusExample.ScriptContextChecker
import           Gen.Cardano.Api.Typed
import qualified Ledger as Plutus

import           Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


genPlutusTxOut :: Gen Plutus.TxOut
genPlutusTxOut = do
  alonzoTxOut <-
    TxOut <$> (shelleyAddressInEra <$> genAddressShelley)
          <*> genTxOutValue AlonzoEra
          <*> genTxOutDatumHash AlonzoEra
  Gen.just $ return $ Alonzo.txInfoOut $ toShelleyTxOut ShelleyBasedEraAlonzo alonzoTxOut

genMyCustomRedeemer :: Gen MyCustomRedeemer
genMyCustomRedeemer = MyCustomRedeemer <$> Gen.list (Range.singleton 1) genPlutusTxOut
                                       <*> error "TxIns"
                                       <*> (Alonzo.transValue . toMaryValue <$> genValueForMinting)
                                       <*> genPOSIXTimeRange
                                       <*> (Alonzo.transValue . toMaryValue <$> genValueForTxOut)
                                       <*> genDatumMap
                                       <*> error "Certs"
                                       <*> error "ReqSigners"

genPlutusTxId :: Gen Plutus.TxId
genPlutusTxId =
  Alonzo.txInfoId . toShelleyTxId <$> genTxId

genDatumMap :: Gen [(Plutus.DatumHash, Plutus.Datum)]
genDatumMap =
  map Alonzo.transDataPair <$> Gen.list (Range.linear 0 5) genDatumHashTuple

genDatumHashTuple :: Gen (Alonzo.DataHash StandardCrypto, Alonzo.Data ledgerera)
genDatumHashTuple = do
  sData <- genScriptData
  let ScriptDataHash h = hashScriptData sData
  return (h, toAlonzoData sData)

genPOSIXTimeRange :: Gen Plutus.POSIXTimeRange
genPOSIXTimeRange = do
  ptime <- Plutus.POSIXTime <$> Gen.integral (Range.linear 0 10)
  Gen.element [ Plutus.to ptime
              , Plutus.always
              , Plutus.never
              , Plutus.singleton ptime
              ]
