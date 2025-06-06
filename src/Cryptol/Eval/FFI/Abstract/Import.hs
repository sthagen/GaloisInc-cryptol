{-# Language OverloadedStrings #-}
{-# Language BlockArguments #-}
{-# Language LambdaCase #-}
-- | Export builders for Cryptol values
module Cryptol.Eval.FFI.Abstract.Import (
  cryStartImport,
  cryFinishImport,
  cry_bool,
  cry_small_uint,
  cry_small_sint,
  cry_large_int,
  cry_sign,
  cry_tag,
  cry_double,
  Value,
  Import,
  LargeIntFun,
  ImportErrorMessage(..)
  )where

import qualified Data.IntMap as IntMap
import Data.List(intersperse)
import Data.Vector(Vector)
import qualified Data.Vector as Vector
import Data.IORef
import LibBF(bfFromDouble)
import Foreign
import Foreign.C.Types(CSize(..) )
import Control.Monad.Primitive(PrimState)
import GHC.Num.Compat(bigNatToInteger, bigNatToNegInteger)
import Data.Primitive.PrimArray(MutablePrimArray(..), PrimArray(..),
        mutablePrimArrayContents, newPinnedPrimArray, unsafeFreezePrimArray)

import Cryptol.Utils.PP
import Cryptol.Backend.FloatHelpers
import Cryptol.Utils.RecordMap
import Cryptol.TypeCheck.Solver.InfNat
import Cryptol.Eval.Type
import Cryptol.Eval.Value
import Cryptol.Backend.Monad
import Cryptol.Backend.Concrete
import Cryptol.Backend.SeqMap

import Cryptol.Backend


type Value = SEval Concrete (GenValue Concrete)

-- | Imported Cryptol values (aka \"context\")
data Importer =
    Building [Frame]    -- ^ A partial value
  | Done Value          -- ^ Fully built value
  | Error ImportErrorMessage
    -- ^ Something went wrong
    -- XXX: It'd be nice to store the [Frame] here as well,
    -- but that's a bit of a pain because of missing `Show` instances...

data Mk =
  Mk { mkPrec :: Int, mkPP :: [Doc] -> Doc
     , mkVal :: [Value] -> Either ImportErrorMessage Value
     }

-- | Describes what we are missing
data Frame =
    NeedVal
    -- ^ A primitive value

  | NeedDouble Integer Integer
    -- ^ We are expecting a double to make the given floating point number

  | NeedSign (MutablePrimArray (PrimState IO) Word64)
    -- ^ This is a step of making a BigInt.
    -- We've allocated a buffer and are waiting for it to be filled with the
    -- base-64 digits of of a big int (least significant first),
    -- and to be told what the sign should be.

  | NeedMany Mk [Value] [TValue]
    -- ^ A compound value, fields are like this:
    --  * Constructor for the value
    --  * Parts of the value we have
    --  * Types of the parts of the values we still need,
    --    not counting the hole.

  | NeedOneOf (Vector (ConInfo TValue))
    -- ^ Sum type value, which needs a constructor tag to proceed.

-- | Fill the "hole" with the given value.
haveValue :: Value -> [Frame] -> Importer
haveValue v fs =
  case fs of
    [] -> Done v
    f : more ->
      case f of
        NeedVal -> haveValue v more
        NeedDouble {} -> Error (ProtocolMismatch AFloat AValue)
        NeedMany mk vs ts ->
          let done = v : vs in
          case ts of
            [] -> case mkVal mk (reverse done) of
                    Left err -> Error err
                    Right a  -> haveValue a more
            t : ts' -> needValue t (NeedMany mk done ts' : more)
        NeedOneOf {} -> Error (ProtocolMismatch ATag AValue)
        NeedSign {} -> Error (ProtocolMismatch ASign AValue)

-- | Provide a constructor tag
haveTag :: Int -> [Frame] -> Importer
haveTag n fs0 =
  case fs0 of
    [] -> Error UnexpectedData
    f : fs ->
      case f of
        NeedVal     -> Error (ProtocolMismatch AValue ATag)
        NeedDouble {} -> Error (ProtocolMismatch AFloat ATag)
        NeedMany {} -> Error (ProtocolMismatch AValue ATag)
        NeedSign {} -> Error (ProtocolMismatch ASign ATag)
        NeedOneOf opts ->
          case opts Vector.!? n of
            Nothing -> Error (TagOutOfBounds n)
            Just ci ->
              case Vector.toList (conFields ci) of
                [] -> haveValue (mkV []) fs
                t : ts ->
                  needValue t (NeedMany (Mk 10 ppV (Right . mkV)) [] ts : fs)
              where
              ppV xs = pp (conIdent ci) <+> hsep xs

              mkV :: [Value] -> Value
              mkV vs = pure (VEnum
                              (toInteger n)
                              (IntMap.singleton n
                                  ci { conFields = Vector.fromList vs }))


haveSign :: Bool -> [Frame] -> IO Importer
haveSign isPos fs0 =
  case fs0 of
    [] -> pure (Error UnexpectedData)
    f : fs ->
      case f of
        NeedVal -> mismatch AValue
        NeedDouble {} -> mismatch AValue
        NeedMany {} -> mismatch AValue
        NeedOneOf {} -> mismatch ATag
        NeedSign buf ->
          do PrimArray fbuf <- unsafeFreezePrimArray buf
             let i = if isPos then bigNatToInteger fbuf else bigNatToNegInteger fbuf
             i `seq` pure (haveValue (pure (VInteger i)) fs)
  where
  mismatch x = pure (Error (ProtocolMismatch x ASign))

haveFloat :: Double -> [Frame] -> Importer
haveFloat d fs0 =
  case fs0 of
    [] -> Error UnexpectedData
    f : fs ->
      case f of
        NeedDouble e p -> haveValue v fs
          where v = pure (VFloat (BF e p (bfFromDouble d))) :: Value
        NeedVal -> Error (ProtocolMismatch AValue AFloat)
        NeedMany {} -> Error (ProtocolMismatch AValue AFloat)
        NeedOneOf {} -> Error (ProtocolMismatch ATag AFloat)
        NeedSign {} -> Error (ProtocolMismatch ASign AFloat)


-- | Make a "hole" of the given type.
needValue :: TValue -> [Frame] -> Importer
needValue tval fs =
  case tval of
    TVBit       -> Building (NeedVal : fs)
    TVInteger   -> Building (NeedVal : fs)
    TVIntMod _  -> Building (NeedVal : fs)
    TVFloat e p -> Building (NeedDouble e p : fs)

    TVRational ->
      Building (NeedVal : NeedMany (Mk 10 ppV mkV) [] [TVInteger] : fs)
        where
        ppV xs = "Rational" <+> hsep xs
        mkV xs =
          case xs of
            [Ready (VInteger i), Ready (VInteger j)] ->
              Right (VRational <$> ratio Concrete i j)
            _ -> Left BadRationalValue

    TVSeq len elT ->
      case elT of
        TVBit -> Building (NeedVal : NeedMany (Mk 10 ppV mkV) [] [] : fs)
          where
          ppV xs = "Word" <+> integer len <+> hsep xs
          mkV xs =
            case xs of
              [Ready (VInteger i)] -> Right (word Concrete len i)
              _ -> Left BadWordValue

        _ | len < 1 -> haveValue (mkV []) fs
          | otherwise ->
            needValue elT (NeedMany (Mk 0 ppV (Right . mkV)) [] ts : fs)
          where
          ppV xs = brackets (commaSep xs)
          mkV xs = mkSeq Concrete (Nat len) elT (finiteSeqMap Concrete xs)
          ts = replicate (fromInteger len - 1) elT

    TVTuple tys ->
      case tys of
        [] -> haveValue (mkV []) fs
        t : ts -> needValue t (NeedMany (Mk 0 ppV (Right . mkV)) [] ts : fs)
        where
        ppV xs = parens (commaSep xs)
        mkV = pure . VTuple

    TVRec rmp -> doRec rmp

    TVNominal _ _ nv ->
      case nv of
        TVAbstract -> Error (Unsupported "abstract")
        TVStruct rmp -> doRec rmp
        TVEnum cons -> Building (NeedOneOf cons : fs)

    TVFun {}    -> Error (Unsupported "function")
    TVStream {} -> Error (Unsupported "infinite stream")
    TVArray {}  -> Error (Unsupported "array")

  where
  doRec rmp =
    case ts of
      [] -> haveValue (mkV []) fs
      t : ts' -> needValue t (NeedMany (Mk 0 ppV (Right . mkV)) [] ts' : fs)
    where
    (ls,ts) = unzip (canonicalFields rmp)
    mkV vs = pure (VRecord (recordFromFields (zip ls vs)))
    ppV xs = braces (commaSep (zipWith ppF ls xs))
    ppF x y = pp x <+> "_" <+> y


ppValPrec :: Int -> Value -> Doc
ppValPrec p v =
  case ppValuePrec Concrete defaultPPOpts p =<< v of
    Ready doc -> doc
    _ -> "<thunk>"

instance PP Frame where
  ppPrec _ f =
    case f of
      NeedVal -> "_"
      NeedDouble e p -> parens ("_" <+> ":" <+> "Float" <+> integer e <+> integer p)
      NeedMany mk vs ts ->
        let p = mkPrec mk
            args = map (ppValPrec p) vs ++ ["_"] ++ map (ppPrec p . tValTy) ts
        in mkPP mk args
      NeedOneOf vs ->
        hsep (intersperse "|" (map (pp . conIdent) (Vector.toList vs)))
      NeedSign {} -> "BigNum _"



--------------------------------------------------------------------------------

cryStartImport :: TValue -> IO (StablePtr (IORef Importer))
cryStartImport ty =
  do ref <- newIORef (needValue ty [])
     newStablePtr ref

cryFinishImport ::
  StablePtr (IORef Importer) -> IO (Either ImportErrorMessage Value)
cryFinishImport ptr =
  do ref <- deRefStablePtr ptr
     freeStablePtr ptr
     builder <- readIORef ref
     pure $!
       case builder of
         Done v -> Right v
         Error e -> Left e
         Building _ -> Left PartialValue


--------------------------------------------------------------------------------




modifyState :: ([Frame] -> Importer) -> Ptr () -> IO ()
modifyState how ptr =
  do ref <- deRefStablePtr (castPtrToStablePtr ptr)
     modifyIORef' ref \case
       Error err    -> Error err
       Done _       -> Error UnexpectedData
       Building fs  -> how fs


-- | This function assumes that we are the only ones modifying the
-- builder state, so we don't need to worry about race conditions.
modifyStateIO :: ([Frame] -> IO Importer) -> Ptr () -> IO ()
modifyStateIO how ptr =
  do ref <- deRefStablePtr (castPtrToStablePtr ptr)
     builder <- readIORef ref
     newS <- case builder of
               Error err    -> pure (Error err)
               Done _       -> pure (Error UnexpectedData)
               Building fs  -> how fs
     newS `seq` writeIORef ref newS



type Import a = Ptr () -> a -> IO ()

-- | Receive a bool value
cry_bool :: Import Word8
cry_bool self b = modifyState (haveValue $! v) self
  where
  v = if b == 0 then pure (VBit False) else pure (VBit True)

-- | Receive a double value
cry_double :: Import Double
cry_double self b = modifyState (haveFloat $! b) self

-- | Receive a small unsigned integer that fits in 64-bits
cry_small_uint :: Import Word64
cry_small_uint self i = modifyState (haveValue $! v) self
  where
  v = pure $! VInteger $! toInteger i

-- | Receive a small signed integer that fits in 64-bits
cry_small_sint :: Import Int64
cry_small_sint self i = modifyState (haveValue $! v) self
  where
  v = pure $! VInteger $! toInteger i


-- | Receive an integer that's larger than 64-bits.
-- This is part 1 of a 2 step process.
type LargeIntFun = Ptr () -> CSize -> IO (Ptr Word64)
cry_large_int :: LargeIntFun
cry_large_int ptr sz =
  do arr <- newPinnedPrimArray (fromIntegral sz)
     modifyState (\fs -> Building (NeedSign arr : fs)) ptr
     pure (mutablePrimArrayContents arr)

-- | Finish building a large integer.
-- The argument is 1 for negative, 0 for non-negative.
cry_sign :: Import Word8
cry_sign self sign = modifyStateIO (haveSign (sign == 0)) self

-- | Receive a tag for a sum type.
cry_tag :: Import CSize
cry_tag self c = modifyState (haveTag $! fromIntegral c) self
