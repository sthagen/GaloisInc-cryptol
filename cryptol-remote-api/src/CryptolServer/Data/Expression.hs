{-# OPTIONS_GHC -fno-warn-type-defaults -Wno-missing-deriving-strategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module CryptolServer.Data.Expression
  ( module CryptolServer.Data.Expression
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson as JSON hiding (Encoding, Value, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Scientific as Sc
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Traversable
import qualified Data.Vector as V
import Data.Text.Encoding (encodeUtf8)
import Numeric (showHex)

import Cryptol.Backend.Monad
import qualified Cryptol.Eval.Concrete as Concrete
import Cryptol.Backend.SeqMap (enumerateSeqMap)
import Cryptol.Backend.WordValue (asWordVal)

import Cryptol.Eval (evalSel)
import Cryptol.Eval.Concrete (Value)
import Cryptol.Eval.Type (TValue(..), tValTy)
import Cryptol.Eval.Value (GenValue(..))
import Cryptol.ModuleSystem (getPrimMap, evalDecls)
import Cryptol.ModuleSystem.Env (deNames,meDynEnv)
import Cryptol.ModuleSystem.Name
  (Name, mkDeclared, NameSource( SystemName ),
   nameUnique, nameIdent, liftSupply)
import Cryptol.ModuleSystem.NamingEnv (singletonE, shadowing)

import qualified Cryptol.Parser as CP
import qualified Cryptol.Parser.AST as CP
import Cryptol.Parser.Position (Located(..), emptyRange)
import Cryptol.Parser.Selector
import qualified Cryptol.TypeCheck.AST as TC
import Cryptol.Utils.Ident
import Cryptol.Utils.RecordMap (recordFromFields, canonicalFields)

import Argo
import CryptolServer
import CryptolServer.Exceptions
import CryptolServer.Data.Type

data Encoding = Base64 | Hex
  deriving (Eq, Show, Ord)

instance JSON.FromJSON Encoding where
  parseJSON =
    withText "encoding" $
    \case
      "hex"    -> pure Hex
      "base64" -> pure Base64
      _        -> empty

data LetBinding =
  LetBinding
  { argDefName :: !Text
  , argDefVal  :: !Expression
  }
  deriving (Eq, Show)

instance JSON.FromJSON LetBinding where
  parseJSON =
    withObject "let binding" $ \o ->
      LetBinding <$> o .: "name" <*> o .: "definition"

instance JSON.ToJSON LetBinding where
  toJSON (LetBinding x def) =
    object [ "name" .= x
           , "definition" .= def
           ]

-- | Cryptol expressions which can be serialized into JSON and sent
-- to an RPC client.
data Expression =
    Bit !Bool
  | Unit
  | Num !Encoding !Text !Integer -- ^ data and bitwidth
  | Record !(HashMap Text Expression)
  | Sequence ![Expression]
  | Tuple ![Expression]
  | Integer !Integer
  | IntegerModulo !Integer !Integer -- ^ value, modulus
  | UniqueName !Int !Text
    -- ^ Essentially a Cryptol.ModuleSystem.Name's nameUnique and nameIdent.
    -- Used when we need to send a result back to an RPC client
  | Concrete !Text -- ^ Concrete Cryptol syntax as a string -- the Cryptol parser
                   -- will establish it's meaning based on the current focus/context
  | Let ![LetBinding] !Expression
  | Application !Expression !(NonEmpty Expression)
  | TypeApplication !Expression !TypeArguments
  deriving (Eq, Show)

newtype TypeArguments = TypeArguments (Map Ident JSONPType)
  deriving (Eq, Show) via Map Ident (CP.Type CP.PName)

data ExpressionTag
  = TagNum
  | TagRecord
  | TagSequence
  | TagTuple
  | TagUnit
  | TagLet
  | TagApp
  | TagTypeApp
  | TagIntMod
  | TagUniqueName

instance JSON.FromJSON ExpressionTag where
  parseJSON =
    withText "tag" $
    \case
      "bits"           -> pure TagNum
      "unit"           -> pure TagUnit
      "record"         -> pure TagRecord
      "sequence"       -> pure TagSequence
      "tuple"          -> pure TagTuple
      "let"            -> pure TagLet
      "call"           -> pure TagApp
      "instantiate"    -> pure TagTypeApp
      "integer modulo" -> pure TagIntMod
      "unique name"    -> pure TagUniqueName
      _                -> empty

instance JSON.ToJSON ExpressionTag where
  toJSON TagNum        = "bits"
  toJSON TagRecord     = "record"
  toJSON TagSequence   = "sequence"
  toJSON TagTuple      = "tuple"
  toJSON TagUnit       = "unit"
  toJSON TagLet        = "let"
  toJSON TagApp        = "call"
  toJSON TagTypeApp    = "instantiate"
  toJSON TagIntMod     = "integer modulo"
  toJSON TagUniqueName = "unique name"

instance JSON.FromJSON TypeArguments where
  parseJSON =
    withObject "type arguments" $ \o ->
      TypeArguments . Map.fromList <$>
        traverse elt (HM.toList o)
    where
      elt (name, ty) = (mkIdent name,) <$> parseJSON ty

instance JSON.FromJSON Expression where
  parseJSON v = bool v <|> integer v <|> concrete v <|> obj v
    where
      bool =
        withBool "boolean" $ pure . Bit
      integer =
        -- Note: this means that we should not expose this API to the
        -- public, but only to systems that will validate input
        -- integers. Otherwise, they can use this to allocate a
        -- gigantic integer that fills up all memory.
        withScientific "integer" $ \s ->
          case Sc.floatingOrInteger s of
            Left _fl -> empty
            Right i -> pure (Integer i)
      concrete =
        withText "concrete syntax" $ pure . Concrete

      obj =
        withObject "argument" $
        \o -> o .: "expression" >>=
              \case
                TagUnit -> pure Unit
                TagNum ->
                  do enc <- o .: "encoding"
                     Num enc <$> o .: "data" <*> o .: "width"
                TagRecord ->
                  do fields <- o .: "data"
                     flip (withObject "record data") fields $
                       \fs -> Record <$> traverse parseJSON fs
                TagSequence ->
                  do contents <- o .: "data"
                     flip (withArray "sequence") contents $
                       \s -> Sequence . V.toList <$> traverse parseJSON s
                TagTuple ->
                  do contents <- o .: "data"
                     flip (withArray "tuple") contents $
                       \s -> Tuple . V.toList <$> traverse parseJSON s
                TagLet ->
                  Let <$> o .: "binders" <*> o .: "body"
                TagApp ->
                  Application <$> o .: "function" <*> o .: "arguments"
                TagTypeApp ->
                  TypeApplication <$> o .: "generic" <*> o .: "arguments"
                TagIntMod ->
                  IntegerModulo <$> o .: "integer" <*> o .: "modulus"
                TagUniqueName ->
                  IntegerModulo <$> o .: "unique" <*> o .: "identifier"

instance ToJSON Encoding where
  toJSON Hex = String "hex"
  toJSON Base64 = String "base64"

instance JSON.ToJSON Expression where
  toJSON Unit = object [ "expression" .= TagUnit ]
  toJSON (Bit b) = JSON.Bool b
  toJSON (Integer i) = JSON.Number (fromInteger i)
  toJSON (IntegerModulo i n) =
    object [ "expression" .= TagIntMod
           , "integer" .= JSON.Number (fromInteger i)
           , "modulus" .= JSON.Number (fromInteger n)
           ]
  toJSON (Concrete expr) = toJSON expr
  toJSON (Num enc dat w) =
    object [ "expression" .= TagNum
           , "data" .= String dat
           , "encoding" .= enc
           , "width" .= w
           ]
  toJSON (Record fields) =
    object [ "expression" .= TagRecord
           , "data" .= object [ fieldName .= toJSON val
                              | (fieldName, val) <- HM.toList fields
                              ]
           ]
  toJSON (Sequence elts) =
    object [ "expression" .= TagSequence
           , "data" .= Array (V.fromList (map toJSON elts))
           ]
  toJSON (Tuple projs) =
    object [ "expression" .= TagTuple
           , "data" .= Array (V.fromList (map toJSON projs))
           ]
  toJSON (Let binds body) =
    object [ "expression" .= TagLet
           , "binders" .= Array (V.fromList (map toJSON binds))
           , "body" .= toJSON body
           ]
  toJSON (Application fun args) =
    object [ "expression" .= TagApp
           , "function" .= fun
           , "arguments" .= args
           ]
  toJSON (UniqueName unique name) =
    object [ "expression" .= TagUniqueName
           , "unique" .= toJSON unique
           , "identifier" .= toJSON name
           ]
  toJSON (TypeApplication gen _args) =
    -- It would be dead code to do anything here, as type
    -- instantiations are not values. This code is called only as part
    -- of translating values (e.g. from "evaluate expression"). So we
    -- just fall through, rather than writing complicated code to
    -- serialize Type PName that never gets called and just bitrots.
    toJSON gen


decode :: (Argo.Method m, Monad m) => Encoding -> Text -> m Integer
decode Base64 txt =
  let bytes = encodeUtf8 txt
  in
    case Base64.decode bytes of
      Left err ->
        Argo.raise (invalidBase64 bytes err)
      Right decoded -> return $ bytesToInt decoded
decode Hex txt =
  squish <$> traverse hexDigit (T.unpack txt)
  where
    squish = foldl (\acc i -> (acc * 16) + i) 0

hexDigit :: (Argo.Method m, Monad m) => Num a => Char -> m a
hexDigit '0' = pure 0
hexDigit '1' = pure 1
hexDigit '2' = pure 2
hexDigit '3' = pure 3
hexDigit '4' = pure 4
hexDigit '5' = pure 5
hexDigit '6' = pure 6
hexDigit '7' = pure 7
hexDigit '8' = pure 8
hexDigit '9' = pure 9
hexDigit 'a' = pure 10
hexDigit 'A' = pure 10
hexDigit 'b' = pure 11
hexDigit 'B' = pure 11
hexDigit 'c' = pure 12
hexDigit 'C' = pure 12
hexDigit 'd' = pure 13
hexDigit 'D' = pure 13
hexDigit 'e' = pure 14
hexDigit 'E' = pure 14
hexDigit 'f' = pure 15
hexDigit 'F' = pure 15
hexDigit c   = Argo.raise (invalidHex c)


-- nameUniqueToName :: Int -> Argo.Command ServerState Name
-- nameUniqueToName n = do
--   evalEnv <- meEvalEnv <$> view moduleEnv <$> Argo.getState
--   error "TODO"


getExpr :: Expression -> CryptolCommand (CP.Expr CP.PName)
getExpr = CryptolCommand . const . getCryptolExpr

-- N.B., used in SAWServer as well, hence the more generic monad
getCryptolExpr ::
  (Argo.Method m, Monad m) =>
  Expression ->
  m (CP.Expr CP.PName)
getCryptolExpr (UniqueName unique name) = error "TODO / FIXME"
getCryptolExpr Unit =
  return $
    CP.ETyped
      (CP.ETuple [])
      (CP.TTuple [])
getCryptolExpr (Bit b) =
  return $
    CP.ETyped
      (CP.EVar (CP.UnQual (mkIdent $ if b then "True" else "False")))
      CP.TBit
getCryptolExpr (Integer i) =
  return $
    CP.ETyped
      (CP.ELit (CP.ECNum i (CP.DecLit (pack (show i)))))
      (CP.TUser (CP.UnQual (mkIdent "Integer")) [])
getCryptolExpr (IntegerModulo i n) =
  return $
    CP.ETyped
      (CP.ELit (CP.ECNum i (CP.DecLit (pack (show i)))))
      (CP.TUser (CP.UnQual (mkIdent "Z")) [CP.TNum n])
getCryptolExpr (Num enc txt w) =
  do d <- decode enc txt
     return $ CP.ETyped
       (CP.ELit (CP.ECNum d (CP.DecLit txt)))
       (CP.TSeq (CP.TNum w) CP.TBit)
getCryptolExpr (Record fields) =
  fmap (CP.ERecord . recordFromFields) $ for (HM.toList fields) $
  \(recName, spec) ->
    (mkIdent recName,) . (emptyRange,) <$> getCryptolExpr spec
getCryptolExpr (Sequence elts) =
  CP.EList <$> traverse getCryptolExpr elts
getCryptolExpr (Tuple projs) =
  CP.ETuple <$> traverse getCryptolExpr projs
getCryptolExpr (Concrete syntax) =
  case CP.parseExpr syntax of
    Left err ->
      Argo.raise (cryptolParseErr syntax err)
    Right e -> pure e
getCryptolExpr (Let binds body) =
  CP.EWhere <$> getCryptolExpr body <*> traverse mkBind binds
  where
    mkBind (LetBinding x rhs) =
      CP.DBind .
      (\bindBody ->
         CP.Bind { CP.bName = fakeLoc (CP.UnQual (mkIdent x))
              , CP.bParams = []
              , CP.bDef = bindBody
              , CP.bSignature = Nothing
              , CP.bInfix = False
              , CP.bFixity = Nothing
              , CP.bPragmas = []
              , CP.bMono = True
              , CP.bDoc = Nothing
              , CP.bExport = CP.Public
              }) .
      fakeLoc .
      CP.DExpr <$>
        getCryptolExpr rhs

    fakeLoc = Located emptyRange
getCryptolExpr (Application fun (arg :| [])) =
  CP.EApp <$> getCryptolExpr fun <*> getCryptolExpr arg
getCryptolExpr (Application fun (arg1 :| (arg : args))) =
  getCryptolExpr (Application (Application fun (arg1 :| [])) (arg :| args))
getCryptolExpr (TypeApplication gen (TypeArguments args)) =
  CP.EAppT <$> getCryptolExpr gen <*> pure (map inst (Map.toList args))
  where
    inst (n, t) = CP.NamedInst (CP.Named (Located emptyRange n) (unJSONPType t))

-- TODO add tests that this is big-endian
-- | Interpret a ByteString as an Integer
bytesToInt :: BS.ByteString -> Integer
bytesToInt bs =
  BS.foldl' (\acc w -> (acc * 256) + toInteger w) 0 bs

-- | Read back a typed value if possible.
readBack :: TValue -> Value -> CryptolCommand (Maybe Expression)
readBack ty val =
  case ty of
    TVRec tfs -> do
      vals <- mapM readBackRecFld $ canonicalFields tfs
      pure $ (Record . HM.fromList) <$> sequence vals
    TVTuple [] ->
      pure $ Just Unit
    TVTuple ts -> do
      vals <- mapM readBackTupleFld $ zip [0..] ts
      pure $ Tuple <$> sequence vals
    TVBit ->
      case val of
        VBit b -> pure $ Just $ Bit b
        _ -> mismatchPanic
    TVInteger ->
      case val of
        VInteger i -> pure $ Just $ Integer i
        _ -> mismatchPanic
    TVIntMod n ->
      case val of
        VInteger i -> pure $ Just $ IntegerModulo i n
        _ -> mismatchPanic
    TVSeq len elemTy
      | len == 0 ->
        pure $ Just Unit
      | elemTy == TVBit
      , VWord width wv <- val -> do
        Concrete.BV w v <- liftEval $ asWordVal Concrete.Concrete wv
        let hexStr = T.pack $ showHex v ""
        let paddedLen = fromIntegral ((width `quot` 4) + (if width `rem` 4 == 0 then 0 else 1))
        pure $ Just $  Num Hex (T.justifyRight paddedLen '0' hexStr) w
      | VSeq _l (enumerateSeqMap len -> mVals) <- val -> do
        vals <- liftEval $ sequence mVals
        es <- mapM (readBack elemTy) vals
        pure $ Sequence <$> sequence es

    _ -> do
      -- The above cases are for types that can unambiguously be converted into
      -- syntax; this case instead tries to essentially let-bind the value to a
      -- fresh variable so we can send that to the RPC client instead. They
      -- obviously won't be able to directly inspect the value, but they can
      -- pass it to other functions/commands via a RPC.
      mName <- bindValToFreshName "rpc" ty val
      case mName of
        Nothing -> pure Nothing
        Just name -> pure $ Just $ UniqueName (nameUnique name) (identText $ nameIdent name)
  where
    mismatchPanic :: CryptolCommand (Maybe Expression)
    mismatchPanic =
      error $ "Internal error: readBack: value '" <>
              show val <>
              "' didn't match type '" <>
              show (tValTy ty) <>
              "'"
    readBackRecFld :: (Ident, TValue) -> CryptolCommand (Maybe (Text, Expression))
    readBackRecFld (fldId, fldTy) = do
      fldVal <- liftEval $ evalSel Concrete.Concrete val (RecordSel fldId Nothing)
      fldExpr <- readBack fldTy fldVal
      pure $ (identText fldId,) <$> fldExpr
    readBackTupleFld :: (Int, TValue) -> CryptolCommand (Maybe Expression)
    readBackTupleFld (i, iTy) = do
      iVal <- liftEval $ evalSel Concrete.Concrete val (TupleSel i Nothing)
      readBack iTy iVal


bindValToFreshName :: Text -> TValue -> Concrete.Value -> CryptolCommand (Maybe Name)
bindValToFreshName name ty val = do
  prims   <- liftModuleCmd getPrimMap
  mb      <- liftEval (Concrete.toExpr prims ty val)
  case mb of
    Nothing   -> return Nothing
    Just expr -> do
      name' <- genFreshName
      let schema = TC.Forall { TC.sVars  = []
                              , TC.sProps = []
                              , TC.sType  = tValTy ty
                          }
          decl = TC.Decl { TC.dName       = name'
                          , TC.dSignature  = schema
                          , TC.dDefinition = TC.DExpr expr
                          , TC.dPragmas    = []
                          , TC.dInfix      = False
                          , TC.dFixity     = Nothing
                          , TC.dDoc        = Nothing
                          }
      liftModuleCmd (evalDecls [TC.NonRecursive decl])
      denv <- meDynEnv <$> getModuleEnv
      let nenv' = singletonE (CP.UnQual (mkIdent name)) name'
                              `shadowing` deNames denv
      modifyModuleEnv $ \me -> me {meDynEnv = denv { deNames = nenv' }}
      return $ Just name'
  where
    genFreshName :: CryptolCommand Name
    genFreshName =
      liftSupply (mkDeclared NSValue mpath SystemName ident Nothing emptyRange)
      where ident = mkIdent name
            mpath = TopModule interactiveName


liftEval :: Eval a -> CryptolCommand a
liftEval e = liftIO (runEval mempty e)

mkEApp :: CP.Expr CP.PName -> [CP.Expr CP.PName] -> CP.Expr CP.PName
mkEApp f args = foldl CP.EApp f args
