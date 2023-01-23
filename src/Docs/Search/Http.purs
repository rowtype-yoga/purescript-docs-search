-- | Definitions for the "search REPL".
module Docs.Search.Http where

import Prim hiding (Type, Constraint)

import Commons as Commons
import Control.Parallel (parallel, sequential)
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un, unwrap)
import Data.Search.Trie as Trie
import Data.String.Common (trim) as String
import Data.Tuple.Nested ((/\))
import Docs.Search.Declarations (mkDeclarations)
import Docs.Search.DocsJson (DataDeclType(..))
import Docs.Search.Engine (Result(..), mkEngineState, packageInfoToString)
import Docs.Search.Engine as Engine
import Docs.Search.Extra ((>#>))
import Docs.Search.IndexBuilder (parseModuleHeaders)
import Docs.Search.IndexBuilder as IndexBuilder
import Docs.Search.ModuleIndex (ModuleResult, mkPackedModuleIndex, unpackModuleIndex)
import Docs.Search.NodeEngine (nodeEngine)
import Docs.Search.PackageIndex (PackageResult, mkPackageIndex, mkPackageInfo)
import Docs.Search.Score (mkScores)
import Docs.Search.SearchResult (ResultInfo(..), SearchResult(..))
import Docs.Search.TypeDecoder (Constraint, FunDeps, QualifiedName, Type, TypeArgument)
import Docs.Search.TypeIndex (resultsWithTypes)
import Docs.Search.TypePrinterHttp (showConstraint, showFunDeps, showType, showTypeArgument, space)
import Docs.Search.Types (ModuleName, PackageName, PackageInfo, Identifier)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log) as Console
import HTTPurple (class Generic, Request, Response, RouteDuplex', headers, jsonHeaders, mkRoute, ok', serve, string, toJson, (?))
import HTTPurple.Json.Yoga as YogaJsonEncoder
import Prelude (Unit, bind, discard, pure, show, void, (#), ($), (<#>), (<$>), (<*>), (<>), (>>=))

type Config =
  { docsFiles :: Array String
  , bowerFiles :: Array String
  , packageName :: PackageName
  , sourceFiles :: Array String
  }

data Route = Search { q :: String }

derive instance Generic Route _

corsMiddleware
  :: forall route
   . String
  -> (Request route -> Aff Response)
  -> Request route
  -> Aff Response
corsMiddleware corsUrl router request = do
  response <- router request
  let
    corsHeaders = headers
      { "Access-Control-Allow-Origin": corsUrl
      , "Access-Control-Allow-Methods": "GET, OPTIONS"
      , "Access-Control-Allow-Headers": "Content-Type, accept, origin"
      }
  pure $ response { headers = response.headers <> corsHeaders }

route :: RouteDuplex' Route
route = mkRoute
  { "Search": "search" ? { q: string }
  }

run :: Config -> Effect Unit
run cfg = launchAff_ $ do

  liftEffect do
    Console.log "Loading search index..."

  docsJsons /\ moduleNames /\ packageMetas <- sequential $
    (\d h m -> d /\ h /\ m)
      <$> parallel (IndexBuilder.decodeDocsJsons cfg)
      <*> parallel (parseModuleHeaders cfg.sourceFiles)
      <*> parallel (IndexBuilder.decodeBowerJsons cfg)

  let
    scores = mkScores packageMetas
    index = mkDeclarations scores docsJsons
    typeIndex = docsJsons >>= resultsWithTypes scores
    packageIndex = mkPackageIndex $ mkPackageInfo scores packageMetas
    moduleIndex = unpackModuleIndex $ mkPackedModuleIndex index moduleNames
    engineState = mkEngineState (unwrap index) typeIndex packageIndex moduleIndex scores
    corsUrl = "http://localhost:4040"
  let
    countOfDefinitions = Trie.size $ unwrap index
    countOfTypeDefinitions = Array.length typeIndex
    countOfPackages = Array.length packageMetas

    router { route: Search { q } } = do
      let
        results :: Array Result
        results =
          Engine.query nodeEngine engineState q <#> (_.results) # un Identity

      ok' jsonHeaders $ toJson YogaJsonEncoder.jsonEncoder $ showResult cfg <$> Array.reverse results

  liftEffect do
    Console.log $
      "Loaded "
        <> show countOfDefinitions
        <> " definitions and "
        <> show countOfTypeDefinitions
        <> " type definitions from "
        <> show countOfPackages
        <> " packages."

  liftEffect $ void do

    serve { port: 8080 } { route, router: corsMiddleware corsUrl router }

showResult :: Config -> Result -> Commons.SearchResult
showResult cfg = case _ of
  DeclResult r -> showSearchResult cfg r
  TypeResult r -> showSearchResult cfg r
  PackResult r -> showPackageResult r
  MdlResult r -> showModuleResult r

showSearchResult :: Config -> SearchResult -> Commons.SearchResult
showSearchResult cfg (SearchResult result@{ name, comments, moduleName, packageInfo }) =
  let
    typeText = showSignature result
    text =
      ( fromMaybe "" $
          comments <#> \comment ->
            "\n" <> (String.trim comment) <> "\n\n"
      )

    p = (packageInfoToString cfg.packageName packageInfo)

    m = showModuleName moduleName
  in
    { info: Commons.Declaration
        { module: m
        , title: ""
        , typeOrValue: ""
        , typeText: Just typeText
        }
    , markup: ""
    , package: p
    , text: text
    , version: ""
    }

showPackageResult :: PackageResult -> Commons.SearchResult
showPackageResult { name, description } =
  let
    packageName = ("package") <> " " <> (unwrap name) <>

      (description >#> \text -> "\n\n" <> text <> "\n")
  in
    { info:
        Commons.Package { deprecated: false }
    , markup: ""
    , package: packageName
    , text: ""
    , version: ""
    }

showModuleResult :: ModuleResult -> Commons.SearchResult
showModuleResult { name, package } =
  let
    moduleName = ("module") <> " " <> showModuleName name
  in
    { info:
        Commons.Module
          { module: moduleName
          }
    , markup: ""
    , package: ""
    , text: ""
    , version: ""
    }

showModuleName :: ModuleName -> String
showModuleName = unwrap

showSignature
  :: forall rest
   . { name :: Identifier
     , moduleName :: ModuleName
     , packageInfo :: PackageInfo
     , info :: ResultInfo
     | rest
     }
  -> String
showSignature result@{ name, info } =
  case info of
    ValueResult { type: ty } ->
      (unwrap name) <> " :: " <> showType ty

    TypeClassResult info' ->
      showTypeClassSignature info' result

    TypeClassMemberResult info' ->
      showTypeClassMemberSignature info' result

    DataResult info' ->
      showDataSignature info' result

    TypeSynonymResult info' ->
      showTypeSynonymSignature info' result

    ExternDataResult info' ->
      showExternDataSignature info' result

    ValueAliasResult ->
      ("(" <> unwrap name <> ")")

    DataConstructorResult info' ->
      showDataConstructorSignature info' result

    _ -> unwrap name

showTypeClassSignature
  :: forall rest
   . { fundeps :: FunDeps
     , arguments :: Array TypeArgument
     , superclasses :: Array Constraint
     }
  -> { name :: Identifier, moduleName :: ModuleName | rest }
  -> String
showTypeClassSignature { fundeps, arguments, superclasses } { name, moduleName } =
  "class"
    <>
      ( if Array.null superclasses then
          ""
        else
          " ("
            <>
              ( Array.intercalate (", ")
                  ( superclasses <#> showConstraint
                  )
              )
            <> ") "
            <>
              "<="
      )
    <> space
    <> (unwrap name)
    <> space
    <>
      ( Array.intercalate space $
          arguments <#> showTypeArgument
      )
    <>
      ( showFunDeps fundeps
      )

showTypeClassMemberSignature
  :: forall rest
   . { "type" :: Type
     , typeClass :: QualifiedName
     , typeClassArguments :: Array TypeArgument
     }
  -> { name :: Identifier | rest }
  -> String
showTypeClassMemberSignature { "type": ty, typeClass, typeClassArguments } result =
  (unwrap result.name)
    <> " :: "
    <>
      showType ty

showDataSignature
  :: forall rest
   . { typeArguments :: Array TypeArgument
     , dataDeclType :: DataDeclType
     }
  -> { name :: Identifier | rest }
  -> String
showDataSignature { typeArguments, dataDeclType } { name } =
  ( case dataDeclType of
      NewtypeDataDecl -> "newtype"
      DataDataDecl -> "data"
  )
    <> space
    <> (unwrap name)
    <> space
    <>
      ( Array.intercalate space $
          typeArguments <#> showTypeArgument
      )

showTypeSynonymSignature
  :: forall rest
   . { type :: Type
     , arguments :: Array TypeArgument
     }
  -> { name :: Identifier | rest }
  -> String
showTypeSynonymSignature { type: ty, arguments } { name } =
  "type"
    <> space
    <> (unwrap name)
    <> space
    <>
      ( Array.intercalate space $
          arguments <#> showTypeArgument
      )
    <> space
    <> "="
    <> space
    <>
      showType ty

showExternDataSignature
  :: forall rest
   . { kind :: Type }
  -> { name :: Identifier | rest }
  -> String
showExternDataSignature { kind } { name } =
  "foreign data"
    <> space
    <> (unwrap name)
    <> " :: "
    <>
      showType kind

showDataConstructorSignature
  :: forall rest
   . { dataDeclType :: DataDeclType
     , "type" :: Type
     }
  -> { name :: Identifier
     | rest
     }
  -> String
showDataConstructorSignature { dataDeclType, type: ctorType } { name } =
  ( case dataDeclType of
      NewtypeDataDecl -> "newtype constructor"
      DataDataDecl -> "data constructor"
  )
    <> space
    <> (unwrap name)
    <> space
    <> "::"
    <> space
    <>
      showType ctorType
