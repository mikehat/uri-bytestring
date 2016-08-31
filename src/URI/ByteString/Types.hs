{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module URI.ByteString.Types where

-------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Typeable
import           Data.Word
import           GHC.Generics
-------------------------------------------------------------------------------
import           Prelude
-------------------------------------------------------------------------------


-- | Required first component to referring to a specification for the
-- remainder of the URI's components, e.g. "http" or "https"
newtype Scheme = Scheme { schemeBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
newtype Host = Host { hostBS :: ByteString }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
-- | While some libraries have chosen to limit this to a Word16, the
-- spec only specifies that the string be comprised of digits.
newtype Port = Port { portNumber :: Int }
  deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data Authority = Authority {
      authorityUserInfo :: Maybe UserInfo
    , authorityHost     :: Host
    , authorityPort     :: Maybe Port
    } deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
data UserInfo = UserInfo {
      uiUsername :: ByteString
    , uiPassword :: ByteString
    } deriving (Show, Eq, Generic, Typeable, Ord)


-------------------------------------------------------------------------------
newtype Query = Query { queryPairs :: [(ByteString, ByteString)] }
              deriving (Show, Eq, Monoid, Generic, Typeable, Ord)


type ParsedQuery = Query
type UnparsedQuery = Maybe ByteString

-------------------------------------------------------------------------------
-- | Note: URI fragment does not include the #
data URIRefT a b where
  URI :: { uriScheme :: Scheme
         , uriAuthority :: Maybe Authority
         , uriPath :: ByteString
         , uriQuery :: a
         , uriFragment :: Maybe ByteString
         } -> URIRefT a Absolute
  RelativeRef :: { rrAuthority :: Maybe Authority
                 , rrPath :: ByteString
                 , rrQuery :: a
                 , rrFragment :: Maybe ByteString
                 } -> URIRefT a Relative

deriving instance (Show a) => Show (URIRefT a b)
deriving instance (Eq a) => Eq (URIRefT a b)
-- deriving instance Generic (URIRefT a b)
deriving instance (Ord a) => Ord (URIRefT a b)

#ifdef WITH_TYPEABLE
deriving instance Typeable URIRefT
#endif


-------------------------------------------------------------------------------
-- | A URIRefT specialized for parsed query strings
type URIRef a = URIRefT ParsedQuery a

-- | A URIRefT specialized for unparsed query strings
type URIRefUQS a = URIRefT UnparsedQuery a


-------------------------------------------------------------------------------
data Absolute deriving(Typeable)


-------------------------------------------------------------------------------
data Relative deriving(Typeable)


-------------------------------------------------------------------------------
-- | An absolute URI with a parsed query string
type URI = URIRefT ParsedQuery Absolute

-- | An absolute URI with an unparsed query string
type URIUQS = URIRefT UnparsedQuery Absolute


-------------------------------------------------------------------------------
-- | A relative URI with a parsed query string
type RelativeRef = URIRef Relative

-- | A relative URI with an unparsed query string
type RelativeRefUQS = URIRefT UnparsedQuery Relative


-------------------------------------------------------------------------------
-- | Options for the parser. You will probably want to use either
-- "strictURIParserOptions" or "laxURIParserOptions"
data URIParserOptions = URIParserOptions {
      upoValidQueryChar :: Word8 -> Bool
    }


-------------------------------------------------------------------------------
data URINormalizationOptions = URINormalizationOptions {
      unoDowncaseScheme    :: Bool
    -- ^ hTtP -> http
    , unoDowncaseHost      :: Bool
    -- ^ eXaMpLe.org -> example.org
    , unoDropDefPort       :: Bool
    -- ^ If the scheme is known and the port is the default (e.g. 80 for http) it is removed.
    , unoSlashEmptyPath    :: Bool
    -- ^ If the path is empty, set it to \/
    , unoDropExtraSlashes  :: Bool
    -- ^ Rewrite path from \/foo\/\/bar\/\/\/baz to \/foo\/bar\/baz
    , unoSortParameters    :: Bool
    -- ^ Sorts parameters by parameter name
    , unoRemoveDotSegments :: Bool
    -- ^ Remove dot segments as per <https://tools.ietf.org/html/rfc3986#section-5.2.4 RFC3986 Section 5.2.4>
    , unoDefaultPorts      :: M.Map Scheme Port
    -- ^ Map of known schemes to their default ports. Used when 'unoDropDefPort' is enabled.
    } deriving (Show, Eq)


-------------------------------------------------------------------------------
-- | URI Parser Types
-------------------------------------------------------------------------------


data SchemaError = NonAlphaLeading -- ^ Scheme must start with an alphabet character
                 | InvalidChars    -- ^ Subsequent characters in the schema were invalid
                 | MissingColon    -- ^ Schemas must be followed by a colon
                 deriving (Show, Eq, Read, Generic, Typeable)


-------------------------------------------------------------------------------
data URIParseError = MalformedScheme SchemaError
                   | MalformedUserInfo
                   | MalformedQuery
                   | MalformedFragment
                   | MalformedHost
                   | MalformedPort
                   | MalformedPath
                   | OtherError String -- ^ Catchall for unpredictable errors
                   deriving (Show, Eq, Generic, Read, Typeable)
