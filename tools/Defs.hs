{-# LANGUAGE NoMonomorphismRestriction #-}
module Defs where
import Data.Char (chr, ord, toUpper)
import Data.List (intercalate)
--import Data.Map.Strict (Map)
import Text.Printf (printf)
--import qualified Data.Map.Strict as Map

lookupBy :: Eq a => (b -> a) -> a -> [b] -> Maybe b
lookupBy f x = (lookup x .) . fmap $ \ y -> (f y, y)

-- | Represents a C type.
data Type
  = TVoid
  | TPtr Type
  | TSize
  | TFloat
  | TDouble
  | TComplex Type
  | TEnum String
  | TDummy Int                    -- ^ Used for extracting the bound variables
  deriving (Eq, Show)

realTypes      = [ float, double ]
complexTypes   = fmap complex realTypes
floatingTypes  = realTypes ++ complexTypes
toRealType t   = case t of
  TComplex t' -> t'
  _           -> t
floatingTypesB = do
  t <- floatingTypes
  return $ case t of
    TComplex t' -> (t', t)
    _           -> (t,  t)
floatingTypesE = do
  t <- floatingTypes
  case t of
    TComplex t' -> [(t, t), (t, t')]
    _           -> return (t, t)

-- | Represents a C function.
data Fun
  = Fun
    { fName   :: String
    , fTypes  :: [Type]
    , fDoc    :: String
    }

-- | Construct a 'Fun'.
fun :: String -> [Type] -> Fun
fun name types = Fun name types ""

-- | Modify the 'fName' in a 'Fun'.
modifyFName :: (String -> String) -> Fun -> Fun
modifyFName f t = t { fName = f $ fName t }

-- | Represents a marshallable C type for c2hs.
data HType
  = HType
    { hInMarshaller  :: String
    , hType          :: String
    , hOutMarshaller :: String
    } deriving Show

mkParamType :: HType -> String
mkParamType (HType m s _) =
  if null m then s' else m ++ " " ++ s'
  where s' = "`" ++ s ++ "'"

mkRetType :: HType -> String
mkRetType (HType _ s m) =
  if null m then s' else s' ++ " " ++ m
  where s' = "`" ++ s ++ "'"

-- | Represents a C function hook for c2hs.
data CFun
  = CFun
    { cfSafe   :: Bool
    , cfName   :: String
    , cfParams :: [Type]
    , cfRet    :: Type
    , cfDoc    :: String
    }

  -- | Construct a 'CFun'.
cFun :: String -> [Type] -> Type -> CFun
cFun name params ret = CFun True name params ret ""

unreturnable t = case t of
  TComplex TFloat  -> True
  TComplex TDouble -> True
  _                -> False

substitute s y = case y of
  []     -> []
  x : xs ->
    let xs' = substitute s xs in
    case x of
      '?' -> s ++ xs'
      _   -> x : xs'

typeAbbrev t = case t of
  TFloat           -> "s"
  TDouble          -> "d"
  TComplex TFloat  -> "c"
  TComplex TDouble -> "z"

decorate [a]    = substitute $ typeAbbrev a
decorate [a, b] | a == b    = substitute $ typeAbbrev a
                | otherwise = substitute $ typeAbbrev a ++ typeAbbrev b

-- note: here we assume that both the C and Haskell types have identical
-- representations; this isn't in the specs but in practice the Storable
-- instances are identical so it should work fine
convType t = case t of
  TVoid            -> simple "()"
  TSize            -> simple "Int"      -- c2hs doesn't understand "size_t"
  TEnum t'         -> enum t'
  TFloat           -> floating "Float"
  TDouble          -> floating "Double"
  TComplex TFloat  -> complex "Complex Float"
  TComplex TDouble -> complex "Complex Double"
  TPtr t'          -> pointer $ case t' of
    TFloat           -> "Float"
    TDouble          -> "Double"
    TComplex TFloat  -> "(Complex Float)"
    TComplex TDouble -> "(Complex Double)"
  _                -> error $ "unmarshallable type: " ++ show t
  where floating s = HType ("C" ++ s) s ("fromC" ++ s)
        complex  s = HType "withVoidPtr*" s ""
        pointer  s = HType "castPtr" ("Ptr " ++ s) "castPtr"
        enum     s = HType "toCEnum" s "fromCEnum"
        simple   s = HType "" s ""

parensIf b s = if b then "(" ++ s ++ ")" else s

prettyType dummyNames = pretty 0
  where pretty p t = case t of
          TVoid       -> "()"
          TSize       -> "Int"
          TEnum t'    -> t'
          TFloat      -> "Float"
          TDouble     -> "Double"
          TComplex t' -> parensIf (p >= 10) $ "Complex " ++ pretty 10 t'
          TPtr t'     -> parensIf (p >= 10) $ "Ptr "     ++ pretty 10 t'
          TDummy n    -> dummyNames !! n

genName n = chr (ord 'a' + r) : if q == 0 then [] else show q
  where (q, r) = quotRem n 26

-- shortcuts
void      = TVoid
ptr       = TPtr
int       = TSize                       -- the ints in blas are actually sizes
float     = TFloat
double    = TDouble
complex   = TComplex
index     = TSize
order     = TEnum "Order"
transpose = TEnum "Transpose"
uplo      = TEnum "Uplo"
diag      = TEnum "Diag"
side      = TEnum "Side"

funInstsUnsafe = fmap (\ x -> x { cfSafe = False }) funInsts

funInsts = fmap mangleFun $ concatFunInstances funs

-- | Blas function signatures.
funs =
  [ gp $ fun "sdsdot" [ int, float, ptr float, int, ptr float, int, float ]
  , gp $ fun  "dsdot" [ int, ptr float, int, ptr float, int, double ]
  , gpR $ \ a   -> fun "?dot"   [ int, ptr a, int, ptr a, int, a ]
  , gpC $ \ a   -> fun "?dotu"  [ int, ptr a, int, ptr a, int, a ]
  , gpC $ \ a   -> fun "?dotc"  [ int, ptr a, int, ptr a, int, a ]
  , gpB $ \ a b -> fun "?nrm2"  [ int, ptr b, int, a ]
  , gpB $ \ a b -> fun "?asum"  [ int, ptr b, int, a ]
  , gpA $ \ a   -> fun "i?amax" [ int, ptr a, int, index ]
  , gpA $ \ a   -> fun "?swap"  [ int, ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?copy"  [ int, ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?axpy"  [ int, a, ptr a, int, ptr a, int, void ]
  , gpR $ \ a   -> fun "?rotg"  [ ptr a, ptr a, ptr a, ptr a, void ]
  , gpR $ \ a   -> fun "?rotmg" [ ptr a, ptr a, ptr a, a, ptr a, void ]
  , gpR $ \ a   -> fun "?rot"   [ int, ptr a, int, ptr a, int, a, a, void ]
  , gpR $ \ a   -> fun "?rotm"  [ int, ptr a, int, ptr a, int, ptr a, void ]
  , gpE $ \ a b -> fun "?scal"  [ int, b, ptr a, int, void ]
  , gpA $ \ a   -> fun "?gemv"  [ order, transpose, int, int, a, ptr a
                                , int, ptr a, int, a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?gbmv"  [ order, transpose, int, int, int, int, a
                                , ptr a, int, ptr a, int, a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?trmv"  [ order, uplo, transpose, diag, int
                                , ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?tbmv"  [ order, uplo, transpose, diag, int, int
                                , ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?tpmv"  [ order, uplo, transpose, diag, int
                                , ptr a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?trsv"  [ order, uplo, transpose, diag, int
                                , ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?tbsv"  [ order, uplo, transpose, diag, int, int
                                , ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?tpsv"  [ order, uplo, transpose, diag, int
                                , ptr a, ptr a, int, void ]
  , gpR $ \ a   -> fun "?symv"  [ order, uplo, int, a, ptr a, int, ptr a, int
                                , a, ptr a, int, void ]
  , gpR $ \ a   -> fun "?sbmv"  [ order, uplo, int, int, a, ptr a, int, ptr a
                                , int, a, ptr a, int, void ]
  , gpR $ \ a   -> fun "?spmv"  [ order, uplo, int, a, ptr a, ptr a, int, a
                                , ptr a, int, void ]
  , gpR $ \ a   -> fun "?ger"   [ order, int, int, a, ptr a, int, ptr a
                                , int, ptr a, int, void ]
  , gpR $ \ a   -> fun "?syr"   [ order, uplo, int, a, ptr a, int, ptr a
                                , int, void ]
  , gpR $ \ a   -> fun "?syr2"  [ order, uplo, int, a, ptr a, int, ptr a
                                , int, ptr a, int, void ]
  , gpR $ \ a   -> fun "?spr"   [ order, uplo, int, a, ptr a
                                , int, ptr a, void ]
  , gpR $ \ a   -> fun "?spr2"  [ order, uplo, int, a, ptr a, int, ptr a
                                , int, ptr a, void ]
  , gpC $ \ a   -> fun "?hemv"  [ order, uplo, int, a, ptr a, int, ptr a
                                , int, a, ptr a, int, void ]
  , gpC $ \ a   -> fun "?hbmv"  [ order, uplo, int, int, a, ptr a
                                , int, ptr a, int, a, ptr a, int, void ]
  , gpC $ \ a   -> fun "?hpmv"  [ order, uplo, int, a, ptr a, ptr a
                                , int, a, ptr a, int, void ]
  , gpC $ \ a   -> fun "?geru"  [ order, int, int, a, ptr a, int
                                , ptr a , int, ptr a, int, void ]
  , gpC $ \ a   -> fun "?gerc"  [ order, int, int, a, ptr a, int
                                , ptr a, int, ptr a, int, void ]
  , gpQ $ \ a   -> fun "?her"   [ order, uplo, int, a, ptr (complex a), int
                                , ptr (complex a), int, void ]
  , gpQ $ \ a   -> fun "?hpr"   [ order, uplo, int, a, ptr (complex a), int
                                , ptr (complex a), void ]
  , gpC $ \ a   -> fun "?her2"  [ order, uplo, int, a, ptr a, int
                                , ptr a, int, ptr a, int, void ]
  , gpC $ \ a   -> fun "?hpr2"  [ order, uplo, int, a, ptr a, int
                                , ptr a, int, ptr a, void ]
  , gpA $ \ a   -> fun "?gemm"  [ order, transpose, transpose, int, int, int, a
                                , ptr a, int, ptr a, int, a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?symm"  [ order, side, uplo, int, int, a, ptr a, int
                                , ptr a, int, a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?syrk"  [ order, uplo, transpose, int, int, a, ptr a
                                , int, a, ptr a, int, void ]
  , gpA $ \ a   -> fun "?syr2k" [ order, uplo, transpose, int, int, a, ptr a
                                , int, ptr a, int, a, ptr a, int, void ]
  , gpC $ \ a   -> fun "?hemm"  [ order, side, uplo, int, int, a, ptr a, int
                                , ptr a, int, a, ptr a, int, void ]
  , gpQ $ \ a   -> fun "?herk"  [ order, uplo, transpose, int, int, a
                                , ptr (complex a), int, a
                                , ptr (complex a), int, void ]
  , gpQ $ \ a   -> fun "?her2k" [ order, uplo, transpose, int, int, complex a
                                , ptr (complex a), int, ptr (complex a)
                                , int, a, ptr (complex a), int, void ]
  , gpA $ \ a   -> fun "?trmm"  [ order, side, uplo, transpose, diag, int, int
                                , a, ptr a, int, ptr a, int, void ]
  , gpA $ \ a   -> fun "?trsm"  [ order, side, uplo, transpose, diag, int, int
                                , a, ptr a, int, ptr a, int, void ]
  ]

data FunGroup
  = FunGroup
    { gpName  :: String
    , gpType  :: [Type]
    , gpInsts :: [FunInstance]
    }

gp f = FunGroup (fName f) (fTypes f) [FunInstance [] f]

gp1 = makeFunGroup1 (const id) []

-- | Function group over @s d c z@.
gpA = makeFunGroup1 decorate floatingTypes

-- | Function group over @s d@.
gpR = makeFunGroup1 decorate realTypes

-- | Function group over @s d@ but relabel them as @c z@.
gpQ = makeFunGroup1 (decorate . fmap complex) realTypes

-- | Function group over @c z@.
gpC = makeFunGroup1 decorate complexTypes

-- | Function group over @ss dd sc dz@.
gpB = makeFunGroup2 decorate floatingTypesB

-- | Function group over @ss dd cc zz cs zd@.
gpE = makeFunGroup2 decorate floatingTypesE

makeFunGroup1 d ts ff = makeFunGroup 1 d ts' ff'
  where ts'        = flip fmap ts $ \ a      -> [a]
        ff' [a]    = ff a
makeFunGroup2 d ts ff = makeFunGroup 2 d ts' ff'
  where ts'        = flip fmap ts $ \ (a, b) -> [a, b]
        ff' [a, b] = ff a b

makeFunGroup n decorator ts ff =
  let f = ff . take n $ fmap TDummy [0 ..] in
  FunGroup (substitute "" $ fName f) (fTypes f) $ do
    t <- ts
    let f' = ff t
    return $ FunInstance t (f' { fName = decorator t $ fName f'})

data FunInstance
  = FunInstance
    { fiArgs :: [Type]
    , fiFun  :: Fun
    }

concatFunInstances = (>>= (>>= return . fiFun) . gpInsts)

funMangler = ("cblas_" ++)

decorateUnsafeFuns CFun { cfSafe = safe, cfName = name } =
  if safe then name else name ++ "_unsafe"

mangleFun :: Fun -> CFun
mangleFun (Fun name types doc) = (\ x -> x { cfDoc = doc } ) $
  if unreturnable ret -- some types must be returned via an out-parameter
  then cFun (name ++ "_sub") (params ++ [ptr ret]) void
  else cFun name params ret
  where params = init types
        ret    = last types

enumMangler name = "CBLAS_" ++ fmap toUpper name
enumValuePrefix  = "Cblas"
enumNames =
  [ "Order"
  , "Transpose"
  , "Uplo"
  , "Diag"
  , "Side"
  ]

links =
  [ link "her2k" "syr2k"
  , link "herk"  "syrk"
  , link "hemm"  "symm"
  , link "geru"  "ger"
  , link "gerc"  "ger"
  , link "her"   "syr"
  , link "hpr"   "spr"
  , link "her2"  "syr2"
  , link "hpr2"  "spr2"
  , link "hemv"  "symv"
  , link "hbmv"  "sbmv"
  , link "hpmv"  "spmv"
  , link "dotu"  "dot"
  , link "dotc"  "dot"
  ] where link = (,)

mergedGroups = do
  g <- funs
  let name = gpName g
  if name `elem` fmap snd links
    then []
    else case lookup name links of
      Nothing -> return g
      Just name' ->
        let Just g' = lookupBy gpName name' funs in
        return $ g { gpInsts = gpInsts g ++ gpInsts g' }

data Class
  = Class
    { csName       :: String
    , csParamNames :: [String]
    , csDefs       :: [String]
    , csDoc        :: String
    }

class1 =
  let paramNames = ["a"] in
  (\ defs -> Class "Numeric" paramNames defs "") $ do
    f <- mergedGroups
    let addIO ts      = init ts ++ ["IO " ++ parensIf (elem ' ' r) r]
          where r = last ts
        typeSignature = intercalate " -> " . addIO .
                        fmap (prettyType paramNames) $ gpType f
    if length (fiArgs . head $ gpInsts f) == 1
      then return $ printf "%s :: %s" (gpName f) typeSignature
      else []
