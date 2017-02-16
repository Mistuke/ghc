{-# LANGUAGE Safe #-}
{-# LANGUAGE CPP #-}

module Main(main) where

import Control.Arrow ((***))
import Control.Monad (when, forM_)
import Control.Exception (bracket)

import Data.Char (toLower, isSpace)
import Data.List (isPrefixOf, nub, sort, (\\))
import qualified Data.Map as M (Map(), alter, empty, toList)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.Directory (findFilesWith, getCurrentDirectory)
import System.FilePath (takeBaseName, takeDirectory, dropExtension, (<.>)
                       ,takeFileName)
import System.IO (hClose, hGetContents, withFile, IOMode(..), hPutStrLn)
import System.Process (proc, createProcess_, StdStream (..), CreateProcess(..)
                      ,waitForProcess)

import Foreign.C.Types (CInt(..), )
import Foreign.C.String (withCWString, peekCWString, CWString)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Alloc (alloca)

#if defined(i386_HOST_ARCH)
# define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
# define WINDOWS_CCONV ccall
#else
# error Unknown mingw32 arch
#endif

usage :: IO ()
usage = putStrLn $ unlines [ " -= Split a dll if required and perform the linking =- "
                           , ""
                           , " Usage: gen-dll <action>"
                           , ""
                           , " Where <action> is one of:"
                           , "     link     perform a real link of dll, "
                           , "              arguments: dir distdir way flags libs objs out link_cmd delay name version"
                           ]

main :: IO ()
main = do
  args <- getArgs
  if null args
     then usage
     else case (head args) of
             "link" -> let (dir:distdir:way:extra_flags:extra_libs:objs:output:
                            command:delayed:abi_name:abi_version:_) = tail args
                       in process_dll_link dir distdir way extra_flags extra_libs
                                           objs output command delayed abi_name
                                           abi_version
             _      -> usage

type Symbol = String
type Symbols = [Symbol]
data Obj
  = Obj { objName  :: String
        , objCount :: Int
        , objItems :: [(Char, Symbol)]
        }
    deriving Show
type Objs = [Obj]

-- | Create the final DLL by using the provided arguments
--   This also creates the resulting special import library.
process_dll_link :: String -- ^ dir
                 -> String -- ^ distdir
                 -> String -- ^ way
                 -> String -- ^ extra flags
                 -> String -- ^ extra libraries to link
                 -> String -- ^ object files to link
                 -> String -- ^ output filename
                 -> String -- ^ link command
                 -> String -- ^ create delay load import libs
                 -> String -- ^ SxS Name
                 -> String -- ^ SxS version
                 -> IO ()
process_dll_link _dir _distdir _way extra_flags extra_libs objs_files output
                 link_cmd delay_imp sxs_name sxs_version
  = do let base = dropExtension output
       -- We need to know how many symbols came from other static archives
       -- So take the total number of symbols and remove those we know came
       -- from the object files. Use this to lower the max amount of symbols.
       --
       -- This granularity is the best we can do without --print-map like info.
       raw_exports <- execProg "nm" ["-g", "--defined-only", objs_files]
       let objs    = collectObjs $ sort $ nub raw_exports
           num_sym = foldr (\a b -> b + objCount a) 0 objs
           exports = base <.> "lst"

       _ <- withFile exports WriteMode $ \hExports ->
             mapM_ (hPutStrLn hExports . unlines . map snd . objItems) objs

       -- Side-by-Side assembly generation flags for GHC. Pass these along so the DLLs
       -- get the proper manifests generated.
       let sxs_opts = [ "-fgen-sxs-assembly"
                      , "-dylib-abi-name"
                      , show sxs_name
                      , "-dylib-abi-version"
                      , show sxs_version
                      ]

       putStrLn $ "Number of symbols in object files for " ++ output ++ ": " ++ show num_sym

       -- Now check that the DLL doesn't have too many symbols. See trac #5987.
       case num_sym > dll_max_symbols of
         False -> do putStrLn $ "DLL " ++ output ++ " OK, no need to split."
                     let defFile    = base <.> "def"
                         dll_import = base <.> "dll.a"

                     build_import_lib base (takeFileName output) defFile objs

                     _ <- execProg link_cmd $ concat [[objs_files
                                                      ,extra_libs
                                                      ,extra_flags
                                                      ]
                                                     ,sxs_opts
                                                     ,["-fno-shared-implib"
                                                      ,"-optl-Wl,--retain-symbols-file=" ++ exports
                                                      ,"-o"
                                                      ,output
                                                      ]
                                                     ]

                     build_delay_import_lib defFile dll_import delay_imp

         True -> do putStrLn $ "Too many symbols for a single DLL " ++ output
                    putStrLn "We'll have to split the dll..."
                    putStrLn $  "OK, we only have space for "
                             ++ show dll_max_symbols
                             ++ " symbols from object files when building "
                             ++ output

                    -- First split the dlls up by whole object files
                    -- To do this, we iterate over all object file and
                    -- generate a the partitions based on allowing a
                    -- maximum of $DLL_MAX_SYMBOLS in one DLL.
                    let spl_objs   = groupObjs objs
                        n_spl_objs = length spl_objs
                        base'      = base ++ "-pt"

                    mapM_ (\(n, _) -> putStrLn $ ">> DLL split at " ++ show n ++ " symbols.") spl_objs
                    putStrLn $ "OK, based on the amount of symbols we'll split the DLL into " ++ show n_spl_objs ++ " pieces."

                    -- Start off by creating the import libraries to break the
                    -- mutual dependency chain.
                    forM_ (zip [(1::Int)..] spl_objs) $ \(i, (n, o)) ->
                      do putStrLn $ "Processing file " ++ show i   ++ " of "
                                 ++ show n_spl_objs    ++ " with " ++ show n
                                 ++ " symbols."
                         let base_pt = base' ++ show i
                             file    = base_pt <.> ".def"
                             dll     = base_pt <.> ".dll"
                             lst     = base_pt <.> ".lst"

                         _ <- withFile lst WriteMode $ \hExports ->
                               mapM_ (hPutStrLn hExports . unlines . map snd . objItems) o

                         build_import_lib base_pt (takeFileName dll) file o

                    -- Now create the actual DLLs by using the import libraries
                    -- to break the mutual recursion.
                    forM_ (zip [1..] spl_objs) $ \(i, (n, _)) ->
                      do putStrLn $ "Creating DLL " ++ show i   ++ " of "
                                 ++ show n_spl_objs    ++ " with " ++ show n
                                 ++ " symbols."
                         let base_pt = base' ++ show i
                             file    = base_pt <.> ".def"
                             dll     = base_pt <.> ".dll"
                             lst     = base_pt <.> ".lst"
                             imp_lib = base_pt <.> ".dll.a"
                             indexes = [1..(length spl_objs)]\\[i]
                             libs    = map (\ix -> (base' ++ show ix) <.> "dll.a") indexes

                         _ <- execProg link_cmd $ concat [[objs_files
                                                          ,extra_libs
                                                          ,extra_flags
                                                          ,file
                                                          ]
                                                         ,libs
                                                         ,sxs_opts
                                                         ,["-fno-shared-implib"
                                                          ,"-optl-Wl,--retain-symbols-file=" ++ lst
                                                          ,"-o"
                                                          ,dll
                                                          ]
                                                         ]

                         build_delay_import_lib file imp_lib delay_imp
                         putStrLn $ "Created " ++ dll ++ "."

                    -- And finally, merge the individual import libraries into
                    -- one with the name of the original library we were
                    -- supposed to make. This means that nothing has to really
                    -- know how we split up the DLLs, for everything else it'so
                    -- as if it's still one large assembly.
                    create_merged_archive base base' (length spl_objs)


collectObjs :: [String] -> Objs
collectObjs = map snd . M.toList . foldr collectObjs' M.empty
collectObjs' :: String -> M.Map String Obj -> M.Map String Obj
collectObjs' []  m   = m
collectObjs' str_in m
  = let clean        = dropWhile isSpace
        str          = clean str_in
        (file, rest) = ((takeWhile (/=':') . clean) *** clean) $
                         break isSpace str
        (typ , sym ) = (id *** clean) $ break isSpace rest
        obj          = Obj { objName  = file
                           , objCount = 1
                           , objItems = [(head typ, sym)]
                           }
        upd value
          = if length typ /= 1
               then value
               else Just $ maybe obj
                                 (\o -> o { objCount = objCount o + 1
                                          , objItems = (head typ, sym) : objItems o
                                          })
                                 value
    in M.alter upd file m

-- Split a list of objects into globals and functions
splitObjs :: Objs -> (Symbols, Symbols)
splitObjs []     = ([], [])
splitObjs (y:ys) = group_ (objItems y) (splitObjs ys)
  where globals = "DdGgrRSs"
        group_ :: [(Char, Symbol)] -> (Symbols, Symbols) -> (Symbols, Symbols)
        group_ []     x                             = x
        group_ (x:xs) (g, f) | fst x `elem` globals = group_ xs (snd x:g, f)
                             |     otherwise        = group_ xs (g, snd x:f)

-- Determine how to split the objects up.
groupObjs :: Objs -> [(Int, Objs)]
groupObjs = binObjs 0 []
 where binObjs :: Int -> Objs -> Objs -> [(Int, Objs)]
       binObjs n l []     = [(n, l)]
       binObjs n l (o:os)
         = let nx = objCount o
               n' = n + nx
           in if n' > dll_max_symbols
                 then (n, l) : binObjs 0 [] os
                 else binObjs n' (o:l) os

-- Maximum number of symbols to allow into
-- one DLL. This is the split factor used.
dll_max_symbols :: Int
dll_max_symbols = 65535

isTrue :: String -> Bool
isTrue = (=="yes") . map toLower

foreign import WINDOWS_CCONV unsafe "Shellapi.h CommandLineToArgvW"
     c_CommandLineToArgvW :: CWString -> Ptr CInt -> IO (Ptr CWString)

foreign import WINDOWS_CCONV unsafe "windows.h LocalFree"
    localFree :: Ptr a -> IO (Ptr a)

mkArgs :: String -> IO [String]
mkArgs []  = return []
mkArgs arg =
  do withCWString arg $ \c_arg -> do
       alloca $ \c_size -> do
         res <- c_CommandLineToArgvW c_arg c_size
         size <- peek c_size
         args <- peekArray (fromIntegral size) res
         _ <- localFree res
         mapM peekCWString args

execProg :: String -> [String] -> IO [String]
execProg prog args =
  do args' <- fmap concat $ mapM mkArgs args
     prog' <- mkArgs prog
     let full@(c_prog:c_args) = prog' ++ args'
     -- print the commands we're executing for debugging and transparency
     putStrLn $ unwords full
     cwdir <- getCurrentDirectory
     let cp = (proc c_prog c_args)
              { std_out = CreatePipe, cwd = Just cwdir }
     bracket
       (createProcess_ ("execProg: " ++ prog)  cp)
       (\(_, Just hout, _, ph) -> do
         hClose hout
         code <- waitForProcess ph
         case code of
           ExitFailure _ -> exitWith code
           ExitSuccess   -> return ())
       (\(_, Just hout, _, _) -> do
         results <- hGetContents hout
         length results `seq` return $ lines results)

-- Builds a delay import lib at the very end which is used to
-- be able to delay the picking of a DLL on Windows.
-- This function is called always and decided internally
-- what to do.
build_delay_import_lib :: String -- ^ input def file
                       -> String -- ^ ouput import delayed import lib
                       -> String -- ^ flag to indicate if delay import
                                 --   lib should be created
                       -> IO ()
build_delay_import_lib input_def output_lib create_delayed
  = when (isTrue create_delayed) $
       execProg "dlltool" ["-d", input_def, "-y", output_lib] >> return ()

-- Build a normal import library from the object file definitions
build_import_lib :: FilePath -> FilePath -> FilePath -> Objs -> IO ()
build_import_lib base dll_name defFile objs
  = do -- Create a def file hiding symbols not in original object files
       -- because --export-all is re-exporting things from static libs
       -- we need to separate out data from functions. So first create two temporaries
       let (globals, functions) = splitObjs objs

       -- This split is important because for DATA entries the compiler should not generate
       -- a trampoline since CONTS DATA is directly referenced and not executed. This is not very
       -- important for mingw-w64 which would generate both the trampoline and direct referecne
       -- by default, but for libtool is it and even for mingw-w64 we can trim the output.
       _ <- withFile defFile WriteMode $ \hDef -> do
              hPutStrLn hDef $ unlines $ ["LIBRARY " ++ show dll_name
                                         ,"EXPORTS"
                                         ]
              mapM_ (\v -> hPutStrLn hDef $ "    " ++ show v ++ " DATA") globals
              mapM_ (\v -> hPutStrLn hDef $ "    " ++ show v           ) functions

       let dll_import = base <.> "dll.a"
       _ <- execProg "dlltool" ["-d", defFile, "-l", dll_import]
       return ()

-- Do some cleanup and create merged lib.
-- Because we have no split the DLL we need
-- to provide a way for the linker to know about the split
-- DLL. Also the compile was supposed to produce a DLL
-- foo.dll and import library foo.dll.a. However we've actually
-- produced foo-pt1.dll, foo-pt2.dll etc. What we don't want is to have
-- To somehow convey back to the compiler that we split the DLL in x pieces
-- as this would require a lot of changes.
--
-- Instead we produce a merged import library which contains the union of
-- all the import libraries produced. This works because import libraries contain
-- only .idata section which point to the right dlls. So LD will do the right thing.
-- And this means we don't have to do any special handling for the rest of the pipeline.
create_merged_archive :: FilePath -> String -> Int -> IO ()
create_merged_archive base prefix count
  = do let ar_script = base <.> "mri"
           imp_lib   = base <.> "dll.a"
           imp_libs  = map (\i -> prefix ++ show i) [1..count]
       let script = [ "create " ++ imp_lib    ] ++
                    map ("addlib " ++) imp_libs ++
                    [ "save", "end" ]
       writeFile ar_script (unlines script)
       _ <- execProg "ar" ["-M", ar_script]
       return ()
