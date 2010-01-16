-- @+leo-ver=4-thin
-- @+node:gcross.20091216150502.1713:@thin Setup.hs
-- @@language Haskell

-- @<< Language extensions >>
-- @+node:gcross.20091216150502.1714:<< Language extensions >>
{-# LANGUAGE PackageImports #-}
-- @-node:gcross.20091216150502.1714:<< Language extensions >>
-- @nl

module Main where

-- @<< Import needed modules >>
-- @+node:gcross.20091216150502.1715:<< Import needed modules >>
import Control.Applicative

import System.FilePath

import Blueprint.Configuration
import Blueprint.Tools.GCC
import Blueprint.Tools.GHC.Main
import Blueprint.Tools.GFortran
-- @nonl
-- @-node:gcross.20091216150502.1715:<< Import needed modules >>
-- @nl

-- @+others
-- @+node:gcross.20091216150502.1716:Types
-- @+node:gcross.20091216150502.1717:AdditionalConfiguration
data AdditionalConfiguration = AdditionalConfiguration
    {   gccConfiguration :: GCCConfiguration
    ,   gfortranConfiguration :: GFortranConfiguration
    }
-- @-node:gcross.20091216150502.1717:AdditionalConfiguration
-- @-node:gcross.20091216150502.1716:Types
-- @+node:gcross.20091216150502.1718:Functions
-- @+node:gcross.20091216150502.1719:configureAdditional
configureAdditional =
    AdditionalConfiguration
        <$> (configureUsingSection "GCC")
        <*> (configureUsingSection "GCC")
-- @-node:gcross.20091216150502.1719:configureAdditional
-- @+node:gcross.20091216150502.1720:compileAdditional
compileAdditional _ configuration build_root digest_cache_subdirectory object_subdirectory interface_subdirectory =
    gfortranCompileAll
        (gfortranConfiguration configuration)
        digest_cache_subdirectory
        gfortran_flags
        object_subdirectory
        (interface_subdirectory </> "fortran")
    .
    gccCompileAll
        (gccConfiguration configuration)
        digest_cache_subdirectory
        gcc_flags
        object_subdirectory
-- @-node:gcross.20091216150502.1720:compileAdditional
-- @-node:gcross.20091216150502.1718:Functions
-- @+node:gcross.20091216150502.1721:Values
-- @+node:gcross.20091216150502.1722:Additional Options
additional_options =
    [   gccOptions
    ,   gfortranOptions
    ]
-- @-node:gcross.20091216150502.1722:Additional Options
-- @+node:gcross.20091216150502.1723:Flags
data FlagMode = Debug | Optimized
flag_mode = Debug

ghc_flags =
    case flag_mode of
        Debug -> []
        Optimized -> ["-O2","-fvia-C","-optc=-O3"]

gcc_flags =
    case flag_mode of
        Debug -> ["-g"]
        Optimized -> ["-O3","-ffast-math","-funroll-loops"]

gfortran_flags =
    ["-cpp","-fimplicit-none"]++gcc_flags++
    case flag_mode of
        Debug -> ["-fbounds-check"]
        Optimized -> []
-- @-node:gcross.20091216150502.1723:Flags
-- @-node:gcross.20091216150502.1721:Values
-- @+node:gcross.20091216150502.1724:main
main =
    defaultMain
        configureAdditional
        compileAdditional
        additional_options
        ("","sources")
        (Just
           (("","tests")
           ,[]
           ,["HUnit == 1.*"
            ,"test-framework == 0.2.*"
            ,"test-framework-hunit == 0.2.*"
            ,"test-framework-quickcheck2 == 0.2.*"
            ,"test-framework-antitest == 0.1.*"
            ,"test-framework-statistics == 0.1.*"
            ,"mersenne-random == 1.*"
            ,"erf == 1.*"
            ]
           )
        )
        ghc_flags
-- @-node:gcross.20091216150502.1724:main
-- @-others
-- @-node:gcross.20091216150502.1713:@thin Setup.hs
-- @-leo
