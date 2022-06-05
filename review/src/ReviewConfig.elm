module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import Docs.NoMissing
import Docs.ReviewAtDocs
import Docs.ReviewLinksAndSections
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenWords
import NoFunctionOutsideOfModules
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeExpose
import NoRecordAliasConstructor
import NoSinglePatternCase
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import OnlyAllSingleUseTypeVarsEndWith_
import Review.Rule as Rule exposing (Rule)
import ReviewPipelineStyles
import ReviewPipelineStyles.Fixes as ReviewPipelineStyles
import ReviewPipelineStyles.Predicates as ReviewPipelineStyles
import Simplify


config : List Rule
config =
    [ Docs.ReviewLinksAndSections.rule
    , Docs.ReviewAtDocs.rule
    , Docs.NoMissing.rule
        { document = Docs.NoMissing.onlyExposed
        , from = Docs.NoMissing.exposedModules
        }
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
        |> Rule.ignoreErrorsForDirectories [ "tests/" ]
    , NoExposingEverything.rule
    , NoForbiddenWords.rule [ "REPLACEME", "TODO", "todo" ]
    , NoImportingEverything.rule []
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeExpose.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , Simplify.rule Simplify.defaults
    , NoSinglePatternCase.rule
        (NoSinglePatternCase.fixInArgument
            |> NoSinglePatternCase.ifAsPatternRequired
                (NoSinglePatternCase.fixInLetInstead
                    |> NoSinglePatternCase.andIfNoLetExists
                        NoSinglePatternCase.createNewLet
                )
        )
    , [ ReviewPipelineStyles.forbid ReviewPipelineStyles.leftPizzaPipelines
            |> ReviewPipelineStyles.that ReviewPipelineStyles.haveAnUnnecessaryInputStep
            |> ReviewPipelineStyles.andTryToFixThemBy ReviewPipelineStyles.eliminatingInputStep
            |> ReviewPipelineStyles.andCallThem "<| pipeline with simple input"
      ]
        |> ReviewPipelineStyles.rule
    , NoRecordAliasConstructor.rule
    ]
