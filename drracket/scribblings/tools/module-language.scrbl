#lang scribble/doc
@(require "common.rkt")
@(tools-title "module-language")

@definterface[drracket:module-language:module-language<%> ()]{

The only language that implements this interface is DrRacket's ``Use
the language declared in the source'' language.

  @defmethod[(get-users-language-name) string]{
  Returns the name of the language that is declared in the source, as a string.
  }
}

drracket:module-language-tools:definitions-text<%>
drracket:module-language-tools:definitions-text-mixin

drracket:module-language-tools:frame<%>
drracket:module-language-tools:frame-mixin

drracket:module-language-tools:tab<%>
drracket:module-language-tools:tab-mixin

@(tools-include "module-language")
