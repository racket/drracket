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

@definterface[drracket:module-language-tools:definitions-text<%> ()]{
 @defmethod[(move-to-new-language) void?]{
  This method is called when a new language is evident in the
  definitions window (by editing the @litchar{#lang} line.
 }

 @defmethod[(get-in-module-language?) boolean?]{
  Returns @racket[#t] when the current language setting
  (from the language dialog) is ``The Racket Language''.
 }
  
}

@definterface[drracket:module-language-tools:tab<%> ()]{
  This interface signals an implementation of a tab
  that specially handles programs beginning with
  @litchar{#lang}.
}

@definterface[drracket:module-language-tools:frame<%> ()]{
  This interface signals an implementation of a frame
  that specially handles programs beginning with
  @litchar{#lang}.
}


@defmixin[drracket:module-language-tools:definitions-text-mixin
          (text:basic<%>
           racket:text<%>
           drracket:unit:definitions-text<%>)
          (drracket:module-language-tools:definitions-text<%>)]{}

@defmixin[drracket:module-language-tools:frame-mixin
          (drracket:unit:frame<%>)
          (drracket:module-language-tools:frame<%>)]{}

@defmixin[drracket:module-language-tools:tab-mixin
          (drracket:unit:tab<%>)
          (drracket:module-language-tools:tab<%>)]{}

@(tools-include "module-language")
