#lang scribble/doc
@(require "common.rkt")
@(tools-title "debug")

@defmixin[drracket:debug:profile-unit-frame-mixin
          (drracket:frame:<%> drracket:unit:frame<%>)
          ()]{
 Adds support for profiling information.

 @defmethod[(show-profile-gui) void?]{
  Shows the GUI information shown about the profile.
 }
 @defmethod[(hide-profile-gui) void?]{
  Hides the GUI information shown about the profile.
 }
 
}

@defmixin[drracket:debug:profile-tab-mixin
          (drracket:unit:tab<%>)
          ()]{
 Tracks profiling information.
}

@defmixin[drracket:debug:profile-definitions-text-mixin
          (drracket:unit:definitions-text<%> text%)
          ()]{
 Tracks profiling information.
}

@defmixin[drracket:debug:test-coverage-definitions-text-mixin
          (text% drracket:unit:definitions-text<%>)
          ()]{
 Tracks test case coverage information.
}
@defmixin[drracket:debug:test-coverage-interactions-text-mixin
          (drracket:rep:text<%> text:basic<%>)
          ()]{
 Tracks test case coverage information.
}
@defmixin[drracket:debug:test-coverage-tab-mixin
          (drracket:rep:context<%> drracket:unit:tab<%>)
          ()]{
 Tracks test case coverage information.
}


@(tools-include "debug")
