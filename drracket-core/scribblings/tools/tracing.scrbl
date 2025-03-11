#lang scribble/doc
@(require "common.rkt")
@(tools-title "tracing")

@defmixin[drracket:tracing:tab-mixin
          (drracket:unit:tab<%> drracket:rep:context<%>)
          ()]{
 Tracks function call tracing information.
}

@defmixin[drracket:tracing:frame-mixin
          (drracket:frame:<%> drracket:unit:frame<%>)
          ()]{
 Tracks function call tracing information.
}

@(tools-include "tracing")
