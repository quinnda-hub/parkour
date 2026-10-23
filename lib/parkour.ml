module Span = Span
module Expected = Expected
module Diagnostic = Diagnostic

exception Grammar_error = Parkour_intf.Grammar_error

module type INPUT = Parkour_intf.INPUT
module type S = Parkour_intf.S
module type TEXT = Parkour_intf.TEXT

module Make = Engine.Make
include Text

module Private = struct
  module Quote = Quote
end
