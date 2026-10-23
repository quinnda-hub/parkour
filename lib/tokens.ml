(* Parsing an array of lexed tokens, with spans into the original source. *)

module Make (T : Parkour_intf.TOKEN) = struct
  module I = struct
    type t = { source : string; tokens : (T.t * Span.t) array }
    type token = T.t

    let length i = Array.length i.tokens
    let get i k = fst (Array.unsafe_get i.tokens k)
    let equal = T.equal
    let show = T.show

    let span i k =
      if k >= Array.length i.tokens then Span.point (String.length i.source)
      else snd (Array.unsafe_get i.tokens k)

    (* The source text of the tokens [a, b), or their written form if the
       source text is empty. *)
    let text i a b =
      let start = (span i a).start and stop = (span i (b - 1)).stop in
      if start < stop then Quote.range i.source start stop
      else
        Quote.literal
          (String.concat " "
             (List.init (b - a) (fun k -> T.show (get i (a + k)))))

    let unexpected i k = (text i k (k + 1), span i k)
    let describe = text
  end

  include Engine.Make (I)

  let input ~source tokens = { I.source; tokens }
end
