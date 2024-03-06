type 'a non_empty_list = 'a * 'a list

val one : 'a -> 'a * 'b list
val from_list : 'a list -> ('a * 'a list) option
val map : ('a -> 'b) -> 'a * 'a list -> 'b * 'b list
val filter : ('a -> bool) -> 'a * 'a list -> ('a * 'a list) option
val exists : ('a -> bool) -> 'a * 'a list -> bool
val for_all : ('a -> bool) -> 'a * 'a list -> bool
val reverse : 'a * 'a list -> 'a * 'a list

(* concat NeL's *)
val ( <+> ) : 'a * 'b list -> 'b * 'b list -> 'a * 'b list

(* append value *)
val ( <+ ) : 'a * 'b list -> 'b -> 'a * 'b list

(* prepend value *)
val ( +> ) : 'a -> 'b * 'b list -> 'a * 'b list
