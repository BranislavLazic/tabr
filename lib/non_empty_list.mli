type 'a non_empty_list = 'a * 'a list

val one : 'a -> 'a * 'b list
val from_list : 'a list -> ('a * 'a list) option
val map : ('a -> 'b) -> 'a * 'a list -> 'b * 'b list
val filter : ('a -> bool) -> 'a * 'a list -> ('a * 'a list) option
val exists : ('a -> bool) -> 'a * 'a list -> bool
val for_all : ('a -> bool) -> 'a * 'a list -> bool
val ( <+> ) : 'a * 'b list -> 'b * 'b list -> 'a * 'b list
val concat : 'a * 'b list -> 'b * 'b list -> 'a * 'b list
