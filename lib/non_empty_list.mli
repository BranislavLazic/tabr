type 'a t = 'a * 'a list

val one : 'a -> 'a * 'b list
val from_list : 'a list -> ('a * 'a list) option
val map : ('a -> 'b) -> 'a * 'a list -> 'b * 'b list
val filter : ('a -> bool) -> 'a * 'a list -> ('a * 'a list) option
val find : ('a -> bool) -> 'a * 'a list -> 'a option
val exists : ('a -> bool) -> 'a * 'a list -> bool
val for_all : ('a -> bool) -> 'a * 'a list -> bool
val zip : 'a * 'a list -> 'b * 'b list -> ('a * 'b) * ('a * 'b) list
val reverse : 'a * 'a list -> 'a * 'a list
val ( <+> ) : 'a * 'b list -> 'b * 'b list -> 'a * 'b list
val ( <+ ) : 'a * 'b list -> 'b -> 'a * 'b list
val ( +> ) : 'a -> 'b * 'b list -> 'a * 'b list
val size : 'a * 'b list -> int
val to_list : 'a * 'a list -> 'a list
val to_option : 'a * 'b -> ('a * 'b) option
val to_result : 'a * 'b -> ('a * 'b, 'c) result
