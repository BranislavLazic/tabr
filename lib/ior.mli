type ('a, 'b) t = Left of 'a | Right of 'b | Both of 'a * 'b

val right : 'a -> ('b, 'a) t
val left : 'a -> ('a, 'b) t
val both : 'a -> 'b -> ('a, 'b) t
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a -> 'c -> 'b) -> ('a, 'c) t -> 'b
val map : ('a -> 'b) -> ('c, 'a) t -> ('c, 'b) t
val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) t -> ('b, 'd) t
val left_map : ('a -> 'b) -> ('a, 'c) t -> ('b, 'c) t
val swap : ('a, 'b) t -> ('b, 'a) t
val to_option : ('a, 'b) t -> 'b option
val to_result : ('a, 'b) t -> ('b, 'a) result
val get_or_else : 'a -> ('b, 'a) t -> 'a
val is_left : ('a, 'b) t -> bool
val is_right : ('a, 'b) t -> bool
val is_both : ('a, 'b) t -> bool
