type ('a, 'b) validated = Valid of 'a | Invalid of 'b

val valid : 'a -> ('a, 'b) validated
val invalid : 'a -> ('b, 'a) validated
val invalid_nel : 'a -> ('b, 'a) validated
val is_valid : ('a, 'b) validated -> bool
val is_invalid : ('a, 'b) validated -> bool
val map : ('a -> 'b) -> ('a, 'c) validated -> ('b, 'c) validated
val bimap : ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) validated -> ('b, 'd) validated
val fold : ('a -> 'b) -> ('c -> 'b) -> ('a, 'c) validated -> 'b
val to_option : ('a, 'b) validated -> 'a option
val to_result : ('a, 'b) validated -> ('a, 'b) result
