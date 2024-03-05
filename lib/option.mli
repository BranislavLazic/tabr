val get_or_else : 'a -> 'a option -> 'a
val or_else : 'a option -> 'a option -> 'a option
val filter : ('a -> bool) -> 'a option -> 'a option
val flatten : 'a option option -> 'a option
val contains : 'a -> 'a option -> bool
val zip : 'a option -> 'b option -> ('a * 'b) option
val tap : ('a -> unit) -> 'a option -> 'a option
val tap_none : (unit -> unit) -> 'a option option -> 'a option
