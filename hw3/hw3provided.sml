(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals xs = 
  List.filter (fn x => Char.isUpper(String.sub(x, 0))) xs

fun longest_string1 cs =
  foldl (fn (x,y) => 
    let 
      val l = String.size x
      val r = String.size y
    in if l > r then x else y end
    ) "" cs

fun longest_string2 cs =
  foldl (fn (x,y) => 
    let 
      val l = String.size x
      val r = String.size y
    in if l >= r then x else y end
    ) "" cs

fun longest_string_helper f cs =
  (foldl (fn (x,y) => 
    let 
      val l = String.size x
      val r = String.size y
    in if f(l, r) then x else y end
    ) "" cs)

val longest_string3 = longest_string_helper (fn (l, r) => l > r)
val longest_string4 = longest_string_helper (fn (l, r) => l >= r)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f cs = 
  case cs of 
       [] => raise NoAnswer
     | x::cs' => case f x of
                      NONE => first_answer f cs'
                    | SOME y => y

fun all_answers f cs = 
  let 
    fun h(cs, acc) = 
    case cs of
         [] => SOME acc
       | x::cs' => case f x of
                    NONE => NONE
                  | SOME y => h(cs', y @ acc)
  in
        h(cs, [])
  end

val count_wildcards = g (fn () => 1) (fn x => 0)
val count_wild_and_variable_lengths = g (fn () => 1) (fn x => String.size x)
fun count_some_var (s, p) = 
  g (fn () => 0) (fn x => if x=s then 1 else 0) p

fun check_pat p =  
let
  fun strings p acc =
    case p of
        Variable x => x::acc
      | TupleP ps => List.foldl (fn (pp,i) => strings pp i) acc ps
      | ConstructorP(_, pp) => strings pp acc
      | _ => acc
  fun hasRepeat xs = 
    case xs of 
         [] => false
       | x::xs' => if List.exists (fn j => j=x) xs'
                    then true
                    else hasRepeat xs'
in
  not (hasRepeat (strings p []))
end

fun match (v, p) = 
  case (v, p) of
       (_, Wildcard) => SOME []
     | (_, Variable s) => SOME [(s, v)]
     | (Unit, UnitP) => SOME []
     | (Const x, ConstP y) => if x=y then SOME[] else NONE
     | (Tuple(vs), TupleP(ps)) => if length vs = length ps 
                              then all_answers match (ListPair.zip(vs, ps))
                              else NONE
     | (Constructor(s2, vv), ConstructorP(s1, pp)) =>
         if s1=s2
         then match(vv,pp)
         else NONE
     | _ => NONE

fun first_match v lp = 
  SOME (first_answer (fn p => match (v,p)) lp)
  handle _ => NONE

