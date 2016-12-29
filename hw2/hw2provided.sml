(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (s, xs) = 
  case xs of
       []     => NONE
     | x::xs' => case same_string(s, x) of
                      true  => SOME xs'
                    | false => case all_except_option(s, xs') of
                                    NONE => NONE
                                  | SOME y => SOME (x::y)

fun get_substitutions1(xl, s) = 
  case xl of
       [] => []
     | e::xl' => case all_except_option(s, e) of
                      NONE => get_substitutions1(xl', s)
                    | SOME y => y @ get_substitutions1(xl', s)

fun get_substitutions2(xl, s) = 
  let
    fun h(xl, s, acc) = 
      case xl of
           [] => acc
         | e::xl' => case all_except_option(s, e) of
                          NONE => h(xl', s, acc)
                        | SOME y => h(xl', s, acc @ y)
  in
    h(xl, s, [])
  end

fun similar_names(ll, {first=x, middle=y, last=z}) = 
  let
    fun h(xl, acc) =
      case xl of
           [] => acc
         | e::xl' => h(xl', acc @ [{first=e, middle=y, last=z}])
  in
    h(x::get_substitutions2(ll, x), [])
  end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color(s, r) = 
  case s of 
       Spades => Black
     | Clubs => Black
     | _ => Red

fun card_value(s, r) = 
  case r of 
       Num(n) => n
     | Ace => 11
     | _ => 10

fun remove_card(cs, c, ex) = 
  case cs of
       [] => raise ex
     | x::cs' => if x=c then cs' else x::remove_card(cs', c, ex)

fun all_same_color(cs) = 
  case cs of
       [] => true
     | _::[] => true
     | e1::e2::cs' => if card_color(e1)=card_color(e2) then
       all_same_color(e2::cs') else false

fun sum_cards cs = 
  let
    fun h(l, acc) = 
      case l of 
           [] => acc
         | e::l' => h(l', card_value(e) + acc)
  in
    h(cs, 0)
  end

fun score(cs, goal) = 
  let
    val sum = sum_cards cs
    val preliminary_score = if sum > goal
                            then 3 * (sum - goal)
                            else goal - sum
  in
    if all_same_color(cs)=true 
    then preliminary_score div 2
    else preliminary_score
  end

fun officiate(cards, moves, goal) = 
  let fun loop(game_state) = 
          case game_state of 
            (_,[],goal,held) => score(held,goal)
          | ([], Draw::Moves, goal, held) => score(held,goal)
          | (card::cards, move::moves, goal, held) => 
              case move of 
                Discard crd => loop(cards,moves,goal,(remove_card(held,crd,IllegalMove)))
              | Draw => if (card_value(card)+sum_cards(held)) > goal
                        then score(card::held,goal)
                        else loop(cards,moves,goal,card::held)
  in
    loop(cards,moves,goal,[])
  end
