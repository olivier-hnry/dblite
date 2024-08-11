open Base
open Stdlib

type word = Symbol of char
  | Symbol_multi of string
  | Big_operator of char (* for ' + ' to implement significant whitespace *)
  | Close of char
  | Word of string
  | Litteral_string of string
  | Enclosed of char * (word list) (* code enclosed in () or [] or {} *)

type statement = Line_s of int*(word list) (* number of !, content *)
  | If_s of (word list * (statement list)) list (* if (condition, program), elif (condition, program), ... *)

let is_symbol l = List.mem l ['+'; '-'; '*'; '/'; '='; '<'; '>'; '!'; '%'; ','; ';']
let is_symbol_multi s = List.mem s ["+="; "-="; "*="; "/="; "%="; "=="; "<="; ">="; "=>"]
let is_operator l = List.mem l ['+'; '-'; '*'; '/'; '%']
let is_assignement w = List.mem w (List.map (function x -> Some x) [Symbol '='; Symbol_multi "+="; Symbol_multi "-="; Symbol_multi "*="; Symbol_multi "/="; Symbol_multi "%="])
let matching_open l = match l with
  | ')' -> '('
  | ']' -> '['
  | '}' -> '{'
  | _ -> failwith "Not a closing symbol."
let precedence = [
  [Word "or"];
  [Word "and"];
  [Symbol '='; Symbol_multi "=="; Symbol_multi "<="; Symbol_multi ">="; Symbol '<'; Symbol '>'];
  [Big_operator '+'; Big_operator '-'];
  [Big_operator '*'; Big_operator '/'];
  [Big_operator '%'];
  [Symbol '+'; Symbol '-'];
  [Symbol '*'; Symbol '/'];
  [Symbol '%']
]
let constructor_of_binary_operator s x y = match s with
  | Word "or" -> Or(x, y)
  | Word "and" -> And(x, y)
  | Symbol_multi "==" -> Eq(x, y)
  | Symbol_multi "<=" -> Leq(x, y)
  | Symbol_multi ">=" -> Leq(y, x)
  | Symbol '=' -> Eq_loose(x, y)
  | Symbol '<' -> Le(x, y)
  | Symbol '>' -> Le(y, x)
  | Big_operator '+' | Symbol '+' -> Add(x, y)
  | Big_operator '-' | Symbol '-' -> Sub(x, y)
  | Big_operator '*' | Symbol '*' -> Mult(x, y)
  | Big_operator '/' | Symbol '/' -> Div(x, y)
  | Big_operator '%' | Symbol '%' -> Mod(x, y)
  | _ -> failwith "Not an operator."
  
let close_paren open_char res_li = let rec aux li to_enclose = match li with
    | Close a :: t -> if a=open_char then Enclosed(open_char, (List.rev to_enclose)) :: t
      else failwith "(/[/{ closed by wrong symbol."
    | x :: t -> aux t (x::to_enclose)
    | _ -> failwith "Unmatched (/[/{."
  in aux res_li []

let is_function_declarator w = let rec aux k l = if k = String.length w then true
    else if l=8 then false
    else if w.[k]="function".[l] then aux (k+1) (l+1)
    else aux k (l+1)
  in aux 0 0

let fill_string li = let len = List.length li
  in let res = Bytes.create len
  in let rec aux k l = match l with
    | [] -> res
    | c :: t -> Bytes.set res k c; aux (k+1) t
  in Bytes.to_string (aux 0 li)

let read prog_s = let rec aux cur word_li in_string res_li = let new_res = match word_li with
    | [] -> res_li
    | _ -> Word (fill_string word_li) :: res_li
  in match cur, word_li, in_string with
    | (-1), _, false -> new_res
    | (-1), _, true -> failwith "Unmatched \"."
    | _, _, true -> begin match prog_s.[cur] with
        | '\'' when cur=0 || prog_s.[cur-1]<>'\\' || cur=1 || prog_s.[cur-2]='\\' -> aux (cur-1) [] false (Litteral_string(Scanf.unescaped(fill_string word_li)) :: res_li)
        | '"' -> aux (cur-1) ('\\'::'"'::word_li) true res_li
        | l -> aux (cur-1) (l::word_li) true res_li
      end
    | _ -> begin match prog_s.[cur] with
        | '\'' -> aux (cur-1) [] true new_res
        | '(' | '[' | '{' as open_char -> aux (cur-1) [] false (close_paren open_char new_res)
        | ')' | ']' | '}' as close_char -> aux (cur-1) [] false (Close(matching_open close_char) :: new_res)
        | ' ' when cur>1 && prog_s.[cur-2]=' ' && is_operator prog_s.[cur-1] -> aux (cur-3) [] false (Big_operator prog_s.[cur-1] :: new_res)
        | ' ' | '\n' -> aux (cur-1) [] false new_res
        | _ when cur>0 && is_symbol_multi (String.sub prog_s (cur-1) 2) -> aux (cur-2) [] false (Symbol_multi (String.sub prog_s (cur-1) 2) :: new_res)
        | l when is_symbol l -> aux (cur-1) [] false (Symbol l :: new_res)
        | l -> aux (cur-1) (l::word_li) false res_li
      end
  in aux (String.length prog_s - 1) [] false []

let rec read_fun_args args = match args with
  | [] -> []
  | [Word arg_name] -> [arg_name]
  | Word arg_name :: Symbol ',' :: t when t <> [] -> arg_name :: read_fun_args t
  | _ -> failwith "Invalid syntax for function declaration : expected comma-separated list of arguments."

let rec split_statement prog_t = let rec aux prog res_li current_line fun_li = match prog, res_li, current_line with
    | [], _, [] -> res_li, fun_li
    | [], _, _ -> failwith "Unfinished line."
    | Symbol '!' :: t, Line_s(n, x) :: res_t, [] -> aux t (Line_s(n+1, x) :: res_t) [] fun_li
    | Symbol '!' :: t, _, _ -> aux t (Line_s(1, List.rev current_line) :: res_li) [] fun_li
    | Word "if" :: Enclosed('(', cdt) :: Enclosed('{', prog) :: t, _, [] ->
      let progf, _ = split_statement prog in aux t (If_s [cdt, progf] :: res_li) [] fun_li
    | Word "elif" :: Enclosed('(', cdt) :: Enclosed('{', prog) :: t, If_s li :: res_t, [] ->
      let progf, _ = split_statement prog in aux t (If_s (List.rev ((cdt, progf) :: List.rev li)) :: res_t) [] fun_li
    | Word "else" :: Enclosed('{', prog) :: t, If_s li :: res_t, [] ->
      let progf, _ = split_statement prog in aux t (If_s (List.rev (([Word "true"], progf) :: List.rev li)) :: res_t) [] fun_li
    | Word "if" :: _, _, _ | Word "elif" :: _, _, _ | Word "else" :: _, _, _ -> failwith "Syntax error near if/elif/else."
    | Word fun_declarator :: Word fun_name :: Enclosed('(', args) :: Symbol_multi "=>" :: Enclosed('{', definition) :: t, _, _ 
      -> if current_line=[] then
        if is_function_declarator fun_declarator then
          let def, _ = split_statement definition and arg_li = read_fun_args args
          in aux t res_li [] ((fun_name, arg_li, def) :: fun_li)
        else failwith ("Invalid function keyword : "^fun_declarator^".")
      else failwith ("Expected ! before function definition.")
    | x :: t, _, _ -> aux t res_li (x :: current_line) fun_li
  in let prog, fun_li = aux prog_t [] [] [] in List.rev prog, fun_li

let split_comma prog = let rec aux li res_li = match li, res_li with
    | Symbol ',' :: _, [] -> failwith "Expected expression before ,."
    | Symbol ',' :: t, _ -> List.rev res_li :: aux t []
    | x :: t, _ -> aux t (x::res_li)
    | [], [] when prog = [] -> []
    | [], [] -> failwith "Expected expression after ,."
    | [], _ -> [List.rev res_li]
  in aux prog []

let split_on splitters prog = let rec aux li res1 = match li with
    | [] -> [], Word "", []
    | w :: t when List.mem w splitters -> List.rev t, w, res1
    | x :: t -> aux t (x::res1)
  in aux (List.rev prog) []

let split_on_precedence prog = let rec aux li = match li with
  | [] -> [], Word "", []
  | splitters :: t -> begin match split_on splitters prog with
      | _, Word "", _ -> aux t
      | res -> res
    end
  in aux precedence

let rec num_of_var var_li var_name = match var_li with
  | [] -> None
  | a :: t when a=var_name -> Some (List.length t)
  | _ :: t -> num_of_var t var_name

let rec is_only_brackets block = match block with
  | [] -> None
  | Enclosed('[', _) :: t -> is_only_brackets t
  | x :: _ -> Some x

(* assuming we read Word "current" :: block, we want to check if this is
"current ... current var_name" pattern. *)
let rec is_only_current block = match block with
  | [] -> failwith "Expected variable after current."
  | Word "current" :: t -> is_only_current t
  | [Word _] -> true
  | _ -> false
let rec var_id_current block var_li = match block with
  | [] -> failwith "Expected variable after current."
  | Word "current" :: t -> var_id_current t var_li
  | [Word w] -> begin match num_of_var var_li w with
      | Some i -> i
      | _ -> failwith ("Unknown variable "^w^" near current.")
    end
  | _ -> failwith ("Faulty implementation : expected a stack of Word \"current\".")

(* assuming we read Word "previous" :: block, we want to check if this is
"previous ... previous var_name" pattern. *)
let rec is_only_previous block = match block with
  | [] -> failwith "Expected variable after previous."
  | Word "previous" :: t -> is_only_previous t
  | [Word _] -> true
  | _ -> false
(* counts in the tail of
Word "previous" :: [Word "previous", ..., Word "previous", Word var_name] *)
let rec count_previous block var_li = match block with
  | Word "previous" :: t -> let id, n = count_previous t var_li in id, (n+1)
  | [Word w] -> begin match num_of_var var_li w with
      | Some i -> i, 1
      | _ -> failwith ("Unknown variable "^w^" near previous.")
    end
  | _ -> failwith "Expected variable after previous."

let rec read_block var_li fun_li block = let r = read_block var_li fun_li 
  in match block with
  | [] -> failwith "Expected expression."
  | [Word "true"] -> Val (Bool_c true)
  | [Word "false"] -> Val (Bool_c false)
  | [Word w] -> begin match num_of_var var_li w with
      | Some i -> Var(i, 0)
      | _ -> begin match int_of_string_opt w with
          | Some n -> Val (Int_c n)
          | _ -> begin match float_of_string_opt w with
              | Some x -> Val (Float_c x)
              | _ -> failwith ("Unknown variable "^w^" : can't access this variable.")
            end
        end
    end
  | [Litteral_string s] -> Val (String_c s)
  | [Enclosed('(', x)] -> r x
  | [Enclosed('[', elements)] -> let li = List.map r (split_comma elements) in Array_b(Array.of_list li)
  | [Word fun_name; Enclosed('(', args)] -> begin match num_of_var fun_li fun_name with
      | Some i -> Funcall_b(i, List.map r (split_comma args))
      | _ -> failwith ("Unknown function "^fun_name^".")
    end
  | Word "current" :: t when is_only_current t -> Var(var_id_current t var_li, 0)
  | Word "previous" :: t when is_only_previous t -> let id, prev = count_previous t var_li in Var(id, prev)
  | x :: t when is_only_brackets t = None -> let rec aux li = match li with
      | [] -> r [x]
      | Enclosed('[', index) :: t -> Elem(aux t, r index)
      | _ -> failwith "Faulty implementation : expected bracket enclosed elements."
    in aux (List.rev t)
  | [Symbol ';'; x] -> Not(r [x])
  | _ -> let a, w, b = split_on_precedence block in let a = match a, w with
      | [], Symbol '-' | [], Big_operator '-' -> [Word "0"]
      | _ -> a
    in (constructor_of_binary_operator w) (r a) (r b)

let data_of_declare var_name var_li = match num_of_var var_li var_name with
  | None -> List.length var_li, var_name :: var_li
  | Some i -> i, var_li

let rec find_coord li = match li with
  | [] -> failwith "Expected expression."
  | Enclosed('[', x) :: t -> let coord_end, w, value = find_coord t in Enclosed('[', x) :: coord_end, w, value
  | w :: t -> [], w, t

let reassignator var_name coord_li assign_symbol value var_li fun_li = let r = read_block var_li fun_li
  in let id = match num_of_var var_li var_name with
    | Some i -> i
    | _ -> failwith ("Unknown variable "^var_name^".")
  in let rec aux li = match li with
    | [] -> []
    | Enclosed('[', block) :: t -> r block :: aux t
    | _ -> failwith "Faulty implementation : expected bracket enclosed elements."
  in let result = match assign_symbol with
    | Symbol '=' -> r value
    | Symbol_multi s -> constructor_of_binary_operator (Symbol s.[0]) (r (Word var_name :: coord_li)) (r value)
    | _ -> failwith "Faulty implementation : expected assignement symbol."
  in Reassign(id, aux coord_li, result)

let rec read_line var_li fun_li line = let r = read_block var_li fun_li 
  in match line with
  | Line_s (_, []) -> failwith "Expected non-empty line."
  | Line_s (n, Word "const" :: Word "const" :: Word var_name :: Symbol '=' :: t) -> let i, newvarli = data_of_declare var_name var_li in Declare(false, false, i, n, r t), newvarli
  | Line_s (n, Word "const" :: Word "var" :: Word var_name :: Symbol '=' :: t) -> let i, newvarli = data_of_declare var_name var_li in Declare(false, true, i, n, r t), newvarli
  | Line_s (n, Word "var" :: Word "const" :: Word var_name :: Symbol '=' :: t) -> let i, newvarli = data_of_declare var_name var_li in Declare(true, false, i, n, r t), newvarli
  | Line_s (n, Word "var" :: Word "var" :: Word var_name :: Symbol '=' :: t) -> let i, newvarli = data_of_declare var_name var_li in Declare(true, true, i, n, r t), newvarli
  | Line_s (_, Word var_name :: Symbol '=' :: t) -> begin match num_of_var var_li var_name with
      | Some i -> Reassign(i, [], r t), var_li
      | _ -> failwith ("Unknown variable "^var_name^" : can't reassign this variable.")
    end
  | Line_s (_, Word var_name :: t) when is_assignement (is_only_brackets t) -> let coord_li, w, value = find_coord t
    in reassignator var_name coord_li w value var_li fun_li, var_li
  | If_s li -> let lif, newv = read_prog_li var_li fun_li li in If lif, newv
  | Line_s (_, [Word "reverse"]) -> Reverse, var_li
  | Line_s (_, Word "return" :: t) -> Return(r t, content_buf), var_li
  | Line_s (_, [Word fun_name; Enclosed('(', args)]) -> begin match num_of_var fun_li fun_name with
      | Some i -> Funcall_l(i, List.map r (split_comma args)), var_li
      | _ -> failwith ("Unknown function "^fun_name^".")
    end
  | _ -> failwith "Syntax error."
  
  and read_prog var_li fun_li prog = let rec aux var_li fun_li prog = match prog with
      | [] -> [], var_li
      | x :: t -> let s, newv = read_line var_li fun_li x
        in let seq, finalv = aux newv fun_li t
        in s :: seq, finalv
    in let final, finalv = aux var_li fun_li prog
    in Array.of_list final, finalv

  and read_prog_li var_li fun_li prog_li = match prog_li with
    | [] -> [], var_li
    | (cdt, x) :: t -> let y, newv = read_prog var_li fun_li x
      in let seq, finalv = read_prog_li newv fun_li t
      in ((read_block var_li fun_li cdt), y) :: seq, finalv

let read_fun (_, arg_li, def) fun_li = let prog, var_li = read_prog (List.rev arg_li) fun_li def
  in Custom(List.length arg_li, List.length var_li, prog)

let init n_var functions = {
  data = Array.make n_var {
    content = [];
    reassign = false;
    edit = false;
    num_exclam = 0
  };
  functions = Array.of_list functions;
  direction = 1;
  returned = false
}

let setup prog_string = let prog_main_first, fun_def_li = split_statement (read prog_string)
  in let rec get_names li = match li with
    | [] -> builtin_fun_names
    | (x, _, _) :: t -> x :: get_names t
  in let fun_li = get_names fun_def_li
  in let rec get_definitions li = match li with
    | [] -> builtin_functions
    | x :: t -> read_fun x fun_li :: get_definitions t
  in let prog_main, var_li = read_prog [] fun_li prog_main_first
  in let context = init (List.length var_li) (List.rev (get_definitions fun_def_li))
  in prog_main, context
