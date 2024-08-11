type content = Int_c of int
  | Float_c of float
  | Array_c of content array
  | Bool_c of bool
  | String_c of string
  | Unit_c

type variable = {
  mutable content:content list;
  reassign:bool;
  edit:bool;
  num_exclam:int
}

type code_block = Val of content
    | Var of int*int (* id, number of previous *)
    | Array_b of code_block array
    | Elem of code_block*code_block (* array/string, index *)
    | Add of code_block*code_block
    | Sub of code_block*code_block
    | Mult of code_block*code_block
    | Div of code_block*code_block
    | Mod of code_block*code_block
    | Eq of code_block*code_block
    | Eq_loose of code_block*code_block
    | Le of code_block*code_block
    | Leq of code_block*code_block
    | Or of code_block*code_block
    | And of code_block*code_block
    | Not of code_block
    | Funcall_b of int*code_block list (* function id, args *)
  and code_line = Declare of bool*bool*int*int*code_block (* var/const reassign, var/const edit, id, number of exclamation marks, value *)
    | Reassign of int*(code_block list)*code_block (* id, coordinates for arrays, value *)
    | If of (code_block*program) list (* if (condition, program), elif (condition, program), ... *)
    | Reverse
    | Return of code_block*(content option ref) (* value, destination *)
    | Funcall_l of int*code_block list (* Same as Funcall_b *)
  and program = code_line array
  and func_t = Builtin of (content list -> content)
    | Custom of int*int*program (* number of arguments, number of variables (including arguments), definition *)

type context_t = {
  data:variable array;
  functions:func_t array;
  mutable direction:int;
  mutable returned:bool
}

let content_buf = ref None (* for returns *)

let start_pos prog context = match context.direction with
  | 1 -> 0
  | _ -> Array.length prog -1

let rec exec_p prog pos context = if pos >= 0 && pos < Array.length prog && not context.returned
  then begin
    exec_l prog.(pos) context;
    exec_p prog (pos+context.direction) context
  end

  and exec_l line context = match line with
    | Reverse -> context.direction <- -context.direction
    | Declare(reassign, edit, id, exclam, value) -> if context.data.(id).num_exclam <= exclam || context.data.(id).content=[]
        then context.data.(id) <- {
          content=[exec_b value context];
          reassign=reassign;
          edit=edit;
          num_exclam=exclam
        }
    | Reassign(id, coord, value) -> begin match context.data.(id).content, coord with
        | l, [] when l<>[] -> if context.data.(id).reassign then
              context.data.(id).content <- exec_b value context :: l
            else failwith "This variable cannot be reassigned."
        | Array_c h :: _, _ -> if context.data.(id).edit then
          let rec change_val l a = match l with
            | [] -> failwith "Faulty implementation."
            | [block] -> let i = match exec_b block context with
                | Int_c k -> k+1
                | _ -> failwith "Index of array must be an integer."
              in a.(i) <- exec_b value context
            | block :: t -> let i = match exec_b block context with
                | Int_c k -> k+1
                | _ -> failwith "Index of array must be an integer."
              in (match a.(i) with
                | Array_c b -> change_val t b
                | _ -> failwith "An array of higher dimension was expected.")
          in change_val coord h
          else failwith "This array cannot be edited."
      | [], _ -> failwith "This variable has not yet been declared."
      | _ -> failwith "An array was expected."
      end
    | If li -> begin match li with
        | [] -> ()
        | (cdt, prog) :: t -> begin match exec_b cdt context with
            | Bool_c true -> begin match context.direction with
                | 1 -> exec_p prog 0 context
                | _ -> exec_p prog (Array.length prog - 1) context
              end
            | Bool_c false -> exec_l (If t) context
            | _ -> failwith "Expected bool as condition."
          end
      end
    | Return(value, dest) -> context.returned <- true; dest := Some (exec_b value context)
    | Funcall_l(id, args) -> let _ = exec_b (Funcall_b (id, args)) context in ()

  and exec_b block context = match block with
    | Val x -> x
    | Array_b a -> let n = Array.length a
      in let res = Array.make n Unit_c
      in let rec aux k = match k with
        | (-1) -> ()
        | _ -> res.(k) <- exec_b a.(k) context; aux (k-1)
      in aux (n-1); Array_c res
    | Elem(array_block, index) -> begin match exec_b array_block context, exec_b index context with
        | Array_c a, Int_c i -> a.(i+1)
        | String_c s, Int_c i -> String_c (String.make 1 s.[i])
        | _ -> failwith "This object isn't subscriptable."
      end
    | Var(id, prev) -> if List.length context.data.(id).content > prev then List.nth context.data.(id).content prev
      else failwith "This variable has not yet been declared or there are too many previous keywords."
    | Add(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Int_c (a+b)
        | Float_c a, Int_c b -> Float_c (a +. (float_of_int b))
        | Int_c a, Float_c b -> Float_c ((float_of_int a) +. b)
        | Float_c a, Float_c b -> Float_c (a+.b)
        | String_c a, String_c b -> String_c (a^b)
        | Array_c a, Array_c b -> Array_c (Array.append a b)
        | _ -> failwith "Wrong types for + operator."
      end
    | Sub(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Int_c (a-b)
        | Float_c a, Int_c b -> Float_c (a -. (float_of_int b))
        | Int_c a, Float_c b -> Float_c ((float_of_int a) -. b)
        | Float_c a, Float_c b -> Float_c (a-.b)
        | _ -> failwith "Wrong types for - operator."
      end
    | Mult(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Int_c (a*b)
        | Float_c a, Int_c b | Int_c b, Float_c a -> Float_c (a *. (float_of_int b))
        | Float_c a, Float_c b -> Float_c (a*.b)
        | Array_c a, Int_c n | Int_c n, Array_c a -> let rec aux k = match k with
          | 0 -> []
          | _ -> Array.copy a :: aux (k-1)
          in Array_c(Array.concat (aux n))
        | _ -> failwith "Wrong types for * operator."
      end
    | Div(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Int_c (a/b)
        | Float_c a, Int_c b -> Float_c (a /. (float_of_int b))
        | Int_c a, Float_c b -> Float_c ((float_of_int a) /. b)
        | Float_c a, Float_c b -> Float_c (a/.b)
        | _ -> failwith "Wrong types for / operator."
      end
    | Mod(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Int_c (a mod b)
        | _ -> failwith "Wrong types for % operator."
      end
    | Eq(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Bool_c (a=b)
        | Float_c a, Float_c b -> Bool_c (a=b)
        | String_c a, String_c b -> Bool_c (a=b)
        | _ -> Bool_c false
      end
    | Eq_loose(x, y) -> begin match exec_b x context, exec_b y context with
        | Float_c a, Float_c b -> Bool_c (Float.round a = Float.round b)
        | Float_c a, Int_c b | Int_c b, Float_c a -> Bool_c (Float.round a = float_of_int b)
        | a, b -> exec_b (Eq (Val a, Val b)) context
      end
    | Le(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Bool_c (a<b)
        | Float_c a, Float_c b -> Bool_c (a<b)
        | String_c a, String_c b -> Bool_c (a<b)
        | Float_c a, Int_c b -> Bool_c (a < (float_of_int b))
        | Int_c a, Float_c b -> Bool_c ((float_of_int a) < b)
        | _ -> failwith "Wrong types for comparison."
      end
    | Leq(x, y) -> begin match exec_b x context, exec_b y context with
        | Int_c a, Int_c b -> Bool_c (a<=b)
        | Float_c a, Float_c b -> Bool_c (a<=b)
        | String_c a, String_c b -> Bool_c (a<=b)
        | Float_c a, Int_c b -> Bool_c (a <= (float_of_int b))
        | Int_c a, Float_c b -> Bool_c ((float_of_int a) <= b)
        | _ -> failwith "Wrong types for comparison."
      end
    | Or(x, y) -> begin match exec_b x context, exec_b y context with
        | Bool_c a, Bool_c b -> Bool_c (a||b)
        | _ -> failwith "Wrong types for or."
      end
    | And(x, y) -> begin match exec_b x context, exec_b y context with
        | Bool_c a, Bool_c b -> Bool_c (a&&b)
        | _ -> failwith "Wrong types for and."
      end
    | Not x -> begin match exec_b x context with
        | Bool_c a -> Bool_c (not a)
        | _ -> failwith "Expected bool for ; operator."
      end
    | Funcall_b (id, args) -> let f = context.functions.(id)
      in let rec compute_args li = match li with
        | [] -> []
        | x :: t -> (exec_b x context) :: (compute_args t)
      in match f with
        | Builtin g -> g (compute_args args)
        | Custom(n_args, n_vars, def) -> if List.length args <> n_args then failwith "Not the right number of arguments."
          else let context_f = {
            data = Array.make n_vars {
              content = [];
              reassign = false;
              edit = false;
              num_exclam = 0
            };
            functions = context.functions;
            direction = 1;
            returned = false
            }
          in let rec fill_args li k = match li with
            | [] -> ()
            | x :: t -> context_f.data.(k) <- {
                content = [x];
                reassign = true;
                edit = true;
                num_exclam = 0
              }; fill_args t (k+1)
          in fill_args (compute_args args) 0;
          content_buf := None;
          exec_p def 0 context_f;
          match !content_buf with
            | None -> failwith "This function has no return value."
            | Some c -> c
