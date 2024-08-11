open Base

let builtin_fun_names = [
  "print";
  "len";
  "exp";
  "sqrt"
]

let f_print c = match c with
  | [String_c s] -> print_string s; Unit_c
  | [Int_c i] -> print_int i; Unit_c
  | [Float_c i] -> print_float i; Unit_c
  | [Bool_c true] -> print_string "true"; Unit_c
  | [Bool_c false] -> print_string "false"; Unit_c
  | _ -> failwith "Invalid argument for print."
let f_len c = match c with
  | [Array_c a] -> Int_c (Array.length a)
  | [String_c s] -> Int_c (String.length s)
  | _ -> failwith "Invalid argument for len."
let f_exp c = match c with
  | [Int_c i] -> Float_c (exp (float_of_int i))
  | [Float_c i] -> Float_c (exp i)
  | _ -> failwith "Invalid argument for exp."
let f_sqrt c = match c with
  | [Int_c i] -> Float_c (sqrt (float_of_int i))
  | [Float_c i] -> Float_c (sqrt i)
  | _ -> failwith "Invalid argument for sqrt."

let builtin_functions = [
  Builtin f_print;
  Builtin f_len;
  Builtin f_exp;
  Builtin f_sqrt
]
