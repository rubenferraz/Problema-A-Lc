open F_parser

(*Cria uma referência para armazenar a variavel alfabética mais pequena encontrada na fórmula.*)
let variavel_mais_peq = ref 'Z'
  
(*Verifica se um caractere é uma letra minúscula ou maiúscula*)
let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false  (*Esta função vem daqui https://stackoverflow.com/questions/49184057/does-ocaml-have-a-module-that-is-like-isdigit-and-isalpha-in-c-c .*)
  
(*Função recursiva que converte uma fórmula lógica para NOR.*)
let rec to_nors form =
  match form with
  | Var a -> a (*Se a fórmula for uma variável, retorna a variável.*)
  | And (a, b) -> (*(A ∧ B); ((A%A)%(B%B)); NOR(NOR(A, A), NOR(B, B)) *)
      let a_nor = to_nors a in
      let b_nor = to_nors b in
      "((" ^ a_nor ^ " % " ^ a_nor ^ ")" ^ " % " ^ "(" ^ b_nor ^ " % " ^ b_nor
      ^ "))"
  | Or (a, b) -> (*(A v B);((A % B) % (A % B)); NOR(NOR(A, B), NOR(A, B))*)
      let a_nor = to_nors a in
      let b_nor = to_nors b in
      "((" ^ a_nor ^ " % " ^ b_nor ^ ")" ^ " % " ^ "(" ^ a_nor ^ " % " ^ b_nor
      ^ "))"
  | True -> (*(T), NOR(NOR(Z, ¬A); ((Z % Z) % (Z % Z)); NOR(NOR(Z, ¬Z), NOR(Z, ¬Z)) *)
      let convertido = Not (False) in 
      to_nors convertido
  | False -> (*(⊥); ((Z % (Z % Z)) % (Z % (Z % Z))); OR(NOR(Z, ¬Z)) *)
      let convertido = Not (Or (Var "Z", Not (Var "Z"))) in
      to_nors convertido
  | Not (Or (a, b)) -> (*¬(A ∨ B); (A % B); NOR(A, B)*)
    let a_nor = to_nors a in 
    let b_nor = to_nors b in 
    "("^a_nor^" % "^b_nor^")"
  | Not a -> (*A (¬A); (A % A); NOR(A, A)*)
      let a_nor = to_nors a in
      "(" ^ a_nor ^ " % " ^ a_nor ^ ")"
  | Implies (a, b) -> (*(A → B); (¬A ∨ B); (((A % A) % B) % ((A % A) % B)); NOR(NOR(¬A, B), NOR(¬A, B))  *)
      let a_nor = to_nors (Not a) in
      let b_nor = to_nors b in
      "(" ^ "(" ^ a_nor ^ " % " ^ b_nor ^ ")" ^ " % " ^ "(" ^ a_nor ^ " % " ^ b_nor
      ^ "))"
  | Equiv (a, b) -> (* (A ↔ B); (¬A ∨ B) ∧ (¬B ∨ A); (((A % B) % (A % B)) % ((A % A) % (B % B))); NOR(NOR(NOR(A, B), NOR(A, B)), NOR(NOR(A, A), NOR(B, B)))*)
      let convertido = And (Or (Not a, b), Or (Not b, a)) in
      to_nors convertido
  
(*Função que substitui a variável 'Z' na fórmula convertida para NOR pela variável alfabética mais pequena encontrada na fórmula original.*)
let z_para_mais_pequena ch = if ch == 'Z' then !variavel_mais_peq else ch
  
let () =
  match parse "stdin" with
  | Some [input_formula] -> (*Verifica se o input é uma lista com uma única fórmula.*)
      let formula_converted = to_nors input_formula in (*Converte a fórmula do input para NOR usando a função 'to_nors'.*)
      for b = 0 to String.length formula_converted - 1 do (*Cria um loop que percorre cada caractere na 'formula_converted'.*)
        let char_atual = String.get formula_converted b in (*Obtém o caractere atual na posição 'b' da 'formula_converted'.*)
        if
          is_alpha char_atual
          && Char.code char_atual < Char.code !variavel_mais_peq (*Verifica se o caractere atual é alfabético e se o eu código ASCII é menor que o código ASCII da variável mais pequena armazenada em 'variavel_mais_peq'.*)
        then variavel_mais_peq := char_atual (*Se se verificar a condição anterior, atualiza a varíavel 'variavel_mais_peq' com o caractere atual.*)
      done;
  
      let nova = String.map z_para_mais_pequena formula_converted in (*Aplica a função 'z_para_mais_pequena' a cada caractere da 'formula_converted' e armazena o resultado na variável 'nova'.*)
  
      print_endline nova (*Imprime a fórmula resultante*)
  | _ -> failwith "Invalid input" (*Lança uma exceção com a mensagem "Invalid input".*)




  