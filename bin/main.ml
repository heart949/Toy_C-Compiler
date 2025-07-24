let usage_msg = "toyc [-opt] < input.tc > output.s"
let optimization_enabled = ref false
let spec_list = [ "-opt", Arg.Set optimization_enabled, " 开启优化" ]

let read_all_stdin () =
  let buffer = Buffer.create 1024 in
  try
    while true do
      let line = input_line stdin in
      Buffer.add_string buffer line;
      Buffer.add_char buffer '\n'
    done;
    Buffer.contents buffer
  with
  | End_of_file -> Buffer.contents buffer
;;

let () =
  Arg.parse spec_list (fun _ -> ()) usage_msg;
  try
    (* 1. 从标准输入读取源代码 *)
    let source = read_all_stdin () in
    if String.length source = 0
    then (
      Printf.eprintf "错误：没有从标准输入读取到任何内容\n%!";
      exit 1);
    (* 2. 词法分析 *)
    let lexbuf = Lexing.from_string source in
    (* 3. 语法分析 *)
    let ast = Toyc_compiler__.Parser.program Toyc_compiler__.Lexer.tokenize lexbuf in
    Printf.eprintf "语法分析完成，共解析了 %d 个函数\n%!" (List.length ast);
    (* 4. 语义分析 *)
    let symtab = Toyc_compiler__.Symbol.enter_scope [] in
    let symtab =
      List.fold_left
        (fun s f ->
           Toyc_compiler__.Symbol.add_symbol
             f.Toyc_compiler__.Ast.name
             (Toyc_compiler__.Symbol.Func
                { ret_type = f.ret_type
                ; params =
                    List.map
                      (function
                        | Toyc_compiler__.Ast.Param name -> Toyc_compiler__.Ast.Int, name)
                      f.params
                ; declared = true
                })
             s)
        symtab
        ast
    in
    Toyc_compiler.analyze_program ast;
    Printf.eprintf "语义分析完成\n%!";
    (* 5. 代码生成 *)
    if !optimization_enabled then Printf.eprintf "优化已开启\n%!";
    let asm_items = Toyc_compiler__.Codegen.gen_program symtab ast in
    let asm_code = Toyc_compiler__.Riscv.emit_asm_to_string asm_items in
    (* 6. 输出到标准输出 *)
    Printf.printf "%s" asm_code;
    Printf.eprintf "代码生成完成\n%!"
  with
  | Sys_error msg ->
    Printf.eprintf "系统错误：%s\n" msg;
    exit 1
  | Toyc_compiler__.Parser.Error ->
    Printf.eprintf "语法分析错误\n";
    exit 1
  | Toyc_compiler.Semantic_error msg ->
    Printf.eprintf "语义错误：%s\n" msg;
    exit 1
  | Toyc_compiler.NotAllPathsReturn msg ->
    Printf.eprintf "控制流错误：%s\n" msg;
    exit 1
  | Failure msg ->
    Printf.eprintf "代码生成错误：%s\n" msg;
    exit 1
;;
