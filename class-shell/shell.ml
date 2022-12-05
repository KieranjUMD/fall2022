open Unix;;

type exec = { name: string;
              args: string list
            }

type command = 
  | Nav of string
  | SP of exec
  | Empty
  | Exit

let log_file = "shell.log"

let start_process {name; args} = 
  match fork () with
  | 0   -> (* we are in the child *)
           execvp name (Array.of_list (name::args))

  | pid -> (* we are the parent *)
      let (_, status) = waitpid [] pid in
        status

let exit_helper p_status = 
  match p_status with
  | WEXITED(s)   -> s
  | WSIGNALED(s) -> s
  | WSTOPPED(s)  -> s

let rec batch_shell commands = 
  match commands with
  | []           -> let _ = log "End of batch" in exit 0
  | Exit::cs     -> let _ = log "Exit" in exit 0
  | Empty::cs    -> batch_shell cs
  | Nav(dir)::cs -> 
      let _ = chdir dir in
      batch_shell cs
  | SP(ex)::cs   -> 
      let exit_status = start_process ex in
      (match exit_status with
      | WEXITED(0) -> batch_shell cs
      | _          -> exit (exit_helper exit_status)
        )

let test_cmds = [Exit];;

let main () = 
  batch_shell test_cmds

main()



