open Base
(* open Lwt *)

type player = {input: Lwt_io.input_channel; output: Lwt_io.output_channel}

let (>>=) = Lwt.(>>=)
let return = Lwt.return
let flush = Lwt_io.flush

(*val sendMsg : string -> player list -> player list Lwt.t = <fun> *)
(* avec map_p on fait un calcul en plus *)

let sendMsg (msg:string) (liste_joueurs: player list): player list Lwt.t =

  (Lwt_list.iter_p (fun p -> (Lwt_io.fprintf (p.output) "%s\n" msg) 
    >>= (fun () -> flush p.output)) liste_joueurs) 
    >>= (fun _ -> return liste_joueurs) 


(* required type of get_answer : player list -> (player * string) list Lwt.t ! Got good type *)
let get_answer (lp:player list) : (player * string) list Lwt.t =
  Lwt_list.map_p (fun p -> Lwt_io.read_line p.input >>= fun resp -> return (p,resp)) lp

(* required type of filter_winners : string -> (player * string) list -> player list Lwt.t *)
let filter_winners (answer:string) (lpa:(player * string) list) : player list Lwt.t = (* lpa --> list players answers*)
  Lwt_list.filter_p(fun (_,answer_p) -> return (String.equal answer answer_p)) lpa >>= Lwt_list.map_p (fun (player,_)-> return player)

(* required type of filter_Fastest : player list -> player list Lwt.t 
  We need a player list Lwt.t as a return type because next function needs a player list Lwt.t type to give us the player 
  that has the correct answer
*)

let filter_Fastest (lp:player list) : player list Lwt.t =
  Lwt.choose (List.map ~f:(fun player -> Lwt_io.read_line player.input >>= fun _ -> return [player]) lp)
(* filter_faster_correct : string -> player list -> player *)


(*Changed find_first_w_promise argument type from player Lwt.t list to (player*string) Lwt.t list, to read answers from players externaly and simplify the recursive function *)

let filter_fastest_correct (answer:string) (lp: player list) : player list Lwt.t = 
  
  let wraped_answers_players = List.map ~f:(fun p->Lwt_io.read_line p.input >>= fun st -> return (p,st)) lp (* find_first_w_promise must take a (player * string) LWT.T list !! that's why used List.map *)
  
  in let rec find_first_w_promise (list_answers_player:( player * string) Lwt.t list) : player list Lwt.t = 
        
      Lwt.nchoose_split list_answers_player >>= fun (ready_threads,pending_threads) ->  Lwt_list.filter_p (fun (_,answer_p) -> return (String.equal answer answer_p)) ready_threads 
                                            >>= Lwt_list.map_p (fun (p,_)-> return p) 
                                            >>= fun all -> match all with
                                             | [] -> find_first_w_promise pending_threads
                                             | h::_ -> return [h] 
     in find_first_w_promise (wraped_answers_players)
  

(*let send_message_looser list_loosers:player list *)

let getPlayers n : player list Lwt.t =
  let sockaddr = Lwt_unix.ADDR_INET (UnixLabels.inet_addr_loopback, 3003) in
  let sock = Lwt_unix.socket Lwt_unix.PF_INET Lwt_unix.SOCK_STREAM 0 in
  Lwt_unix.set_close_on_exec sock ;
  Lwt_unix.setsockopt sock Lwt_unix.SO_REUSEADDR true ;
  Lwt_unix.bind sock sockaddr >>= fun () ->
  Lwt_unix.listen sock 3003 ;
  
  let rec acceptPlayer n acc : player list Lwt.t =
    if n > 0 then
      let pt =
        Lwt_unix.accept sock >>= fun (cliFD, _sock) ->
        let inputChan = Lwt_io.of_fd ~mode:Lwt_io.input cliFD in
        let outputChan = Lwt_io.of_fd ~mode:Lwt_io.output cliFD in
        {input=inputChan; output= outputChan} |> return
      in
      pt >>= fun p ->
      acceptPlayer (n - 1) (p :: acc)
    else
      acc |> return
  in
  acceptPlayer n []

let closePlayers listPlayers =
  Lwt_list.map_p
    
  (fun player -> Lwt_io.close player.input)
    listPlayers

let _ =
  Lwt_main.run

    (* création des player *)
    (
      (Lwt_io.fprintf Lwt_io.stderr "Attente des joueurs...\n") >>=
      fun () -> let threadListPlayers = getPlayers 2 in
     (* actions *)
      threadListPlayers >>=
      fun listPlayers -> (* fonction stupide à changer *) return listPlayers >>= sendMsg "Ocaml est assez cool : vrai ou faux ?" >>= get_answer >>=
      filter_winners "vrai" >>= sendMsg "Bravo!" 
      >>= sendMsg "Javascript est mieux qu'Ocaml: vrai ou faux ?" >>=
      get_answer >>=
      filter_winners "faux" >>= sendMsg "Bravo!"
      >>= sendMsg "Question de rapidité, avec quoi programment les vrais programmeurs : 1) nano, 2) emacs, 3) vi, 4) des papillons ?" >>=
      (*filter_Fastest  >>= 'ou' *) filter_fastest_correct "4" >>=
      sendMsg  "Vous êtes le grand champion des Netcat !"
      (*fermeture des player
      on reprend "threadListPlayers" pour être sur de tous les fermer *)
      >>= fun _ -> threadListPlayers >>= closePlayers)
