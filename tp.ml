#require "base";;
#require "lwt.unix" ;;
open Base;;


let (>>=) = Lwt.bind
let return = Lwt.return

let alarme (f:float) =
  Lwt_unix.sleep f
  >>= fun () -> Lwt_io.fprintf Lwt_io.stdout "debout %f" f

let alarme_en_paralle (l:float list) =
  Lwt.join (List.map ~f:alarme l)

let alarme_utilisateur () =
  Lwt_io.read_line Lwt_io.stdin
  >>= fun st -> alarme (Float.of_string st)

let rec rec_alarm () =
  Lwt_io.read_line Lwt_io.stdin
  >>= fun st -> Lwt.join [alarme (Float.of_string st); rec_alarm ()]
