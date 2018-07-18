class type message =
  object
    method fetch_headers: (string * string) list Lwt.t
    method fetch_body: string Lwt.t
  end

class type message_set =
  object
    method count: int Lwt.t
    method get: int32 -> message
    method uids: int32 list Lwt.t

    method contain_from: string -> message_set
    method is_unseen: message_set
  end

class type mailbox =
  object
    inherit message_set
    method name: string
  end

class account: host:string -> ?port:int -> username:string -> password:string -> unit ->
  object
    method inbox: mailbox
    method list_all: mailbox list Lwt.t
  end
