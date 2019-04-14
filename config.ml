open Mirage

let data_dir = "data"

let disk  = crunch data_dir

let port =
  let doc = Key.Arg.info ~doc:"Listening port." ["port"] in
  Key.(create "port" Arg.(opt int 443 doc))

let dns_key =
  let doc = Key.Arg.info ~doc:"nsupdate key (name:type:value,...)" ["dns-key"] in
  Key.(create "dns-key" Arg.(required string doc))

let dns_server =
  let doc = Key.Arg.info ~doc:"dns server IP" ["dns-server"] in
  Key.(create "dns-server" Arg.(required ipv4_address doc))

let dns_port =
  let doc = Key.Arg.info ~doc:"dns server port" ["dns-port"] in
  Key.(create "dns-port" Arg.(opt int 53 doc))

let hostname =
  let doc = Key.Arg.info ~doc:"hostname" ["hostname"] in
  Key.(create "hostname" Arg.(required string doc))

let key_seed =
  let doc = Key.Arg.info ~doc:"certificate key seed" ["key-seed"] in
  Key.(create "key-seed" Arg.(required string doc))

let keys = Key.[
    abstract port ; abstract dns_key ; abstract dns_server ; abstract dns_port ;
    abstract hostname ; abstract key_seed
  ]

let stack = generic_stackv4 default_network

let logger =
  syslog_udp ~config:(syslog_config "tls.nqsb.io") stack

let server =
  foreign ~deps:[abstract nocrypto ; abstract logger ; abstract app_info] ~keys "Unikernel.Main" @@
  random @-> pclock @-> time @-> stackv4 @-> kv_ro @-> job

let () =
  let packages = [
    package ~min:"0.2.1" "logs-syslog" ;
    package ~min:"3.7.1" "tcpip" ;
    package ~min:"2.0.0" "mirage-kv" ;
    package "cohttp-mirage" ;
    package "yojson" ;
    package "sexplib" ;
    package ~sublibs:["mirage"] "tls" ;
    package ~sublibs:["lwt"] "logs" ;
    package "udns-mirage-certify" ;
  ]
  in
  register ~packages "tls-server" [
    server $ default_random $ default_posix_clock $ default_time $ stack $ disk
  ]
