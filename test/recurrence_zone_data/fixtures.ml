open Core
module D = Recurrence_zone_data
module Z = Recurrence_rules.Zone

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let timing_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Recurrence_rules.Timing_error.t)]
;;

let time = Time_ns.of_string_with_utc_offset
let ns t = Time_ns.to_int63_ns_since_epoch t |> Int63.to_int64
let of_ns t = Time_ns.of_int63_ns_since_epoch (Int63.of_int64_exn t)
let second value = of_ns Int64.(value * 1_000_000_000L)
let offset zone at = Z.offset_seconds zone at |> timing_ok
let new_york = [%blob "fixtures/America_New_York.tzif"]
let lord_howe = [%blob "fixtures/Australia_Lord_Howe.tzif"]
let apia = [%blob "fixtures/Pacific_Apia.tzif"]
let santiago = [%blob "fixtures/America_Santiago.tzif"]
let gaza = [%blob "fixtures/Asia_Gaza.tzif"]
let dublin = [%blob "fixtures/Europe_Dublin.tzif"]
let utc = [%blob "fixtures/Etc_UTC.tzif"]

let all =
  [ ( "America/New_York"
    , new_york
    , "d7f2206b3a45989fc9ad63d558922532fa7352280d5f87176bf1db79cb1d1fa9" )
  ; ( "Australia/Lord_Howe"
    , lord_howe
    , "f368bd25659c0293d02bb79ec7dac7d5b73a92dffafce14b4dd2ffb8ba11aada" )
  ; ( "Pacific/Apia"
    , apia
    , "dc70c47c80ab2c87a1ab754bab8febfc38508059e249dfe55e73a3759808ea14" )
  ; ( "America/Santiago"
    , santiago
    , "fd006953c2b442a2e1e66db2a967dd932a4824390f01cddd9c801ce63450c715" )
  ; "Asia/Gaza", gaza, "f8f0bffe018e0da0c682fe4b2f31fba6d34b6cd752b4f931198e0b99e6ef70d5"
  ; ( "Europe/Dublin"
    , dublin
    , "11c00336e02f1318fe764ab29467c5f2afefbfffa644fa8dd24f5b083b495b71" )
  ; "Etc/UTC", utc, "fddce1e648a1732ac29afd9a16151b2973cdf082e7ec0c690f7e42be6b598b93"
  ]
;;

let data bytes = D.of_string bytes ~source:"IANA2025b-controlled" |> ok

let zone ?(from = "1998-01-01 00:00:00Z") ?(until = "2102-01-01 00:00:00Z") bytes =
  D.zone (data bytes) ~name:"Fixture/Controlled" ~from:(time from) ~until:(time until)
  |> ok
;;
