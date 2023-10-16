(* *********************************************************** *)
(*  Sort di una string list per lunghezza elementi ascendente  *) 

let sort_help s1 s2 = 
	let len_s1 = String.length s1 in	
	let len_s2 = String.length s2 in
		if len_s1 > len_s2
			then 1
			else if len_s1 = len_s2
				then 0
				else -1;;
	
let sorter_lst lst = List.sort sort_help lst;;  



let printer lst = 
	print_string("[");
	let rec printer_helper lst = match lst with
		[] -> print_string("]"); ()
		| hd::tl -> print_string ("\""^hd^"\""); 
			if tl <> []
				then print_string("; ");
			printer_helper tl
	in printer_helper lst;;


let my_print_int prefix value postfix =
	print_string(prefix); 
	print_int(value); 
	print_string(postfix);;

let my_print_char prefix value postfix =
	print_string(prefix); 
	print_char(value); 
	print_string(postfix);;


let rec single_check s1 s2 i j counter_char k =
	let len_s1 = String.length s1 in
	let len_s2 = String.length s2 in
		if (j <= (len_s2-k) || (j < len_s2 && counter_char > 0)) (* len_s2-j) >= k  ----  per testare tutta la stringa:  j < len_s2 *)
			then (
				my_print_int "i: " i "\n";
				my_print_int "j: " j "\n"; 
				my_print_char "s1.[i]: " s1.[i] "\n";
				my_print_char "s2.[j]: " s2.[j] "\n";
				my_print_int "len_s1-1: " (len_s1-1) "\n\n"; 

				if s1.[i] = s2.[j]
					then if (len_s1-1) = counter_char
						then true
						else single_check s1 s2 (i+1) (j+1) (counter_char+1) k
					else single_check s1 s2 0 (j+1 -counter_char) 0 k
			)
			else false;;



exception NonPresente;;

let rec find_substring_check small_str str_list k index counter = 
	let small_str_len = String.length small_str in
		if (((index+counter) < small_str_len  && counter > 0) || index <= (small_str_len-k))
			then
				try
					let rec check_all list_to_test = match list_to_test with
						[] -> if (counter+1) < k
							then ( 
								print_string("** find_substring_check -- testate tutte le stringhe **\n");
								my_print_int "index: " index "\n";
								my_print_int "counter: " counter "\n\n";
								find_substring_check small_str str_list k index (counter+1)
							)
							else (true, String.sub small_str index (counter+1))  (* Fine ricerca a "k" *)
						| x::rest -> print_string("** find_substring_check **\n");
							my_print_int "index: " index "\n";
							my_print_int "counter: " counter "\n";
							print_string("x: "^x^"\n");
							print_string("to find: "^(String.sub small_str index (counter+1))^"\n\n");

							if (single_check (String.sub small_str index (counter+1)) x 0 0 0 k)
								then check_all rest
								else raise NonPresente
					in check_all str_list
				with NonPresente -> print_string("** find_substring_check -- testata stringa **\n");
					my_print_int "index: " index "\n";
					my_print_int "counter: " counter "\n\n";

					find_substring_check small_str str_list k (index+1) 0
			else (false, "");;


exception KNonUtilizzabile of string;;
exception ListaVuota of string;;

let find_substring lst_of_string k = 
 	if k <= 0 
 		then raise (KNonUtilizzabile "Usare un K >= 0")
 	else if lst_of_string = []
 		then raise (ListaVuota "Usare una lista con almeno un elemento")
	else if List.length lst_of_string = 1
 		then (false, "")
 		else 
 			let sorted_list = sorter_lst lst_of_string in 
			let shortest_string = List.hd sorted_list in
			let tail = List.tl sorted_list in
				if k > String.length shortest_string
					then raise (KNonUtilizzabile "Usare un K <= lunghezza stringa piu' corta nella lista") 
				else if shortest_string = ""
					then (false, "")
 					else (
 						print_string("\n** Inizio ricerca substring **\n\n");
 						print_string("Parola campione: \""^shortest_string^"\"\n");
 						print_string("Lista in cui cercare la substring: "); printer tail; print_newline();
 						my_print_int "Lunghezza substring (K): " k "\n\n\n";
 						find_substring_check shortest_string tail k 0 0
 					);;

find_substring [] 4;; 

find_substring ["abcd";"ab"] 5;; 

find_substring ["òàùèì";"èòù"] 1;;

find_substring ["abcde";"fghijkcdelm";"nopqrcdstuqwdcdeqwq"] 3;;

find_substring ["ciao";"ciaooo";"ciooaociao"] 4;; 

find_substring ["ciao";"misoa";"cartia"] 3;;

find_substring ["ciao";"falciasao";"piae"] 3;;


(**********************************************************)
(***************** MIGLIORIE DA APPORTARE *****************)
(**********************************************************)
- Fai un disclaimer dicendo che le lettere accentate non 
  vengono stampate
- Trova substring maggiori di K chiedendolo all'utente
- Misura il tempo di computazione e mettilo a confronto con
  quello che si avrebbe usando l'altra tecnica di risoluzione

(**********************************************************)
(****************** MIGLIORIE APPORTATE *******************)
(**********************************************************)
- Termina prima la ricerca quando fai scansione di una 
  parola nella lista e index supera len_stringa-k 
- Lancia delle eccezioni se:
	- k <= 0
	- k > len str più corta 
	- lista vuota


(**********************************************************)
(******************* ERRORI INDIVIDUATI *******************)
(**********************************************************)
- Lettere accentate non vengono visualizzate


(******************************************************)
(******************* ERRORI RISOLTI *******************)
(******************************************************)
- Se k = 0 il mio input viene comunque testato come 1
- Se passo una lista vuota
- Se passo una lista con un solo elemento



(******************************************************)
(******************* NOTE PROGETTO ********************)
(******************************************************)
- Ho scelto di testare ad ogni turno le stringhe a 
  ripartire dall'indice iniziale perchè nei principali 
  alfabeti le parole di media sono lunghe 6 caratteri
  quindi il tempo che impiego per ricontrollare quei 
  pochi char, almeno secondo me, risulta essere 
  inferiore rispetto al costo in memoria che si 
  creerebbe se dovessi salvarmi ogni volta gli indici 
  in cui devo testare il carattere successivo

  single_check (String.sub small_str index (counter+1)) x 0 0 0 k    (* riga 82, sotto "to find: " *)

