exception NumeroStringheNonValido of string;;
exception LunghezzaMinimaNonValida of string;;
exception LunghezzaMassimaNonValida of string;;

exception ProlungaRicerca;;
exception ProlungaRicercaCambioK;;
exception ProlungaRicercaIncrementoK;;

exception NonPresente;;

exception KNonUtilizzabile of string;;
exception ListaVuota of string;;




let stampa_lista lst = 
	print_string("[");
	let rec stampa_lista_helper lst = match lst with
		[] -> print_string("]"); ()
		| hd::tl -> Printf.printf "%S" hd;
			if tl <> []
				then print_string("; ");
			stampa_lista_helper tl
	in stampa_lista_helper lst;;



let rec genera_stringa_random lunghezza risultato =
	if lunghezza <= 0
		then risultato
		else genera_stringa_random (lunghezza-1) (risultato^(Printf.sprintf "%c" (Char.chr (97 + (Random.int 26)))));;

let genera_stringa lunghezza_stringa substring_comune =
	let lunghezza_base = Random.int lunghezza_stringa in
		(genera_stringa_random lunghezza_base "") ^ substring_comune ^ (genera_stringa_random (lunghezza_stringa - lunghezza_base) "");;


let genera_lista () =
	print_string("Quante stringhe vuoi generare? ");

	let numero_stringhe = read_int() in 
		if numero_stringhe <= 0
			then raise (NumeroStringheNonValido "Il numero delle strighe da generare deve essere > 0")
			else (
				print_string("\nInserisci la substring che tutte le stringhe devono avere in comune: ");
				let substring_comune = read_line() in 
					print_string("\nInserisci il valore di lunghezza minimo per le stringhe da generare: ");
					let lunghezza_min = read_int() in 
						if lunghezza_min <= 0
							then raise (LunghezzaMinimaNonValida "Il valore di lunghezza minimo per le stringhe da generare deve essere > 0")
							else (
								print_string("\nInserisci il valore di lunghezza massima per le stringhe da generare (ad esso verra' aggiunto il valore della lunghezza della substring in comune): ");
								let lunghezza_max = read_int() in 
									if lunghezza_max < lunghezza_min
										then raise (LunghezzaMassimaNonValida "Il valore di lunghezza massima per le stringhe da generare deve essere >= del valore di lunghezza minimo")
										else (
											let risultato = 
												let rec genera_lista_helper numero_stringhe lista_risultato =
													if numero_stringhe = 0 
														then lista_risultato
														else (
															let lunghezza_random = (lunghezza_min + Random.int (lunghezza_max - lunghezza_min + 1)) in
																genera_lista_helper (numero_stringhe-1) ((genera_stringa lunghezza_random substring_comune) :: lista_risultato)
														)
												in genera_lista_helper numero_stringhe [];
											in stampa_lista risultato;
										)
							)
			);;




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


let stampa_soluzione (res_bool, res_string) richiesta =
	Printf.printf "(%B, %S) %s" res_bool res_string richiesta;;


let my_print_int prefix value postfix =
	print_string(prefix); 
	print_int(value); 
	print_string(postfix);;

let my_print_char prefix value postfix =
	print_string(prefix); 
	print_char(value); 
	print_string(postfix);;



let ask_more soluzione k =
	stampa_soluzione soluzione "\n\nCercare un'altra substring (S/n) ? ";
	let s = read_line() in 
		if s <> "" && (s.[0]='n' || s.[0]='N') 
			then soluzione
			else (
				print_string("\nCosa vuoi fare? (default = [1])\n"); (* Cambiare il valore di K (s/N) ? *)
				Printf.printf "[1] Continua con stesso K (K=%d)\n" k;
				Printf.printf "[2] Incrementare il valore di K (diventerebbe K=%d)\n" (k+1);
				print_string("[3] Cambiare il valore di K\n\n");
				print_string("Scelta: ");

				let s2 = read_int() in 
					if s2 = 2
						then raise ProlungaRicercaIncrementoK
					else if s2 = 3 
						then raise ProlungaRicercaCambioK
					else
						raise ProlungaRicerca
			);;



let rec single_check s1 s2 i j counter_char k =
	let len_s1 = String.length s1 in
	let len_s2 = String.length s2 in
		if (j <= (len_s2-k) || (j < len_s2 && counter_char > 0)) (* len_s2-j) >= k  ----  per testare tutta la stringa:  j < len_s2 *)
			then (
				Printf.printf "i: %d\n" i;
				Printf.printf "j: %d\n" j; 
				Printf.printf "s1.[i]: %C\n" s1.[i];
				Printf.printf "s2.[j]: %C\n" s2.[j];
				Printf.printf "len_s1-1: %d\n\n" (len_s1-1); 

				if s1.[i] = s2.[j]
					then if (len_s1-1) = counter_char
						then true
						else single_check s1 s2 (i+1) (j+1) (counter_char+1) k
					else single_check s1 s2 0 (j+1 -counter_char) 0 k
			)
			else false;;



let rec find_substring_check small_str str_list k index counter = 
	let small_str_len = String.length small_str in
		if (((index+counter) < small_str_len  && counter > 0) || index <= (small_str_len-k))
			then
				try
					let rec check_all list_to_test = match list_to_test with
						[] -> if (counter+1) < k
							then ( 
								print_string("** find_substring_check -- testate tutte le stringhe **\n");
								Printf.printf "index: %d\n" index;
								Printf.printf "counter: %d\n\n" counter;
								find_substring_check small_str str_list k index (counter+1)
							)
							else (
								try 
									ask_more (true, String.sub small_str index (counter+1)) k (* Fine ricerca a "k" *) 
								with 
									| ProlungaRicerca -> find_substring_check small_str str_list k (index+1) 0 
									| ProlungaRicercaIncrementoK -> if k > String.length small_str
										then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length small_str))^")") in
											raise (KNonUtilizzabile eccezione) 
										else find_substring_check small_str str_list (k+1) 0 0
									| ProlungaRicercaCambioK -> print_string("\nInserisci il nuovo valore di K: ");
										let new_k = read_int() in
											if new_k <= 0 
 												then raise (KNonUtilizzabile "Usare un K >= 0")
											else if new_k > String.length small_str
												then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length small_str))^")") in
													raise (KNonUtilizzabile eccezione) 
											else find_substring_check small_str str_list new_k 0 0
							)	
						| x::rest -> print_string("** find_substring_check **\n");
							Printf.printf "index: %d\n" index;
							Printf.printf "counter: %d\n" counter;
							Printf.printf "x: %s\n" x;
							Printf.printf "to find: %s\n\n" (String.sub small_str index (counter+1));

							if (single_check (String.sub small_str index (counter+1)) x 0 0 0 k)
								then check_all rest
								else raise NonPresente
					in check_all str_list
				with NonPresente -> print_string("** find_substring_check -- testata stringa **\n");
					Printf.printf "index: %d\n" index;
					Printf.printf "counter: %d\n\n" counter;

					find_substring_check small_str str_list k (index+1) 0
			else (false, "");;



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
					then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length shortest_string))^")") in
						raise (KNonUtilizzabile eccezione) 
				else if shortest_string = ""
					then (false, "")
 					else (
 						print_string("\n** Inizio ricerca substring **\n\n");
 						Printf.printf "Parola campione: %S\n" shortest_string;
 						print_string("Lista in cui cercare la substring: "); stampa_lista tail; print_newline();
 						Printf.printf "Lunghezza substring (K): %d\n\n\n" k;
 						find_substring_check shortest_string tail k 0 0
 					);;

["xxgnsdcmlmlnwertyuwailug"; "mncpewertymwgx"; "sdgbxpbaghmcluwertytqcnf"; "nvwertyyfryf"; "pmijjfwertyzitulyfszvn"; "dumgakxwertyzematgazdij"; "avxhbdbpedsgowertygcle"; "naojvnrrjouukrwertym"; "ewertyuvtcqgpdzfxkjtvx"; "mvmxxwertyuft"; "aucufbvshpknzfhwertywzsn"; "zwertywnixacqgwolys"; "orntuwertyzu"; "jqbwertyfsz"; "zwertyfunnlrw"; "wertyahgkxlfddfftucsvypqs"; "ixwertyixxsip"; "dqxtftnwertywi"; "szehmxwertyfjoytir"; "remuuwertyzbvzw"; "xtptgwertywvftv"; "bjsubulbacawertywrzg"; "vmtuctdwldmctwertyw"; "memlmskmwertybvffq"; "xxpcncttwertyy"; "ldjnwertynofczbhxbktyp"; "tqywertyqkhnfaa"; "duzwwertyythj"; "vgwertyahfs"; "lytwertylthkzc"; "elpzygezpkbeawertywtvhm"; "wertyycpxuyzfrexqsfvvqy"; "pmmjaoodumwertyixjwwi"; "rrzmafxswertyhrtxzsturef"; "wertyntvhdczxdmskmbziz"; "bdxahlswamulxvwwertyss"; "qwertyygdpdzbepighu"; "cgygaarjtwertyelozmvzbv"; "tgrxgwertyoncb"; "sshpsiwertyvelzhbsaa"; "lrcrmoiowertyat"; "lzttmwertyhmfu"; "fbjcyzswyczxliwertyzmqsi"; "koqecgnixhpacwertydqexawu"; "ajwertyjdzokjsfr"; "ekpwertyezqbjuffbyh"; "wertykoiprxzabyumbjewa"; "mpvsawertywpn"; "ahxhwertyng"; "lsiwertykjxmkxndrdph"; "rvwertybuyzkt"; "wertymppdnutcpuaw"; "awertyeiefu"; "mejwynepfgnrwertyffliy"; "qunagtotyyphhwertymbywps"; "ycwertympbguwbqws"; "fjfuiwertylsxedous"; "zbdwertyjskoftqawskvjt"; "vhezexwertymnwhyhnibem"; "cipiswertygfbtdlijgqpwxi"; "xnwertyyoeegwiujwss"; "rhaapnwertyq"; "pwertywruqnthg"; "jypmvtwertywwyot"; "bbmxbbrnxbyowpfkgwertyxq"; "qpxxhxbpwertypa"; "mtyvggtwertymlb"; "qpkfwccrjtywertyue"; "zgqnwertyit"; "trlhwertyzyriqeslobcrorn"; "owertybyyabqruqnxhjpmmp"; "wertyywkaijnwnv"; "hjvpahpidfgzhbuzwertyeoa"; "lqesizbwertyfbz"; "wertyjzbmwe"; "eygwertyzflugfqrj"; "rlwptwedwfwkuedvwertyu"; "aocbszwertyj"; "hcwertydudsw"; "tmpyjwertyamzr"; "hypjrwwertyyxlzaa"; "vmzwertyfbs"; "ogonggwertyj"; "cbhwertyyraujkpbhu"; "cwertyfboby"; "nywertyyrnrr"; "kpvjiiwertyypsrws"; "dekcwertysa"; "lqewertycuebglihcsw"; "dswertynazg"; "oqcouwertybmqbgss"; "wertyrprqfexwjgbqnw"; "tboinixzchcwertyjvrzebw"; "nkwertyjzuikhjzuphfzi"; "clitoswertynqz"; "cwertytxgazafsyhzehjqxihf"; "vwertygrymlfevj"; "vemihdgrwertyxyzxuvztojs"; "zwertyfrxpusos"; "miwbsowertyw"]

find_substring [] 4;; 

find_substring ["abcd";"ab"] 5;; 

find_substring ["òàùèì";"èòù"] 1;;

find_substring ["abcde";"fghijkcdelm";"nopqrcdstuqwdcdeqwq"] 3;;

find_substring ["ciao";"ciaooo";"ciooaociao"] 4;; 

find_substring ["ciao";"ciaooo";"ciooaociao"] 2;; 

find_substring ["ciao";"misoa";"cartia"] 3;;

find_substring ["ciao";"falciasao";"piae"] 3;;


(**********************************************************)
(***************** MIGLIORIE DA APPORTARE *****************)
(**********************************************************)
- Fai un disclaimer dicendo che le lettere accentate non 
  vengono stampate
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
- Trova substring maggiori di K chiedendolo all'utente


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

