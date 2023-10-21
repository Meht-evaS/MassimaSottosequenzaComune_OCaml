(* *********************************************************************** *)
(*                          DEFINIZIONE ECCEZIONI                          *)
(* *********************************************************************** *)

(* Eccezioni - genera_lista() *)
exception NumeroStringheNonValido of string;;
exception LunghezzaMinimaNonValida of string;;
exception LunghezzaMassimaNonValida of string;;

(* Eccezioni - continua_ricerca() *)
exception ProlungaRicerca;;
exception ProlungaRicercaCambioK;;
exception ProlungaRicercaIncrementoK;;

(* Eccezioni - ricerca_substring_helper() *)
exception NonPresente;;
exception GiaInRisultato;;

(* Eccezioni - ricerca_substring() *)
exception KNonUtilizzabile of string;;
exception ListaVuota of string;;



(* *********************************************************************** *)
(*                 FUNZIONI GENERAZIONE RANDOMICA STRINGHE                 *)
(* *********************************************************************** *)

(* ***************************************** *)
(*  Funzione ausiliaria di genera_stringa()  *)

let rec genera_stringa_random lunghezza risultato =
	if lunghezza <= 0
		then risultato
		(* Char 97 = a,  Char (97+25) = z *)
		(* Quindi con Random.int 26 riesco a generare un char (in minuscolo) appartenente all'alfabeto *)
		else genera_stringa_random (lunghezza-1) (risultato^(Printf.sprintf "%c" (Char.chr (97 + (Random.int 26)))));;


(* *********************************************************** *)
(*  Genera una stringa randomica, tramite funzione ausiliaria  *)
(*  genera_stringa_random(), e la scrive su file               *)

let genera_stringa lunghezza_stringa substring_comune file bool_ultima_stringa =
	let lunghezza_base = Random.int lunghezza_stringa in
		let risultato = (genera_stringa_random lunghezza_base "") ^ substring_comune ^ (genera_stringa_random (lunghezza_stringa - lunghezza_base) "") in
			if (not bool_ultima_stringa)
				then Printf.fprintf file "%S; " risultato
				else Printf.fprintf file "%S]" risultato; 
			risultato ;;


(* ******************************************************************************** *)
(*  Genera una lista di stringhe, che viene poi salvata nel file "lista.txt", in    *)
(*  base ad alcuni parametri che vengono chiesti interattivamente all'utente quali: *)
(*    - numero di stringhe da generare                                              *)
(*    - lunghezza minima stringhe da generare                                       *)
(*    - lunghezza massima stringhe da generare                                      *)
(*    - valore della substring che avranno in comune tutte le stringhe generate     *)

let genera_lista () =
	print_string("\nQuante stringhe vuoi generare? ");

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
											let file = open_out "lista.txt" in
												Printf.fprintf file "[";
												let risultato = 
													let rec genera_lista_helper numero_stringhe lista_risultato =
														if numero_stringhe = 0 
															then lista_risultato
															else (
																let lunghezza_random = (lunghezza_min + Random.int (lunghezza_max - lunghezza_min + 1)) in
																	genera_lista_helper (numero_stringhe-1) ((genera_stringa lunghezza_random substring_comune file (if numero_stringhe = 1 then true else false)) :: lista_risultato)
															)
													in genera_lista_helper numero_stringhe [];
												in close_out file;
												print_string("\nLista salvata in \"lista.txt\"\n");
												risultato
										)
							)
			);;





(* *********************************************************************** *)
(*          FUNZIONE PRINCIPALE E AUSILIARIE DI RICERCA SUBSTRING          *)
(* *********************************************************************** *)

(* ****************************** *)
(*  Stampa una lista di stringhe  *)

let stampa_lista lista = 
	print_string("[");
	let rec stampa_lista_helper lista = match lista with
		[] -> print_string("]"); ()
		| hd::tl -> Printf.printf "%S" hd;
			if tl <> []
				then print_string("; ");
			stampa_lista_helper tl
	in stampa_lista_helper lista;;



(* ****************************************************************** *)
(*             Funzione ausiliaria di continua_ricerca()              *)
(*  Stampa la soluzione e la richiesta di continuo ricerca substring  *)

let stampa_soluzione (res_bool, res_string) lista_risultati richiesta =
	Printf.printf "(%B, %S)\n" res_bool res_string;
	print_string("Lista substring trovate: "); stampa_lista lista_risultati;
	Printf.printf "\n%s\n" richiesta;;

(* ***************************************************************************** *)
(*               Funzione di richiesta continua ricerca substring                *)
(*                                                                               *)
(*  La ricerca può continuare in 3 modi differenti:                              *)
(*    - mantenendo la stessa K   -> lancio eccezione ProlungaRicerca             *)
(*    - incrementando K di 1     -> lancio eccezione ProlungaRicercaIncrementoK  *)
(*    - cambiando il valore di K -> lancio eccezione ProlungaRicercaCambioK      *)

let continua_ricerca soluzione lista_risultati k =
	stampa_soluzione soluzione lista_risultati "\n\nCercare un'altra substring (S/n) ? ";
	let scelta = read_line() in 
		if scelta <> "" && (scelta.[0]='n' || scelta.[0]='N') 
			then soluzione
			else (
				print_string("\nCosa vuoi fare? (default = [1])\n");
				Printf.printf "[1] Continua con stesso K (K=%d)\n" k;
				Printf.printf "[2] Incrementare il valore di K (diventerebbe K=%d)\n" (k+1);
				print_string("[3] Cambiare il valore di K\n\n");
				print_string("Scelta: ");

				let scelta2 = read_line() in 
					if scelta2 = "2"
						then raise ProlungaRicercaIncrementoK
					else if scelta2 = "3" 
						then raise ProlungaRicercaCambioK
					else
						raise ProlungaRicerca
			);;



(* *********************************************************** *)
(*  Sort di una string list per lunghezza elementi ascendente  *)

let ordina_ascendente_lunghezza stringa1 stringa2 = 
	let lunghezza_s1 = String.length stringa1 in	
	let lunghezza_s2 = String.length stringa2 in
		if lunghezza_s1 > lunghezza_s2
			then 1
			else if lunghezza_s1 = lunghezza_s2
				then 0
				else -1;;
	
let ordina_lista lista = List.sort ordina_ascendente_lunghezza lista;;  



(* ************************************************************************************* *)
(*  Controlla se la substring che stiamo per ricercare è gia presente all'interno della  *)
(*  lista di substring precedentemente trovate così da non doverla ricercare nuovamente  *)

  
let uguale = (=);;
let gia_in_risultato_helper substring lista_risultati = List.exists (uguale substring) lista_risultati;;

let gia_in_risultato contatore_char index k stringa_piu_corta lista_risultati = 
	if (List.length lista_risultati > 0 && (contatore_char = 0 && (index+k) <= (String.length stringa_piu_corta)))
		then let substring_completa = (String.sub stringa_piu_corta index k) in
			if (gia_in_risultato_helper substring_completa lista_risultati)
				then (
				    print_string("\n** ricerca_substring_helper -- substring \"" ^ substring_completa ^ "\" gia trovata precedentemente **\n"); 
				    true
			    ) else false
	    else false;;



(* ***************************************************************************** *)
(*  Questa funzione serve a verificare se una specifica substring, appartenente  *)
(*  alla stringa più corta, è presente all'interno di una specifica stringa tra  *)
(*  quelle che compongo la lista di confronto                                    *)

let rec cerca_substring_in_stringa substring stringa_confronto i j contatore_char k =
	let len_substring = String.length substring in
	let len_stringa_confronto = String.length stringa_confronto in
		(* Continuo a testare una stringa che NON ha ottenuto un match solo fino a lunghezza stringa - k perchè oltre *)
		(* quell'index, anche se trovassi un match di un char, non arriverei ad avere una substring di lunghezza k    *)
		if (j <= (len_stringa_confronto-k) || (j < len_stringa_confronto && contatore_char > 0)) (* per testare tutta la stringa:  j < len_stringa_confronto *)
			then (
				Printf.printf "i: %d\n" i;
				Printf.printf "j: %d\n" j; 
				Printf.printf "        substring.[i]: %C\n" substring.[i];
				Printf.printf "stringa_confronto.[j]: %C\n\n" stringa_confronto.[j];
				(* Printf.printf "len_substring-1: %d\n\n" (len_substring-1); *) 

				if substring.[i] = stringa_confronto.[j]
					(* Se ho letto tutta la substring e il numero dei char trovati è uguale alla lunghezza della *)
					(* substring, allora ho trovato quella substring nella stringa                               *)
					then if (len_substring-1) = contatore_char
						then true
						else cerca_substring_in_stringa substring stringa_confronto (i+1) (j+1) (contatore_char+1) k
					else cerca_substring_in_stringa substring stringa_confronto 0 (j+1 -contatore_char) 0 k
			)
			else false;;


(* ********************************************************************************** *)
(*  Questa funzione utilizza il backtracking e permette di confrontare la stringa più *)
(*  corta con il resto delle stringhe presenti nella lista al fine di verificare la   *)
(*  presenza di una substring di lunghezza K in comune tra tutte quelle stringhe      *)

let rec ricerca_substring_helper stringa_piu_corta lista k index contatore_char lista_risultati = 
	let len_stringa_piu_corta = String.length stringa_piu_corta in
		(* Continuo a testare la stringa più corta che NON ha ottenuto un match solo fino a lunghezza stringa - k perchè *)
		(* oltre quell'index, anche se trovassi un match di un char, non arriverei ad avere una substring di lunghezza k *)
		if (((index+contatore_char) < len_stringa_piu_corta  && contatore_char > 0) || index <= (len_stringa_piu_corta-k))
			then
				try
					if (gia_in_risultato contatore_char index k stringa_piu_corta lista_risultati)
				        then raise GiaInRisultato
					    else (
        					let rec cerca_substring_in_lista lista_da_testare = match lista_da_testare with
        						(* Guardo se la lunghezza della substring appena trovata è inferiore a K *)
        						(* e in caso affermativo continuo la ricerca sulla substring ^ 1 char    *)
        						[] -> if (contatore_char+1) < k
        							then ( 
        								print_string("** ricerca_substring_helper -- testate tutte le stringhe **\n");
        								Printf.printf "index:          %d\n" index;
        								Printf.printf "contatore_char: %d\n\n" contatore_char;
        								ricerca_substring_helper stringa_piu_corta lista k index (contatore_char+1) lista_risultati
        							)
        							(* Ho trovato una soluzione, ovvero una substring di lunghezza K *)
        							else (
        								(* Chiedo all'utente se vuole continuare la ricerca di altre substring, magari cambiando anche K *)
        								let risultato = String.sub stringa_piu_corta index (contatore_char+1) in
        								let lista_risultati_aggiornata = (risultato :: lista_risultati) in
        									try 
        										continua_ricerca (true, risultato) lista_risultati_aggiornata k 
        									with 
        										| ProlungaRicerca -> ricerca_substring_helper stringa_piu_corta lista k (index+1) 0 lista_risultati_aggiornata
        										| ProlungaRicercaIncrementoK -> if k > String.length stringa_piu_corta
        											then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length stringa_piu_corta))^")") in
        												raise (KNonUtilizzabile eccezione) 
        											else ricerca_substring_helper stringa_piu_corta lista (k+1) 0 0 []
        										| ProlungaRicercaCambioK -> print_string("\nInserisci il nuovo valore di K: ");
        											let new_k = read_int() in
        												if new_k <= 0 
        	 												then raise (KNonUtilizzabile "Usare un K >= 0")
        												else if new_k > String.length stringa_piu_corta
        													then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length stringa_piu_corta))^")") in
        														raise (KNonUtilizzabile eccezione) 
        												else ricerca_substring_helper stringa_piu_corta lista new_k 0 0 []
        							)	
        						| x::rest -> print_string("** ricerca_substring_helper -- test stringa successiva **\n");
        							Printf.printf "index:          %d\n" index;
        							Printf.printf "contatore_char: %d\n" contatore_char;
        							Printf.printf "  stringa: %S\n" x;
        							Printf.printf "substring: %S\n\n" (String.sub stringa_piu_corta index (contatore_char+1));
        
        							(* Se trovo la substring all'interno dell'attuale stringa, proseguo la ricerca nella stringa successiva *)
        							(* altrimenti lancio l'eccezione NonPresente che va ad effettuare il backtracking vero e proprio        *)
        							(* abbandonando questo ramo di soluzione e richiamando l'algoritmo di ricerca sull'indice successivo    *)
        							if (cerca_substring_in_stringa (String.sub stringa_piu_corta index (contatore_char+1)) x 0 0 0 k)
        								then cerca_substring_in_lista rest
        								else raise NonPresente
        					in cerca_substring_in_lista lista
        				)
				with 
					(* Backtracking e richiamo algoritmo su indice successivo della stringa più corta *)
					| NonPresente -> print_string("** ricerca_substring_helper -- testata stringa, fallimento **\n");
						Printf.printf "index:          %d\n" index;
						Printf.printf "contatore_char: %d\n\n" contatore_char;

						ricerca_substring_helper stringa_piu_corta lista k (index+1) 0 lista_risultati

					(* Sto per ricercare una substring che è già stata trovata come soluzione quindi salto direttamente all'indice successivo *)
					| GiaInRisultato -> Printf.printf "index: %d\n\n" index;
						ricerca_substring_helper stringa_piu_corta lista k (index+1) 0 lista_risultati
			
			else (false, "");;


(* ************************************************************************************ *)
(*  Questa è la funzione principale del programma e che l'utente deve richiamare per    *)
(*  avviare la ricerca di una substring in comune tra tutte le stringhe presenti nella  *)
(*  lista che viene passata in input alla funzione. Tale funzione va inoltre ad         *)
(*  effettuare dei controlli di sicurezza per verificare che:                           *)
(*    - la lunghezza (K) della substring da cercare sia >= 0                            *)
(*    - la lunghezza (K) della substring da cercare sia <= lunghezza stringa piu' corta *)
(*      all'interno della lista in input                                                *)
(*    - la lista in input contenga almeno un elemento                                   *)
(*                                                                                      *)
(*  La funzione restituisce in output una tupla (bool * string) in cui il valore bool   *)
(*  ci indica se la ricerca ha avuto successo o meno mentre il valore string ci ritorna *)
(*  "" se la ricerca non ha avuto successo, altrimenti il valore della substring in     *)
(*  comune tra tutte le stringhe.                                                       *)

let ricerca_substring lista k = 
 	if k <= 0 
 		then raise (KNonUtilizzabile "Usare un K > 0")
 	else if lista = []
 		then raise (ListaVuota "Usare una lista con almeno un elemento")
	else if List.length lista = 1
 		then (false, "")
 		else 
 			let lista_ordinata = ordina_lista lista in 
			let stringa_piu_corta = List.hd lista_ordinata in
			let coda = List.tl lista_ordinata in
				if k > String.length stringa_piu_corta
					then let eccezione = ("Usare un K <= lunghezza stringa piu' corta nella lista (in questo caso max K = "^(string_of_int (String.length stringa_piu_corta))^")") in
						raise (KNonUtilizzabile eccezione) 
				else if stringa_piu_corta = ""
					then (false, "")
 					else (
 						print_string("\n** Inizio ricerca substring **\n\n");
 						Printf.printf "Parola campione: %S\n" stringa_piu_corta;
 						print_string("Lista in cui cercare la substring: "); stampa_lista coda; print_newline();
 						Printf.printf "Lunghezza substring (K): %d\n\n\n" k;
 						ricerca_substring_helper stringa_piu_corta coda k 0 0 []
 					);;


(****************************************************************)
(******************** MIGLIORIE DA APPORTARE ********************)
(****************************************************************)
(* - Fai un disclaimer dicendo che le lettere accentate non     *)
(*   vengono stampate                                           *)


(****************************************************************)
(********************* MIGLIORIE APPORTATE **********************)
(****************************************************************)
(* - Termina prima la ricerca quando fai scansione di una       *)
(*   parola nella lista e index supera len_stringa-k            *)
(* - Lancia delle eccezioni se:                                 *)
(*  - k <= 0                                                    *)
(*  - k > len str più corta                                     *)
(*  - lista vuota                                               *)
(* - Trova substring maggiori di K chiedendolo all'utente       *)
(* - Creata funzione che mi genera una lista di stringhe rand   *)
(* - Se su stringa minima si hanno substring soluzione che si   *)
(*   ripetono, non vado a cercarli e passo al successivo index  *)



(****************************************************************)
(********************** ERRORI INDIVIDUATI **********************)
(****************************************************************)
(* - Lettere accentate non vengono visualizzate                 *)


(*********************************************************)
(********************* ERRORI RISOLTI ********************)
(*********************************************************)
(* - Se k = 0 il mio input viene comunque testato come 1 *)
(* - Se passo una lista vuota                            *)
(* - Se passo una lista con un solo elemento             *)



(************************************************************)
(********************** NOTE PROGETTO ***********************)
(************************************************************)
(* - Ho scelto di testare ad ogni turno le stringhe a       *)
(*   ripartire dall'indice iniziale perchè nei principali   *)
(*   alfabeti le parole di media sono lunghe 6 caratteri    *)
(*   quindi il tempo che impiego per ricontrollare quei     *)
(*   pochi char, almeno secondo me, risulta essere          *)
(*   inferiore rispetto al costo in memoria che si          *)
(*   creerebbe se dovessi salvarmi ogni volta gli indici    *)
(*   in cui devo testare il carattere successivo            *)
(*                                                          *)
(* - Un algoritmo in ampiezza probabilmente sarebbe stato   *)
(*   molto più performante in quanto prima mi andavo a      *)
(*   prendere la prima substring di lunghezza K, se         *)
(*   presente e se non presente già terminavo la ricerca.   *)
(*   Se invece esisteva, cercavo se questa substring        *)
(*   compariva nel primo elemento della lista di confronto. *)
(*   Se compariva allora lo ricercavo anche nel secondo     *)
(*   elemento della lista e così via. Se ad una certa non   *)
(*   trovavo una corrispondenza allora effettuavo           *)
(*   backtraking scorrendo di 1 l'index dal quale prendere  *)
(*   la substring sulla stringa più corta per poi ripetere  *)
(*   la ricerca come visto poco prima. Se ottenevo un       *)
(*   riscontro tra tutte le stringhe allora sarebbe stato   *)
(*   il risultato altrimenti, dopo aver testato tutte le    *)
(*   substring "utili" nella stringa più corta, ritorno     *)
(*   false come risultato.                                  *)