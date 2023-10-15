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



let rec single_check s1 s2 i j counter_char =
	let len_s1 = String.length s1 in
	let len_s2 = String.length s2 in
		if j < len_s2
			then (
				print_string("i: "); print_int(i); print_newline();
				print_string("j: "); print_int(j); print_newline();
				print_string("s1.[i]: "); print_char(s1.[i]); print_newline();
				print_string("s2.[j]: "); print_char(s2.[j]); print_newline();
				print_string("len_s1-1: "); print_int(len_s1-1); print_newline(); print_newline();

				if s1.[i] = s2.[j]
					then if (len_s1-1) = counter_char
						then true
						else single_check s1 s2 (i+1) (j+1) (counter_char+1)
					else single_check s1 s2 0 (j+1 -counter_char) 0
			)
			else false;;



exception NonPresente;;

let rec find_substring_check small_str str_list k index counter = 
	let small_str_len = String.length small_str in
		if (index+counter) < small_str_len (* Forse ci va index + counter *)
			then
				try
					let rec check_all list_to_test = match list_to_test with
						[] -> if (counter+1) < k
							then ( 
								print_string("** find_substring_check -- testate tutte le stringhe **"); print_newline();
								print_string("index: "); print_int(index); print_newline();
								print_string("counter: "); print_int(counter); print_newline(); print_newline();
								find_substring_check small_str str_list k index (counter+1)
							)
							else (true, String.sub small_str index (counter+1))  (* Fine ricerca a "k" *)
						| x::rest -> print_string("** find_substring_check **"); print_newline();
							print_string("index: "); print_int(index); print_newline();
							print_string("counter: "); print_int(counter); print_newline();
							print_string("x: "); print_string(x); print_newline();
							print_string("to find: "); print_string(String.sub small_str index (counter+1)); print_newline(); print_newline();

							if (single_check (String.sub small_str index (counter+1)) x 0 0 0)
								then check_all rest
								else raise NonPresente
					in check_all str_list
				with NonPresente -> print_string("** find_substring_check -- testata stringa **"); print_newline();
					print_string("index: "); print_int(index); print_newline();
					print_string("counter: "); print_int(counter); print_newline(); print_newline();

					find_substring_check small_str str_list k (index+1) 0
			else (false, "");;


let find_substring lst_of_string k = 
 	if k = 0 || lst_of_string = [] || List.length lst_of_string = 1
 		then (false, "")
 		else 
 			let sorted_list = sorter_lst lst_of_string in 
			let shortest_string = List.hd sorted_list in
			let tail = List.tl sorted_list in
				if shortest_string = ""
					then (false, "")
 					else (
 						print_string("\n** Inizio ricerca substring **\n\n");
 						print_string("Parola campione: "); print_endline(shortest_string);
 						print_string("Lista in cui cercare la substring: "); printer tail; print_newline();
 						print_string("K: "); print_int(k); print_string("\n\n\n");
 						find_substring_check shortest_string tail k 0 0
 					);;


find_substring ["ciao";"falciasao";"piae"] 3;;


(**********************************************************)
(***************** MIGLIORIE DA APPORTARE *****************)
(**********************************************************)
- Termina prima la ricerca quando fai scansione di una 
  parola e index supera len_stringa-k  

(**********************************************************)
(******************* ERRORI INDIVIDUATI *******************)
(**********************************************************)


(******************************************************)
(******************* ERRORI RISOLTI *******************)
(******************************************************)

- Se k = 0 il mio input viene comunque testato come 1
- Se passo una lista vuota
- Se passo una lista con un solo elemento
