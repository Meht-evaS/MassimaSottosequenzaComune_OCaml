
# Problema della massima sottosequenza comune: ricerca in profondità

Progetto d'esame, in linguaggio OCaml, per la materia Artificial Intelligent Systems: mod. Intelligent Application Development

##  :pencil: Sommario
* [Obiettivo](#obiettivo)
* [Problema](#problema)
* [Ambiente di lavoro](#ambiente-di-lavoro)
* [Setup](#setup)
* [Funzionamento codice](#funzionamento-codice)


## :dart: Obiettivo <a name="obiettivo"/>
L'obiettivo di questo progetto è la creazione di un codice, in linguaggio [OCaml](https://ocaml.org/), che permetta di risolvere il problema intitolato "_Massima sottosequenza comune: ricerca in profondità_".

## :question: Problema <a name="problema"/>
Si consideri un insieme finito `S` di stringhe ed un intero `K`. Determinare, se esiste, una stringa `x` di lunghezza maggiore o uguale a `K` che sia sottosequenza di ogni stringa `s∈S` . Si risolva il problema utilizzando una ricerca in profondità.

## :computer: Ambiente di lavoro <a name="ambiente-di-lavoro"/>
Il progetto è stato realizzato su Windows dove sono andato ad emulare l'ambiente Unix tramite il software [Cygwin](https://www.cygwin.com/). Per installarlo, insieme ad OCaml, sono andato a sfruttare la procedura di installazione grafica disponibile alla seguente [pagina GitHub](https://fdopen.github.io/opam-repository-mingw/installation/).

## :gear: Setup <a name="setup"/>
Per eseguire questo progetto puoi scaricare OCaml dal seguente [sito web](https://fdopen.github.io/opam-repository-mingw/installation/) (lo stesso fornito al punto precedente) oppure utilizzare uno dei seguenti compiler online, [onlinegdb OCaml](https://www.onlinegdb.com/online_ocaml_compiler) e [TryOCaml](https://try.ocamlpro.com/), dove puoi usare questo linguaggio in modalità interattiva senza scaricare nulla. Se preferisci usarlo localmente (tramite Cygwin), dovrai:

1. Aprire l'applicazione Cygwin
2. Andare al path del progetto: `cd C:\\path\\to\\project`
3. Digitare: `ocaml`
4. Importare le funzioni del progetto così da poterle richiamare; digitare:
```ocaml
#use "massima_sottosequenza_comune.ml";;
```

## :man_technologist: Funzionamento codice <a name="funzionamento-codice"/>
La spiegazione del codice è presente all'interno del file ["Relazione_progetto.pdf"](Relazione_progetto.pdf), dove si possono vedere anche alcuni esempi di test del programma.

Le due funzioni principali sono:
- **ricerca_substring lista k**: avvia la ricerca di una substring in comune tra tutte le stringhe presenti nella lista che viene passata in input alla funzione. Tale funzione va inoltre ad effettuare dei controlli di sicurezza per verificare che:
	- la lunghezza (`K`) della substring da cercare sia `>= 0`
	- la lunghezza (`K`) della substring da cercare sia `<=` lunghezza stringa più corta all'interno della lista in input
	- la lista in input contenga almeno un elemento
	La funzione restituisce in output una tupla `(bool * string)` in cui il valore bool ci indica se la ricerca ha avuto successo o meno mentre il valore string ci ritorna `""` se la ricerca non ha avuto successo, altrimenti il valore della substring in comune tra tutte le stringhe.
- **genera_lista ()**: Genera una lista di stringhe, che viene poi salvata nel file `"lista.txt"`, in base ad alcuni parametri che vengono chiesti interattivamente all'utente quali:
	- numero di stringhe da generare                                              
	- lunghezza minima stringhe da generare                                       
	- lunghezza massima stringhe da generare                                      
	- valore della substring che avranno in comune tutte le stringhe generate