(* reading one line from the list *)
let read_line (input: in_channel) : string option =
	match input_line input with
	| l -> Some l
	| exception End_of_file -> None;;

let char_list = ['a'; 'b'; 'c'; 'd'; 'e';'f';
				'g'; 'h'; 'i'; 'j'; 'k';'l';
				'm'; 'n'; 'o'; 'p'; 'q';'r';
				's'; 't'; 'u'; 'v'; 'w';'x';
				'y';'z'];;

let rec isPangram (inc: string) (l: char list) : bool = 
	match l with 
	| [] -> true
	| x::xs -> 
		if (  (String.contains inc x))
		then (isPangram inc xs)
		else false  
		;;
	

(* reading multiple lines from the list *)
let rec helper_read_line (input: in_channel) (result : string)  : string = 	
	let l = read_line input in (* returns string option *)
	match l with
	| Some s -> helper_read_line input (result^string_of_bool (isPangram s char_list )^"\n") 
	| None -> String.sub result 0 ((String.length result)-1);;

let rec helper_read_line2 (input: in_channel) (result : string)  : string = 	
	let l = read_line input in (* returns string option *)
	match l with
	| Some s -> helper_read_line2 input (result^string_of_bool (isPangram s char_list )^"\n") 
	| None -> result;;




let pangram (a: string*string) : unit =
 	(* let a = ("input3.txt", "o.txt") in *)
 	let (arg1, arg2) = a  in
	let input=open_in arg1  in
	let output=open_out arg2  in
	let lines = helper_read_line2 input ""  in
	let _ = Printf.fprintf output "%s" lines  in
	let _ = close_in input in
	let _ = close_out output in ()