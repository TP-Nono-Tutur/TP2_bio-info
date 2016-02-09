let print_window_list window_list =
  List.iter Window.print window_list
  		    
let window length shift file = 
  let fasta = Fasta.of_file file in
  let seq_window_list = Window.extract_from_fasta length shift fasta
  and p (seq_name, window_list) =
    print_endline seq_name;
    print_window_list window_list
  in List.iter p seq_window_list

let mapper_windows_kmers length_win shift_win length_kmer step_kmer file1 file2 =
  let read = Fasta.of_file file1
  in let k_mers_read = K_mer.list length_kmer read
     and genome = Fasta.of_file file2 in
     let window_list = Fasta.extract_windows length_win shift_win genome in
     let p window =
       let k_mers_window = K_mer.list_of_window length_kmer window in
       let common_k_mers = K_mer.common k_mers_window k_mers_read in
       let ratio = Math.((List.length common_k_mers) // (List.length k_mers_window))
       in if ratio >= step_kmer
	  then Window.print window
	  else ()
     in List.iter p window_list
	       
	       
let main =
  let commande = Sys.argv.(1) in
  match commande with
  | "windows" ->
     let length = int_of_string(Sys.argv.(2)) 
     and shift = int_of_string(Sys.argv.(3))
     and file = Sys.argv.(4)
     in window length shift file
  | "mapper-windows-kmers" ->
     let length_win = int_of_string(Sys.argv.(2))
     and shift_win = int_of_string(Sys.argv.(3))
     and length_kmer = int_of_string(Sys.argv.(4))
     and step_kmer = int_of_string(Sys.argv.(5))
     and file1 = Sys.argv.(6)
     and file2 = Sys.argv.(7)
     in mapper_windows_k_mers length_win shift_win length_kmer step_kmer file1 file2
	       
