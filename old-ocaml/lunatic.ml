

let main () =
  GtkMain.Main.init ();

  let window = GWindow.window ~title: "Lunatic" 
    ~width: 640 ~height: 480 () in
    ignore (window#connect#destroy ~callback: GMain.Main.quit);
    
    let sw = GBin.scrolled_window ~packing: (window#add) () in
    let b = GText.buffer () in
    let tv = GText.view ~buffer: b ~packing: (sw#add) () in


      window#show (); 
      GMain.Main.main ();
;;

let _ =
  Printexc.print main ()
;;
