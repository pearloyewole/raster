open Core

let transform ~foreground ~background =
  (*r/2 for "improved" green screen*)
  let is_blue (r, g, b) = b > (r / 2) + g in
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    let bg_pixel = Image.get background ~x ~y in
    if is_blue pixel then bg_pixel else pixel)
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
