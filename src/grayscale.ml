open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image =
  let new_image =
    Image.map image ~f:(fun (r, g, b) ->
      (r + g + b) / 3, (r + g + b) / 3, (r + g + b) / 3)
  in
  new_image
;;

(*let%expect_test "transform" =
  (* This test uses existing files on the filesystem. *)
  let transformed_image =
    transform (Image.load_ppm ~filename:"images/beach_portrait.ppm")
  in
  let pixels = Image.mean_pixel transformed_image in 
  print_s [%message pixels];
  [%expect
    {|
        (load_ppm ~filename:"images/reference-beach_portrait_gray.ppm")
        print_s [%message (pixels)]);]
    |}]
;;
*)

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
