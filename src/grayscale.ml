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

let%expect_test "transform" =
  (* This test uses existing files on the filesystem. *)
  let transformed_image =
    transform (Image.load_ppm ~filename:"images/beach_portrait.ppm")
  in
  let ref_image =
    Image.load_ppm ~filename:"images/reference-beach_portrait_gray.ppm"
  in
  let difference =
    Image.foldi transformed_image ~init:0 ~f:(fun ~x ~y acc image ->
      if Image.get transformed_image ~x ~y != Image.get ref_image ~x ~y
      then acc + 1)
  in
  print_s [%message];
  [%expect {| (difference) (0)|}]
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
