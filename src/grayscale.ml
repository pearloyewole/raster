open Core

let transform image =
  Image.map image ~f:(fun (r, g, b) ->
    let avg = (r + g + b) / 3 in
    avg, avg, avg)
;;

let%expect_test "transform" =
  (* This test uses existing files on the filesystem. *)
  let transformed_image =
    transform (Image.load_ppm ~filename:"images/beach_portrait.ppm")
  in
  let ref_image =
    Image.load_ppm ~filename:"images/reference-beach_portrait_gray.ppm"
  in
  (* CR leli: Move this to a helper function somewhere
  *)
  let difference =
    (* CR leli: Instead of counting number of differences, actually find the pixel coordinates that are different *)
    Image.foldi transformed_image ~init:[] ~f:(fun ~x ~y acc _image ->
      if
        not
          (Pixel.equal
             (Image.get transformed_image ~x ~y)
             (Image.get ref_image ~x ~y))
      then acc @ []
      else acc)
  in
  List.iter difference ~f:print_endline;
  [%expect {|[]|}]
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
