open Core

let transform image ~radius =
  let width = Image.width image in
  let height = Image.height image in
  let new_image =
    Image.mapi image ~f:(fun ~x ~y _pixel ->
      let x_start = max 0 (x - radius) in
      let x_end = min (width - 1) (x + radius) in
      let y_start = max 0 (y - radius) in
      let y_end = min (height - 1) (y + radius) in
      let region = Image.slice image ~x_start ~x_end ~y_start ~y_end in
      Image.mean_pixel region)
  in
  new_image
;;

let%expect_test "transform" =
  (* This test uses existing files on the filesystem. *)
  let transformed_image =
    transform
      (Image.load_ppm ~filename:"images/beach_portrait.ppm")
      ~radius:3
  in
  let ref_image =
    Image.load_ppm ~filename:"images/reference-beach_portrait_blur.ppm"
  in
  let difference =
    Image.foldi transformed_image ~init:0 ~f:(fun ~x ~y acc _image ->
      if
        not
          (Pixel.equal
             (Image.get transformed_image ~x ~y)
             (Image.get ref_image ~x ~y))
      then acc + 1
      else acc)
  in
  print_endline (string_of_int difference);
  [%expect {|0|}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
