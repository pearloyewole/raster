open Core

let transform ~foreground ~background =
  (*r/2 for "improved" green screen*)
  let is_blue (r, g, b) = b > (r / 2) + g in
  Image.mapi foreground ~f:(fun ~x ~y pixel ->
    let bg_pixel = Image.get background ~x ~y in
    if is_blue pixel then bg_pixel else pixel)
;;

let%expect_test "transform" =
  (* This test uses existing files on the filesystem. *)
  let transformed_image =
    transform
      ~foreground:(Image.load_ppm ~filename:"images/oz_bluescreen.ppm")
      ~background:(Image.load_ppm ~filename:"images/meadow.ppm")
  in
  let ref_image =
    Image.load_ppm ~filename:"images/reference-oz_bluescreen_vfx.ppm"
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
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
