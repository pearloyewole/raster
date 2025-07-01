open Core

let transform image = image
(*
   let max_pixel_value = 765 in
   let pixel_sum pixel =
   Pixel.red pixel + Pixel.green pixel + Pixel.blue pixel
   in
   let pixel_error pixel = max_pixel_value - pixel_sum pixel in
   let new_image =
   Image.mapi image ~f:(fun ~x ~y pixel ->
   if pixel_sum pixel > max_pixel_value / 2
   then Image.set ~x ~y Pixel.zero
   else
   Image.set (~x + 1) ~y pixe)
   in
   new_image
   ;;
*)

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
