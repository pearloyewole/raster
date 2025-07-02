open Core

let transform image =
  let grayscale_image = Grayscale.transform image in
  let max_pixel_value = Image.max_val grayscale_image in
  let new_image =
    Image.mapi grayscale_image ~f:(fun ~x ~y pixel ->
      let brightness = Pixel.red pixel in
      let error = max_pixel_value - brightness in
      let new_pixel =
        if brightness > max_pixel_value / 2
        then max_pixel_value, max_pixel_value, max_pixel_value
        else Pixel.zero
      in
      let right_addition = int_of_float (7. /. 16. *. float_of_int error) in
      let right_pixel_addition =
        right_addition, right_addition, right_addition
      in
      let d_left_addition = int_of_float (3. /. 16. *. float_of_int error) in
      let d_left_pixel_addition =
        d_left_addition, d_left_addition, d_left_addition
      in
      let down_addition = int_of_float (5. /. 16. *. float_of_int error) in
      let down_pixel_addition =
        down_addition, down_addition, down_addition
      in
      let d_right_addition =
        int_of_float (1. /. 16. *. float_of_int error)
      in
      let d_right_pixel_addition =
        d_right_addition, d_right_addition, d_right_addition
      in
      Image.set
        grayscale_image
        ~x:(x + 1)
        ~y
        (Pixel.( + ) pixel right_pixel_addition);
      Image.set
        grayscale_image
        ~x:(x - 1)
        ~y:(y - 1)
        (Pixel.( + ) pixel d_left_pixel_addition);
      Image.set
        grayscale_image
        ~x
        ~y:(y - 1)
        (Pixel.( + ) pixel down_pixel_addition);
      Image.set
        grayscale_image
        ~x:(x + 1)
        ~y:(y - 1)
        (Pixel.( + ) pixel d_right_pixel_addition);
      new_pixel)
  in
  new_image
;;

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
