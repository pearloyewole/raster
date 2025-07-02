open Core

let transform image =
  let grayscale_image = Grayscale.transform image in
  let max_pixel_value = Image.max_val grayscale_image in 
  let new_image = Image.mapi grayscale_image ~f:(fun ~x ~y pixel ->
    let brightness = Pixel.red pixel in
    let error = max_pixel_value - brightness

    if brightness > max_pixel_value/2  
        then Image.set grayscale_image ~x ~y (max_pixel_value,max_pixel_value,max_pixel_value)
    else Image.set grayscale_image ~x ~y Pixel.zero 

    let right_pixel_addition = ((7/.16) *. error, (7/.16) *. error, (7/.16) *. error) in 
    let left_pixel_addition = ((3/.16) *. error, (3/.16) *. error, (3/.16) *. error) in 
    let down_pixel_addition = ((5/.16) *. error, (3/.16)*. error, (3/.16)*. error)in 
    let diagonal_pixel_addition = ((1/.16) *. error, (1/.16) *. error, (1/.16) *. error) in 

    let current_pixel ~x ~y = Image.get grayscale_image ~x ~y 

    Image.set grayscale_image ~x+1 ~y Pixel.(+) current_pixel right_pixel_addition
    Image.set grayscale_image ~x+1 ~y Pixel.(+) current_pixel left_pixel_addition
    Image.set grayscale_image ~x+1 ~y Pixel.(+) current_pixel down_pixel_addition
    Image.set grayscale_image ~x+1 ~y Pixel.(+) current_pixel diagonal_pixel_additio

  new_image

  (*change new image vs grayscale image*)

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
