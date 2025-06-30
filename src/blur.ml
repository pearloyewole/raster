open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius = 
  let new_image = Image.mapi image ~f:(
    fun ~x ~y->
      let pixel_area = (Image.slice image ~x_start: x ~x_end: radius ~y_start:y ~y_end:radius) in 
      let set_pixel =  Image.set pixel_area ~x:x ~y:y (Image.mean_pixel pixel_area) in 
      )
    in 
  new_image
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
          ~filename:(String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
