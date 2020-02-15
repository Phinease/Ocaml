#require "Graphics";;
open Graphics;;

let left = 0.;;
let right = 500.;;
let down = 0.;;
let up = 500.;;

open_graph " 100x100";;
resize_window (int_of_float right) (int_of_float up);;
Graphics.auto_synchronize false;;

let ball = 10.;;

let draw_ball x y =
  set_color black;
  Graphics.fill_circle (int_of_float x) (int_of_float y) (int_of_float ball);;

let vx = Random.float 1.;;
let vy = Random.float 3.;;
let new_position_x x vx = x +. vx;;
let new_position_y y vy = y +. vy;;

(*
let rec f x y =
  if (x > 1. && x < 300.) && (y > 1. && y < 500.)
  then
    begin
      clear_graph ();
      Graphics.synchronize ();
      draw_ball x y;
      f (new_position_x x vx) (new_position_y y vy)
    end
  else ();;
*)

let draw_paddle x =
  Graphics.fill_rect x 20 80 10;;

let position_paddle () =
  match Graphics.mouse_pos () with (x,y) ->
    draw_paddle x;
    x;;

let bounce_x x vx = if x <= 5. || x >= right-.5. then
    -.vx else vx;;

let bounce_y x y vy p =
  if y <= 30. then
    if x >= (float_of_int p) && x <= (float_of_int (p+80)) then
      -.vy else vy
  else
  if y >= up-.10. then
    -.vy else vy;;

let rec game x y vx vy =
  Graphics.clear_graph ();
  draw_ball x y;
  draw_paddle (position_paddle ());
  Graphics.synchronize ();
  if y < -10. then
    close_graph ()
  else
    let pos_p = position_paddle () in
    let new_vx = bounce_x x vx in
    let new_vy = bounce_y x y vy pos_p in
    let new_x = new_position_x x new_vx in
    let new_y = new_position_y y new_vy in
    game new_x new_y new_vx new_vy;;

game 250. 250. vx vy; read_key ();;
