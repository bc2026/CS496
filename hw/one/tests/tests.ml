(* Helper function to compare int list *)
let compare_int_lists lst1 lst2 = lst1 = lst2

(* Helper function to compare (int * int) list *)
let compare_tuples_lists lst1 lst2 = lst1 = lst2

(* Test case for mirror_image *)
let test_mirror_image () =
  let input = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1] in
  let expected_output = [0; 4; 4; 5; 5; 3; 3; 2; 5; 3; 2; 5; 5; 3; 3; 1] in
  assert (compare_int_lists (mirror_image input) expected_output)


(* Test case for rotate_90_letter *)
let test_rotate_90_letter () =
  let input = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1] in
  let expected_output = [0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1] in
  assert (compare_int_lists (rotate_90_letter input) expected_output)

(* Test case for rotate_90_word *)
let test_rotate_90_word () =
  let input = [[0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]] in
  let expected_output = [[0; 3; 3; 4; 4; 2; 2; 5; 4; 2; 5; 4; 4; 2; 2; 1]] in
  assert (List.for_all2 compare_int_lists (rotate_90_word input) expected_output)

(* Test case for repeat *)
let test_repeat () =
  let input_n = 3 in
  let input_x = "hello" in
  let expected_output = ["hello"; "hello"; "hello"] in
  assert (compare_int_lists (repeat input_n input_x) expected_output)

(* Test case for pantograph *)
let test_pantograph () =
  let scale_factor = 2 in
  let program = [0;2;3;5;1] in
  let expected_output = [0; 2; 2; 3; 3; 5; 5; 1] in
  assert (compare_int_lists (pantograph scale_factor program) expected_output)

(* Test case for coverage *)
let test_coverage () =
  let starting_point = (0, 0) in
  let program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1] in
  let expected_output = [(0,0); (0, 0); (0, 1); (0, 2); (1, 2); (2, 2); (2, 1); (2, 0); (1, 0); (0, 0); (0,0)] in
  assert (compare_tuples_lists (coverage starting_point program) expected_output)

(* Test case for compress *)
let test_compress () =
  let program = [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1] in
  let expected_output = [(0, 1); (2, 2); (3, 2); (5, 2); (4, 1); (3, 1); (5, 1); (4, 1); (3, 2); (5, 2); (1, 1)] in
  assert (compare_tuples_lists (compress program) expected_output)

(* Test case for uncompress *)
let test_uncompress () =
  let compressed_program = [(0, 1); (2, 2); (3, 2); (5, 2); (4, 1); (3, 1); (5, 1); (4, 1); (3, 2); (5, 2); (1, 1)] in
  let expected_output = [0; 2; 2; 3; 3; 5; 5; 4; 3; 5; 4; 3; 3; 5; 5; 1] in
  assert (compare_int_lists (uncompress compressed_program) expected_output)

(* Test case for optimize *)
let test_optimize () =
  let input_program = [0; 1; 2; 0; 1; 3; 0; 1; 4; 0; 1; 5] in  (* Simplified example with redundant pen commands *)
  let expected_output = [0; 2; 0; 3; 0; 4; 0; 5] in  (* Optimized program with unnecessary pen commands removed *)
  assert (compare_int_lists (optimize input_program) expected_output)


let test() =
        test_mirror_image();
test_rotate_90_letter();
test_rotate_90_word();
test_repeat();
test_pantograph();
(* test_coverage(); *)
test_compress();
test_uncompress();
test_optimize();

