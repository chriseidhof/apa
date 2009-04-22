var a = x || y;

// // {a :: [Int]}
// b = 100;
// // {a :: [Int], b :: [Int]}
// c = false;
// // {a :: [Int], b :: [Int], c :: [Boolean]}
// a = "test";
// // Could be a warning: do you really want this type conversion?
// // {a :: [String], b :: [Int], c :: [Boolean]}
// if(a == "hi"){
//   b = "test";
// }
// 
// // {a :: [String], b :: [Int, String], c :: [Boolean]}
// // if a type > k then the type = Top
// //
// if(b == 5) // Warning: b might be a String
