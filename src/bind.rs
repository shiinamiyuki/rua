// struct RefWrapper<'a, T>(&'a T);

// pub trait IsReference {
//     const VALUE:bool;
// }
// impl IsReference for i32{
//     const VALUE:bool = false;
// }
// impl IsReference for f32{
//     const VALUE:bool = false;
// }
// impl <'a, T> IsReference for RefWrapper<'a, T> {
//     const VALUE:bool = true;
// }