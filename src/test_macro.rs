macro_rules! test_macro {
    (($($x:tt)*)) => {test_macro! ($($x)*)};
    ([$x:expr]) => {$x};
    ($x:ident : $($y:tt)*) => {($x($(test_macro! ($y)),*))};
}

fn add(x: u32, y: u32) -> u32 {x + y}

fn main() {
    test_macro! (add: (add: [3] [2]) [1]);
}

// Taken from macros tutorial
/*
macro_rules! write_html {
    ($w:expr, ) => (());

    ($w:expr, $e:tt) => (write!($w, "{}", $e));

    ($w:expr, $tag:ident [ $($inner:tt)* ] $($rest:tt)*) => {{
        write!($w, "<{}>", stringify!($tag));
        write_html!($w, $($inner)*);
        write!($w, "</{}>", stringify!($tag));
        write_html!($w, $($rest)*);
    }};
}

fn main() {
    use std::fmt::Write;
    let mut out = String::new();

    write_html!(&mut out,
        html[
            head[title["Macros guide"]]
            body[h1["Macros are the best!"]]
        ]);

    assert_eq!(out,
        "<html><head><title>Macros guide</title></head>\
         <body><h1>Macros are the best!</h1></body></html>");
}
*/
